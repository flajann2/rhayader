{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

module Engine.NEAT.Izhikevich
  ( -- * State and parameters
    NeuronState (..)
  , IzhikevichParams (..)
  , stepIzh
    -- * Named presets
  , IzhPreset (..)
  , preset
  , allPresets
  , canonicalPresets
  , presetAliasOf
  ) where

import Data.Kind          (Type)
import Data.Proxy         (Proxy (..))
import GHC.Generics
import GHC.OverloadedLabels (IsLabel (..))
import GHC.TypeLits

-- | Izhikevich neuron state: membrane potential and recovery variable.
-- `v` and `u` are the two state variables of the model — everything else (a, b, c, d) just shapes how they interact.
--
-- **v — membrane potential**
-- This is the actual voltage across the neuron's membrane, in millivolts. It's the variable that spikes. The equation
--
-- ```
-- v' = 0.04v² + 5v + 140 - u + I
-- ```
--
-- is a fitted quadratic (Izhikevich reverse-engineered the coefficients 0.04, 5, 140 to match real cortical neuron dynamics using far less computation than Hodgkin-Huxley). The quadratic term is what gives you the fast upstroke of a spike — once `v` crosses a certain point the `0.04v²` term dominates and `v` shoots up toward +30 mV essentially in finite time, which is why the model needs the explicit `if v ≥ 30` reset rather than a smooth repolarization phase like Hodgkin-Huxley has. `I` is your injected/synaptic input current — the thing you'd drive with external stimulation.
--
-- **u — membrane recovery variable**
-- This isn't a real physical quantity in the way `v` is — it's a lumped abstraction standing in for the slow ionic currents that oppose depolarization, mainly K⁺ activation and Na⁺ inactivation. Its job is negative feedback: it grows in response to spiking (via the `+d` reset) and subtracts from `v`'s growth (the `-u` term), which is what eventually pulls the neuron back down and creates a refractory-like period.
--
-- The `a(bv - u)` dynamics mean `u` chases `bv` at rate `a` — so `u` is always lagging behind a scaled version of `v`, which is exactly the "slow feedback trailing a fast variable" structure that makes delay embedding work.
--
-- **Why this matters for your delay embedding**
--
-- Your choice of `(v(t), u(t), v(t-τ))` is interesting because `v` is fast/spiky and `u` is slow/smooth — they already form a natural fast-slow pair, similar to how the Lorenz system has a fast variable and slower ones. Using `v(t-τ)` as a stand-in for a third independent dimension works because `v` alone doesn't have enough memory in it to unfold the attractor (the spike reset destroys information), but `u` partially compensates since it integrates spike history. You'll likely see the reset rule show up as a visible discontinuity or fold in the embedded trajectory — worth watching for when `v` hits 30 and jumps to `c`, since that's a non-smooth event that a pure delay-embedding of a smooth ODE wouldn't have.
data NeuronState a = NeuronState
  { nV :: !a  -- ^ membrane potential (mV)
  , nU :: !a  -- ^ recovery variable
  } deriving (Show, Eq)

data IzhikevichParams a = IzhikevichParams
  { za :: !a  -- ^ recovery time scale
  , zb :: !a  -- ^ sensitivity of u to subthreshold fluctuations of v
  , zc :: !a  -- ^ post-spike reset value of v (mV)
  , zd :: !a  -- ^ post-spike bump to u
  } deriving (Show, Eq)

-- | One 1ms integration step: two 0.5ms half-steps for v (stability near the
--   v^2 spike-threshold nonlinearity), one full step for u, then reset on spike.
-- The Izhikevich model boils a spiking neuron down to two coupled ODEs plus a reset rule:
--
-- ```
-- v' = 0.04v² + 5v + 140 - u + I
-- u' = a(bv - u)
-- if v ≥ 30: v ← c, u ← u + d
-- ```
--
-- Here's what each parameter does:
--
-- **a — recovery time scale**
-- Controls how fast `u` (the recovery variable) responds to changes in `v`. Small `a` (like 0.02) means slow recovery, so the neuron can sustain longer depolarizations or exhibit bursting. Larger `a` (like 0.1) means fast recovery, which tends to produce fast-spiking behavior.
--
-- **b — sensitivity of recovery to subthreshold voltage**
-- Controls how strongly `u` is coupled to `v` below threshold. Larger `b` makes `u` track `v` more tightly, which can produce low-threshold oscillations or resonator behavior. If `b < a`, you tend to get regular spiking/integrator dynamics; if `b > a`, you get resonator dynamics (the neuron responds preferentially to a certain input frequency).
--
-- **c — reset voltage**
-- After a spike, `v` snaps back to `c` (typically around -65 mV). More negative `c` gives a bigger after-spike hyperpolarization — this is one of the knobs that shapes bursting vs. tonic firing.
--
-- **d — after-spike jump in recovery**
-- After a spike, `u` gets bumped up by `d`. This is the key bursting/adaptation control — larger `d` means the recovery variable accumulates more with each spike, suppressing subsequent spikes and producing chattering or bursting patterns. Small `d` gives more regular tonic spiking.
--
-- For your phase portrait visualizer, sweeping `d` while
-- holding the rest fixed is a nice way to show the qualitative transition
-- from tonic to bursting in the `(v, u, v(t-τ))` embedding — the bursts
-- show up as the trajectory looping through a slower,
-- larger-amplitude excursion before returning to the fast spike manifold.
stepIzh :: RealFloat a => a -> IzhikevichParams a -> a -> NeuronState a -> NeuronState a
stepIzh dt (IzhikevichParams a b c d) iCur (NeuronState v u) =
  let half = dt / 2
      v1 = v  + half * (0.04 * v  * v  + 5 * v  + 140 - u + iCur)
      v2 = v1 + half * (0.04 * v1 * v1 + 5 * v1 + 140 - u + iCur)
      u' = b * v2 + (u - b * v2) * exp (-a * dt)  -- exact linear integration, dt-independent
  in if v2 >= 30
     then NeuronState c (u' + d)   -- d applied once, per spike event, no dt scaling needed
     else NeuronState v2 u'

------------------------------------------------------------------------------
-- Named presets
------------------------------------------------------------------------------

-- | Named Izhikevich neuron types (Izhikevich 2003, 2004).
--
-- Constructors are the "symbols": checked at compile time, enumerable via
-- 'Enum'/'Bounded', printable via 'Show', parseable from config via 'Read'.
-- With @OverloadedLabels@ you may also write @#FastSpiking@ wherever an
-- 'IzhikevichParams' or 'IzhPreset' is expected.
data IzhPreset
  -- Table 1: canonical cortical/thalamic types
  = RegularSpiking
  | IntrinsicallyBursting
  | Chattering
  | FastSpiking
  | LowThresholdSpiking
  | Thalamocortical
  | Resonator
  | ThalamicReticular
  -- Table 2: other named dynamical classes
  | Class1Excitable
  | Class2Excitable
  | SpikeLatency
  | SubthresholdOscillations
  | ResonatorAlt
  | Integrator
  | ReboundSpike
  | ReboundBurst
  | ThresholdVariability
  | Bistability
  | DepolarizingAfterpotential
  | Accommodation
  | InhibitionInducedSpiking
  | InhibitionInducedBursting
  | MixedMode
  deriving (Show, Read, Eq, Ord, Enum, Bounded, Generic)

-- | Parameter table. The 'case' is exhaustiveness-checked, so adding a
--   constructor without a row is a compiler warning.
preset :: Fractional a => IzhPreset -> IzhikevichParams a
preset = \case
  --                            a         b       c      d
  RegularSpiking             -> p   0.02     0.2   (-65)    8
  IntrinsicallyBursting      -> p   0.02     0.2   (-55)    4
  Chattering                 -> p   0.02     0.2   (-50)    2
  FastSpiking                -> p   0.1      0.2   (-65)    2
  LowThresholdSpiking        -> p   0.02     0.25  (-65)    2
  Thalamocortical            -> p   0.02     0.25  (-65)    0.05
  Resonator                  -> p   0.1      0.26  (-65)    2
  ThalamicReticular          -> p   0.1      0.25  (-65)    2.05
  Class1Excitable            -> p   0.02   (-0.1)  (-55)    6
  Class2Excitable            -> p   0.2      0.26  (-65)    0
  SpikeLatency               -> p   0.02     0.2   (-65)    6
  SubthresholdOscillations   -> p   0.05     0.26  (-60)    0
  ResonatorAlt               -> p   0.1      0.26  (-60)  (-1)
  Integrator                 -> p   0.02   (-0.1)  (-55)    6
  ReboundSpike               -> p   0.03     0.25  (-60)    4
  ReboundBurst               -> p   0.03     0.25  (-52)    0
  ThresholdVariability       -> p   0.03     0.25  (-60)    4
  Bistability                -> p   1.0      1.5   (-60)    0
  DepolarizingAfterpotential -> p   1.0      0.2   (-60)  (-21)
  Accommodation              -> p   0.02     1.0   (-55)    4
  InhibitionInducedSpiking   -> p (-0.02)  (-1.0)  (-60)    8
  InhibitionInducedBursting  -> p (-0.026) (-1.0)  (-45)    0
  MixedMode                  -> p   0.02     0.2   (-55)    4
  where p = IzhikevichParams

-- | Every preset, in declaration order.
allPresets :: [IzhPreset]
allPresets = [minBound .. maxBound]

-- | Presets whose (a, b, c, d) duplicate an earlier preset. In Izhikevich's
--   2004 figure these behaviours differ only in the input current, not the
--   parameters.
presetAliasOf :: IzhPreset -> Maybe IzhPreset
presetAliasOf = \case
  MixedMode            -> Just IntrinsicallyBursting
  Integrator           -> Just Class1Excitable
  ThresholdVariability -> Just ReboundSpike
  _                    -> Nothing

-- | Presets with distinct parameter sets. Use this when sampling neuron types
--   for NEAT mutation, so aliased dynamics aren't double-weighted.
canonicalPresets :: [IzhPreset]
canonicalPresets = [ x | x <- allPresets, presetAliasOf x == Nothing ]

------------------------------------------------------------------------------
-- #Label syntax, derived from the constructors via Generics
------------------------------------------------------------------------------

-- | Constructor names of a Generic representation, in declaration order.
type family ConNames (f :: Type -> Type) :: [Symbol] where
  ConNames (M1 D _ f)                   = ConNames f
  ConNames (f :+: g)                    = Append (ConNames f) (ConNames g)
  ConNames (M1 C ('MetaCons n _ _) _)   = '[n]

type family Append (xs :: [Symbol]) (ys :: [Symbol]) :: [Symbol] where
  Append '[]       ys = ys
  Append (x ': xs) ys = x ': Append xs ys

-- | Position of a name in the list, or a readable compile error.
type family IndexOf (s :: Symbol) (xs :: [Symbol]) (i :: Nat) :: Nat where
  IndexOf s (s ': _)  i = i
  IndexOf s (_ ': xs) i = IndexOf s xs (i + 1)
  IndexOf s '[]       _ = TypeError
    ( 'Text "Unknown Izhikevich preset: #" ':<>: 'Text s
      ':$$: 'Text "See the IzhPreset constructors in Engine.NEAT.Izhikevich" )

type PresetIndex s = IndexOf s (ConNames (Rep IzhPreset)) 0

-- | @#FastSpiking :: IzhPreset@
instance KnownNat (PresetIndex s) => IsLabel s IzhPreset where
  fromLabel = toEnum (fromIntegral (natVal (Proxy @(PresetIndex s))))

-- | @#FastSpiking :: IzhikevichParams Double@
instance (KnownNat (PresetIndex s), Fractional a) => IsLabel s (IzhikevichParams a) where
  fromLabel = preset (fromLabel @s)
