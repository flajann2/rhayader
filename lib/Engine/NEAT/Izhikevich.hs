module Engine.NEAT.Izhikevich where

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
data NeuronState a = NeuronState { nV :: !a
                                  , nU :: !a
                                  }

data IzhikevichParams a = IzhikevichParams { za -- ^ recovery time scale
                                            , zb -- ^ sensitivity of u to subth
                                            , zc -- ^ post-spike reset value of
                                            , zd -- ^ post-spike bump to u
                                              :: !a
                                            }

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
-- **The classic combos** (from Izhikevich's 2003 paper) are worth having on hand since they map cleanly onto neuron classes:
-- - Regular spiking (RS): a=0.02, b=0.2, c=-65, d=8
-- - Intrinsically bursting (IB): a=0.02, b=0.2, c=-55, d=4
-- - Chattering (CH): a=0.02, b=0.2, c=-50, d=2
-- - Fast spiking (FS): a=0.1, b=0.2, c=-65, d=2
-- - Low-threshold spiking (LTS): a=0.02, b=0.25, c=-65, d=2
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

-- Table 1: Canonical cortical/thalamic types

mkRegularSpiking :: Fractional a => IzhikevichParams a
mkRegularSpiking = IzhikevichParams { za = 0.02, zb = 0.2, zc = -65, zd = 8 }

mkIntrinsicallyBursting :: Fractional a => IzhikevichParams a
mkIntrinsicallyBursting = IzhikevichParams { za = 0.02, zb = 0.2, zc = -55, zd = 4 }

mkChattering :: Fractional a => IzhikevichParams a
mkChattering = IzhikevichParams { za = 0.02, zb = 0.2, zc = -50, zd = 2 }

mkFastSpiking :: Fractional a => IzhikevichParams a
mkFastSpiking = IzhikevichParams { za = 0.1, zb = 0.2, zc = -65, zd = 2 }

mkLowThresholdSpiking :: Fractional a => IzhikevichParams a
mkLowThresholdSpiking = IzhikevichParams { za = 0.02, zb = 0.25, zc = -65, zd = 2 }

mkThalamocortical :: Fractional a => IzhikevichParams a
mkThalamocortical = IzhikevichParams { za = 0.02, zb = 0.25, zc = -65, zd = 0.05 }

mkResonator :: Fractional a => IzhikevichParams a
mkResonator = IzhikevichParams { za = 0.1, zb = 0.26, zc = -65, zd = 2 }

mkThalamicReticular :: Fractional a => IzhikevichParams a
mkThalamicReticular = IzhikevichParams { za = 0.1, zb = 0.25, zc = -65, zd = 2.05 }

-- Table 2: Other named dynamical classes

mkClass1Excitable :: Fractional a => IzhikevichParams a
mkClass1Excitable = IzhikevichParams { za = 0.02, zb = -0.1, zc = -55, zd = 6 }

mkClass2Excitable :: Fractional a => IzhikevichParams a
mkClass2Excitable = IzhikevichParams { za = 0.2, zb = 0.26, zc = -65, zd = 0 }

mkSpikeLatency :: Fractional a => IzhikevichParams a
mkSpikeLatency = IzhikevichParams { za = 0.02, zb = 0.2, zc = -65, zd = 6 }

mkSubthresholdOscillations :: Fractional a => IzhikevichParams a
mkSubthresholdOscillations = IzhikevichParams { za = 0.05, zb = 0.26, zc = -60, zd = 0 }

mkResonatorAlt :: Fractional a => IzhikevichParams a
mkResonatorAlt = IzhikevichParams { za = 0.1, zb = 0.26, zc = -60, zd = -1 }

mkIntegrator :: Fractional a => IzhikevichParams a
mkIntegrator = IzhikevichParams { za = 0.02, zb = -0.1, zc = -55, zd = 6 }

mkReboundSpike :: Fractional a => IzhikevichParams a
mkReboundSpike = IzhikevichParams { za = 0.03, zb = 0.25, zc = -60, zd = 4 }

mkReboundBurst :: Fractional a => IzhikevichParams a
mkReboundBurst = IzhikevichParams { za = 0.03, zb = 0.25, zc = -52, zd = 0 }

mkThresholdVariability :: Fractional a => IzhikevichParams a
mkThresholdVariability = IzhikevichParams { za = 0.03, zb = 0.25, zc = -60, zd = 4 }

mkBistability :: Fractional a => IzhikevichParams a
mkBistability = IzhikevichParams { za = 1.0, zb = 1.5, zc = -60, zd = 0 }

mkDepolarizingAfterpotential :: Fractional a => IzhikevichParams a
mkDepolarizingAfterpotential = IzhikevichParams { za = 1.0, zb = 0.2, zc = -60, zd = -21 }

mkAccommodation :: Fractional a => IzhikevichParams a
mkAccommodation = IzhikevichParams { za = 0.02, zb = 1.0, zc = -55, zd = 4 }

mkInhibitionInducedSpiking :: Fractional a => IzhikevichParams a
mkInhibitionInducedSpiking = IzhikevichParams { za = -0.02, zb = -1.0, zc = -60, zd = 8 }

mkInhibitionInducedBursting :: Fractional a => IzhikevichParams a
mkInhibitionInducedBursting = IzhikevichParams { za = -0.026, zb = -1.0, zc = -45, zd = 0 }

mkMixedMode :: Fractional a => IzhikevichParams a
mkMixedMode = IzhikevichParams { za = 0.02, zb = 0.2, zc = -55, zd = 4 }
