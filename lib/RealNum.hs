{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module RealNum where

import GHC.Float (float2Double, double2Float)
import GHC.Generics (Generic)
import Data.Serialize ( Serialize )

type RealNum a = (RealFloat a, Serialize a)
