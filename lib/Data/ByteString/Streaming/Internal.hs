-- | A simple module reexport to aid back-compatibility. Please use the new
-- module.
module Data.ByteString.Streaming.Internal
  {-# DEPRECATED "Use Streaming.ByteString.Internal instead." #-}
  ( module Streaming.ByteString.Internal ) where

import Streaming.ByteString.Internal
