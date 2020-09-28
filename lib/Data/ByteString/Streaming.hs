-- | A simple module reexport to aid back-compatibility. Please use the new
-- module.
module Data.ByteString.Streaming
  {-# DEPRECATED "Use Streaming.ByteString instead." #-}
  ( module Streaming.ByteString ) where

import Streaming.ByteString
