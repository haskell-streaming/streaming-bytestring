-- | A simple module reexport to aid back-compatibility. Please use the new
-- module.
module Data.ByteString.Streaming.Char8
  {-# DEPRECATED "Use Streaming.ByteString.Char8 instead." #-}
  ( module Streaming.ByteString.Char8 ) where

import Streaming.ByteString.Char8
