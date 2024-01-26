module BSession.Prefix where

import BSession.Syntax

stripPrefix :: Session n -> Session n -> IO (Session n)
stripPrefix _full _pfx = undefined
