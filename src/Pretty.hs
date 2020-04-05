module Pretty
  ( Pretty(..)
  , evalDocState
  , module Type
  )
where

import           Pretty.Internal.Basic          ( Pretty(..)
                                                , evalDocState
                                                )
import qualified Pretty.Internal.Printers.Type as Type
                                                ( )
