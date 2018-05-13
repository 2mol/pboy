module Options () where

import           Control.Applicative ((<|>))
import           Data.Monoid         ((<>))
import           Options.Applicative

data Options = Options

data Parameters = Parameters
    { action :: Action
    }

data Action = Setup | MoveFile
