module Faro.Types where

import Data.Text (Text)
import Prettyprinter (Doc)

data Warning = Warning
  { name :: Text,
    subject :: Doc (),
    details :: Doc (),
    explanation :: Doc ()
  }
