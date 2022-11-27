module Example (exampleFunction) where

import Plutarch.Integer (PInteger)
import Plutarch.Monadic qualified as P
import Plutarch.Prelude
import Eval

exampleFunction :: Term s PInteger
exampleFunction = 1 + 3
