module Example (exampleFunction) where

import Eval
import Plutarch.Integer (PInteger)
import Plutarch.Monadic qualified as P
import Plutarch.Prelude

exampleFunction :: Term s PInteger
exampleFunction = 1 + 3
