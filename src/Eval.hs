module Eval (evalT, evalWithArgsT) where

import Data.Bifunctor (first)
import Data.Default (def)
import Data.Text (Text, pack)
import Data.Text.Encoding qualified as TE
import Plutarch (ClosedTerm, compile)
import Plutarch.Evaluate (evalScript)
import Plutarch.Script (Script (..))
import PlutusLedgerApi.V1 (Data, ExBudget)
import UntypedPlutusCore (DeBruijn, DefaultFun, DefaultUni, Program)

import PlutusCore qualified as PLC
import PlutusCore.Data qualified as PLC
import PlutusCore.Evaluation.Machine.ExBudget qualified as PLC
import UntypedPlutusCore qualified as UPLC
import UntypedPlutusCore.Check.Scope qualified as UPLC
import UntypedPlutusCore.Evaluation.Machine.Cek qualified as UPLC

import PlutusCore.MkPlc qualified as PLC

applyArguments :: Script -> [Data] -> Script
applyArguments (Script (UPLC.Program a v t)) args =
  let termArgs = fmap (PLC.mkConstant ()) args
      applied = PLC.mkIterApp () t termArgs
   in Script (UPLC.Program a v applied)

evalT :: ClosedTerm a -> Either Text (Script, ExBudget, [Text])
evalT x = evalWithArgsT x []

evalWithArgsT :: ClosedTerm a -> [Data] -> Either Text (Script, ExBudget, [Text])
evalWithArgsT x args = do
  cmp <- compile def x
  let (escr, budg, trc) = evalScript $ applyArguments cmp args
  scr <- first (pack . show) escr
  pure (scr, budg, trc)

evalWithArgsT' :: ClosedTerm a -> [Data] -> Either Text (Program DeBruijn DefaultUni DefaultFun (), ExBudget, [Text])
evalWithArgsT' x args =
  (\(res, budg, trcs) -> (unScript res, budg, trcs))
    <$> evalWithArgsT x args
