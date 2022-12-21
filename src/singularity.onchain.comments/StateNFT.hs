module StateNFT (
  pstateNFTPolicy,
  pstateNFTPolicyUntyped,
) where

{-
    This module implements a one-shot policy for minting NFTs as described in
    the spec for bonded/unbonded pools.

    The policy is parametrized by a UTXO and a token name, the latter hardcoded
    in the `Settings` module.
-}

import Plutarch.Api.V1 (
  PScriptContext,
  PTxInInfo,
  PTxOutRef,
  PValue,
 )
import Plutarch.Monadic qualified as P
import Plutarch.Unsafe (punsafeCoerce)
import Plutus.V1.Ledger.Api (TokenName)
import Utils (
  getCs,
  oneOf,
  ptryFromUndata,
 )

pstateNFTPolicy ::
  forall (s :: S).
  TokenName ->
  Term s (PTxOutRef :--> PUnit :--> PScriptContext :--> PUnit)  -- PUnit ~ ()
pstateNFTPolicy tn = plam $ \txOutRef _ ctx' -> P.do
  {-
    From Plutarch docs:
      `pmatch`, `plet`, and `pletFields - not convenient, 
        so
          we use "TermCont" monad an then should

      we can use regular `do` syntax on the `TermCont` monad. All the continuations are flattened! 
      Just remember to `unTermCont` the result
      
      *TermCont @b s a` essentially represents `(a -> Term s b) -> Term s b`
      *newtype TermCont :: forall (r :: PType). S -> Type -> Type where
        TermCont :: forall r s a. {runTermCont :: (a -> Term s r) -> Term s r} -> TermCont @r s a
  -}

  ctx <- pletFields @'["txInfo", "purpose"] ctx'
  cs <- runTermCont $ getCs ctx.purpose
  txInfo <- pletFields @'["inputs", "mint", "id"] $ ctx.txInfo
  let mint :: Term s PValue
  -- pfromData :: Term s (PAsData a) -> Term s a
      mint = pfromData txInfo.mint
      inputs :: Term s (PBuiltinList (PAsData PTxInInfo))
      inputs = pfromData txInfo.inputs
  pif
    ( consumesRef # txOutRef # inputs
      -- pconstant: const to Plutarch (otherwise plam)
        #&& oneOf # cs # pconstant tn # mint
    )
    (pconstant ())
    perror


{-  This should mostly be avoided, though it can be safely
  used to assert known types of Datums, Redeemers or ScriptContext.

    punsafeCoerce :: Term s a -> Term s b
    punsafeCoerce (Term x) = Term x}
-}

pstateNFTPolicyUntyped ::
  forall (s :: S). TokenName -> Term s (PData :--> PData :--> PData :--> PUnit)
  --plam :: (Term s a -> Term s b) -> Term s (a :--> b)
pstateNFTPolicyUntyped tn = plam $ \utxo _ ctx ->
    --unTermCont :: TermCont @a s (Term s a) -> Term s a
  pstateNFTPolicy tn # unTermCont (ptryFromUndata utxo) 
    # pconstant ()
    # punsafeCoerce ctx

consumesRef ::
  forall (s :: S).
  Term s (PTxOutRef :--> PBuiltinList (PAsData PTxInInfo) :--> PBool)
consumesRef = phoistAcyclic $
  plam $ \txOutRef ->
    -- check that predicate holds for any element in a list.
      -- pany :: PIsListLike list a => Term s ((a :--> PBool) :--> list a :--> PBool)
    pany #$ plam $ \info -> pdata txOutRef #== pdata (getOutRef info) 
  where
    getOutRef ::
      forall (s :: S).
      Term s (PAsData PTxInInfo) ->
      Term s PTxOutRef
    getOutRef info' = pfield @"outRef" # pfromData info'
