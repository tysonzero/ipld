{-# LANGUAGE LambdaCase, OverloadedStrings #-}

module Codec.DAG.CBOR (decode, encode) where

import Control.Category ((>>>))
import Codec.CBOR.Read (deserialiseFromBytes)
import Codec.CBOR.Term (Term(..), decodeTerm, encodeTerm)
import Codec.CBOR.Write (toLazyByteString)
import Control.Arrow ((***))
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as BL
import Data.Foldable (toList)
import Data.IPLD (Node(..))
import Data.IPLD.CID (buildCid, decodeCid)
import qualified Data.Map as M
import Data.Semigroup.Cancellative (stripPrefix)
import qualified Data.Text.Lazy as TL
import Data.Traversable (for)
import qualified Data.Vector as V
import GHC.Float (float2Double)

decode :: ByteString -> Maybe Node
decode = deserialiseFromBytes decodeTerm >>> \case
    Left _ -> Nothing
    Right (_, t) -> go t
  where
    go = \case
        TInt i -> Just . Integer $ fromIntegral i
        TInteger i -> Just $ Integer i
        TBytes b -> Just $ Bytes b
        TBytesI b -> Just . Bytes $ BL.toStrict b
        TString t -> Just $ String t
        TStringI t -> Just . String $ TL.toStrict t
        TList xs -> List . V.fromList <$> traverse go xs
        TListI xs -> List . V.fromList <$> traverse go xs
        TMap m -> Map . M.fromList <$> traverse goMapEntry m
        TMapI m -> Map . M.fromList <$> traverse goMapEntry m
        TTagged 42 t -> go t >>= \case
            Bytes b -> do
                b' <- stripPrefix "\0" b
                case decodeCid b' of
                    Left _ -> Nothing
                    Right l -> Just $ Link l
            _ -> Nothing
        TTagged _ t -> Nothing
        TBool b -> Just $ Boolean b
        TNull -> Just Null
        TSimple i -> Nothing
        THalf f -> Just . Float $ float2Double f
        TFloat f -> Just . Float $ float2Double f
        TDouble d -> Just $ Float d
    goMapEntry (k, v) = do
        k' <- go k
        v' <- go k
        case k' of
            String t -> Just (t, v')
            _ -> Nothing

encode :: Node -> ByteString
encode = toLazyByteString . encodeTerm . go
  where
    go = \case
        Null -> TNull
        Boolean b -> TBool b
        Integer i -> TInteger i
        Float d -> TDouble d
        String t -> TString t
        Bytes b -> TBytes b
        List xs -> TList $ go <$> toList xs
        Map m -> TMap $ (TString *** go) <$> M.toList m
        Link l -> TTagged 42 . TBytes $ "\0" <> BL.toStrict (BB.toLazyByteString $ buildCid l)
