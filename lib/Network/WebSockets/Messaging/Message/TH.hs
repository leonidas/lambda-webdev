{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Network.WebSockets.Messaging.Message.TH
    ( deriveRequest
    , deriveNotify
    ) where

import Control.Applicative ((<$>))
import Control.Monad (replicateM)

import Data.Aeson
import Data.Aeson.Types
import Data.String

import Language.Haskell.TH

import Network.WebSockets.Messaging.Message

deriveRequest :: Name -> DecsQ
deriveRequest = deriveMessage True

deriveNotify :: Name -> DecsQ
deriveNotify = deriveMessage False


deriveMessage :: Bool -> Name -> DecsQ
deriveMessage req n = do
    (TyConI (DataD _ _ tyvars cons _)) <- reify n
    let genToJson = caseE (varE $ mkName "m") $ map conToJson cons
        genFromJson = [|
            let p (Object o) = do
                    typ <- o .: "msg"  :: Parser String
                    dat <- o .: "data" :: Parser Value
                    $(caseE (varE $ mkName "typ") . wild
                        $ map (conFromJson req) cons)

                p _ = fail "invalid message, object expected"
            in parse p
            |]
        wild = flip (++) [match wildP (normalB [|fail "invalid message"|]) []]

    if req
        then [d|
            instance Request $(conT n) where
                reqToJSON m = $(genToJson)
                reqFromJSON = $(genFromJson)
            |]
        else [d|
            instance Notify $(conT n) where
                ntfyToJSON m = $(genToJson)
                ntfyFromJSON = $(genFromJson)
            |]

conFromJson :: Bool -> Con -> MatchQ
conFromJson req (ForallC _ _ c) = conFromJson req c
conFromJson req c = do
    let (name, numFields) = case c of
            NormalC n ts -> (n, length ts)
            RecC n ts -> (n, length ts)

    varNames <- replicateM numFields (newName "f")

    let base = nameBase name

        pat = litP $ StringL base
        dat = varE (mkName "dat")
        con = conE name

        tuple  = return $ TupP $ map VarP varNames
        fields = map varE varNames

        body = case c of
            NormalC _ ts -> case ts of
                []  -> [|case $dat of
                    Null -> return $con
                    _    -> fail "unexpected data for unit message"
                    |]
                [_] -> [|$con <$> parseJSON $dat|]
                _   -> doE [bind, noBindS [| return $(appsE (con:fields)) |]]
                where bind = bindS tuple [|parseJSON|]

            RecC _ ts -> [|let Object datObj = $dat in $expr|] where
                datObj = varE (mkName "datObj")
                expr   = doE [bind, noBindS [| return $(appsE (con:fields)) |]]
                bind   = bindS tuple $ tupE $ map getField ts
                getField (nameBase -> n, _, _) = [| $datObj .: fromString n |]

        body' = [|Some <$> $body|]

    if req
        then match pat (normalB body') []
        else match pat (normalB body) []


conToJson :: Con -> MatchQ
conToJson (ForallC _ _ c) = conToJson c
conToJson c = do
    let (name, numFields) = case c of
            NormalC n ts -> (n, length ts)
            RecC n ts -> (n, length ts)

        base = nameBase name

    varNames <- replicateM numFields (newName "f")

    let pat = conP name $ map varP varNames
        body = normalB $ [|object ["msg" .= (base :: String), "data" .= $(dat)]|]
        vars = map varE varNames
        dat = case c of
            NormalC _ _ -> case vars of
                []  -> [|Null|]
                [v] -> [|toJSON $v|]
                vs  -> [|toJSON $(listE $ map (\v -> [|toJSON $v|]) vs)|]

            RecC _ ts -> [|object $(listE fields)|] where
                fields = zipWith field vars ts
                field v (n,_,_) = [|fromString fieldName .= $v|]
                    where fieldName = nameBase n

    match pat body []
