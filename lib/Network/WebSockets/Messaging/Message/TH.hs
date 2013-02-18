{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Network.WebSockets.Messaging.Message.TH where

import Control.Applicative ((<$>))
import Control.Monad (replicateM)

import Data.Aeson
import Data.Aeson.Types
import Data.String

import Language.Haskell.TH

import Network.WebSockets.Messaging.Message

deriveMessage :: Name -> DecsQ
deriveMessage n = do
    (TyConI (DataD _ _ tyvars cons _)) <- reify n
    let genToJson = CaseE (VarE $ mkName "m") <$> mapM conToJson cons
        genFromJson = [|
            let p (Object o) = do
                    typ <- o .: "msg"  :: Parser String
                    dat <- o .: "data" :: Parser Value
                    $(CaseE (VarE $ mkName "typ") <$> mapM conFromJson cons)

                p _ = fail "invalid message, object expected"
            in parse p
            |]

    case length tyvars of
        0 -> [d|
            instance Message $(conT n) where
                msgToJSON m = $(genToJson)
                msgFromJSON = $(genFromJson)
            |]
        1 -> [d|
            instance Message ($(conT n) t) where
                msgToJSON m = $(genToJson)
                msgFromJSON = $(genFromJson)
            |]
        _ -> fail "Types with more than one type variable not supported"

conFromJson :: Con -> MatchQ
conFromJson (ForallC _ _ c) = conFromJson c
conFromJson c = do
    let (name, numFields) = case c of
            NormalC n ts -> (n, length ts)
            RecC n ts -> (n, length ts)

    varNames <- replicateM numFields (newName "f")

    let base = nameBase name

        pat = litP $ StringL base
        dat = varE (mkName "dat")
        datObj = varE (mkName "datObj")
        con = conE name

        tuple  = return $ TupP $ map VarP varNames
        fields = map varE varNames
        -- foldr (conE name) $ replicate numFields [|parseJSON|]
        body = normalB $ case c of
            NormalC _ ts -> case ts of
                []  -> [|return $con|]
                [_] -> [|$con <$> parseJSON $dat|]
                _   -> doE [bind, noBindS [| return $(appsE (con:fields)) |]]
                where bind = bindS tuple [|parseJSON|]

            RecC _ ts -> [|let Object datObj = $dat in $expr|] where
                expr = doE [bind, noBindS [| return $(appsE (con:fields)) |]]
                bind = bindS tuple $ tupE $ map getField ts
                getField (nameBase -> n, _, _) = [| $datObj .: fromString n |]


    match pat body []

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
