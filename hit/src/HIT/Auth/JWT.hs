{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}


module HIT.Auth.JWT
        (
          makeClaims
        , signClaims
        , secretKey 
        , validateJWT      
        ) 
        where

import Data.Text (Text)
import qualified Web.JWT as JWT
import Data.Time.Clock (NominalDiffTime, getCurrentTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import HIT.Types.Identifier (Username (..))

-- JWTClaimsSet is a data type, represents a JSON object with claims in its body
makeClaims :: Username -> IO JWT.JWTClaimsSet
makeClaims username = do 
        time <- getCurrentTime

        pure $ mempty 
                { JWT.sub = JWT.stringOrURI (username.name)        -- subject
                , JWT.exp = JWT.numericDate $ day $ utcTimeToPOSIXSeconds $ time   -- expires 
                }


--sign claims using generated key (static rn). Might not need to pass in IO JWT...
signClaims :: Text -> IO JWT.JWTClaimsSet -> IO Text
signClaims key ioClaims = do

        claims <- ioClaims  
        let secret = JWT.hmacSecret key 
            jwtHeader = mempty 
                { JWT.alg = Just JWT.HS256 }
        pure $ JWT.encodeSigned secret jwtHeader claims


-- validates the structure of jwt, does NOT match the subject to a username
validateJWT :: Text -> Text -> IO (Maybe Text)
validateJWT jwt key = case 
        (verifySignature jwt key) of 
                Nothing -> pure Nothing 
                Just verifiedJWT -> do

                        claimsSet <- verifyExpiration verifiedJWT
                        pure (claimsSet >>= getSubject)


verifyHeader :: JWT.JWT JWT.UnverifiedJWT -> Maybe JWT.Algorithm
verifyHeader jwt = let 
        header = JWT.header jwt 

        in case JWT.alg header of 
                Nothing -> Nothing 
                Just algorithm -> 
                        if algorithm == (JWT.HS256) 
                                then Just algorithm
                        else Nothing


--verify signature to ensure message has not been altered, check header to ensure matching algorithm
verifySignature :: Text -> Text -> Maybe (JWT.JWT JWT.VerifiedJWT)
verifySignature input key =
        case JWT.decode input of 
                Nothing -> Nothing 
                Just unverifiedJWT -> 
                        case verifyHeader unverifiedJWT of     
                                Nothing -> Nothing
                                Just _ -> let
                                        secret = JWT.hmacSecret key 
                                        in JWT.verify (JWT.toVerify secret) unverifiedJWT


verifyExpiration :: JWT.JWT JWT.VerifiedJWT -> IO (Maybe JWT.JWTClaimsSet) 
verifyExpiration input = let 
                jwtClaims = JWT.claims input
                in case jwtClaims.exp of 
                        Nothing -> pure Nothing 
                        Just expires -> do
                                currentTime <- getCurrentTime
                                
                                if (utcTimeToPOSIXSeconds currentTime) < (JWT.secondsSinceEpoch expires) 
                                  then pure (Just jwtClaims)
                                else pure Nothing


-- doesnt compare username but ensures it is a value in the jwt and extracts it
getSubject :: JWT.JWTClaimsSet -> Maybe Text
getSubject claims = case 
        JWT.sub claims of 
                Nothing -> Nothing 
                Just subject -> Just (JWT.stringOrURIToText subject)     -- look into simplifying with monadic funcs e.g. <$> or >>= so context extraction is avoided
                                
                                 
--assume input is in NominaldiffTime so add seconds to it. In future could have different expire times for different auth levels
day :: NominalDiffTime -> NominalDiffTime
day  diffTime = diffTime + 86400

--use a constant key for now (used randomkeygen/hmac-key.com) and HMAC-SHA256 / Base64
secretKey :: Text
secretKey = "ynX9es95xtGFL/zlw5qe1p8xH9pTUmrs1E/N1ywNON8"