{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module ApiTranslate where

import GHC.Generics
import Control.Lens
import Control.Concurrent.Async
import Network.Wreq
import Data.Text (Text)
import Data.Aeson
import qualified Data.Text.IO as T


data TranslateRequest = TranslateRequest {
        q :: Text,
        source :: Text,
        target :: Text,
        format :: Text
    } 
    deriving (Generic, Show)

data TranslateResponse = TranslateResponse {
        translatedText :: Text
    } 
    deriving (Generic, Show)

data Language = Language {
        code :: Text,
        name :: Text
    } 
    deriving (Generic, Show)

instance ToJSON TranslateRequest

instance FromJSON TranslateResponse

instance FromJSON Language

doTranslate :: IO [Text]
doTranslate = do
    langs <- getLanguages
    results <- forConcurrently langs $ \lang -> do
        result <- translateText "Haskell is an interesting programming language" "en" (code lang)
        pure ((name lang) <> " : " <> result)
    pure results


-- Get languages from API
-- Method: GET
-- Endpoint: https://translate.argosopentech.com/languages
getLanguages :: IO [Language]
getLanguages = do
    rsp <- asJSON =<< get "https://translate.argosopentech.com/languages"
    pure (rsp ^. responseBody)


-- Get languages from API
-- Method: POST
-- Endpoint: https://translate.argosopentech.com/languages
translateText :: Text -> Text -> Text -> IO Text
translateText text sourceLang targetLang = do
    rsp <- asJSON =<< post "https://translate.argosopentech.com/translate" (toJSON (TranslateRequest {
        q = text,
        source = sourceLang,
        target = targetLang,
        format = "text"  
    }))
    pure (translatedText (rsp ^. responseBody))