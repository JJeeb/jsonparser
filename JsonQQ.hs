{-# LANGUAGE TemplateHaskell #-}

module JsonQQ where

import qualified JsonParser (json, Json)
import Text.ParserCombinators.Parsec

import qualified Language.Haskell.TH as TH
import Language.Haskell.TH.Quote
import Data.Generics.Aliases (extQ)



parseJson :: Monad m => (String, Int, Int) -> String -> m JsonParser.Json
parseJson (file, line, col) s =
    case runParser p () "" s of
      Left err  -> fail $ show err
      Right e   -> return e
  where
    p = do  pos <- getPosition
            setPosition $
              (flip setSourceName) file $
              (flip setSourceLine) line $
              (flip setSourceColumn) col $
              pos
            spaces
            e <- JsonParser.json
            eof
            return e


json  :: QuasiQuoter
json  =  QuasiQuoter quoteJsonExp quoteJsonPat


quoteJsonExp :: String -> TH.ExpQ
quoteJsonExp s =  do  loc <- TH.location
                      let pos =  (TH.loc_filename loc,
                                 fst (TH.loc_start loc),
                                 snd (TH.loc_start loc))
                      json <- parseJson pos s
                      dataToExpQ (const Nothing) json




quoteJsonPat :: String -> TH.PatQ
quoteJsonPat = undefined


