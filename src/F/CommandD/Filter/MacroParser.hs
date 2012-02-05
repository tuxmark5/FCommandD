module F.CommandD.Filter.MacroParser
( NamedKey(..)
, parseMacro
) where

{- ########################################################################################## -}
import F.CommandD.Util.KeyMap
import Text.ParserCombinators.Parsec hiding (spaces)
{- ########################################################################################## -}

data NamedKey = NamedKey Char String String deriving (Show)

deviceName :: Parser (Maybe String)
deviceName = optionMaybe $ do
  dev <- identifier
  char ':'
  return dev

identifier :: Parser String
identifier = many1 (letter <|> digit <|> oneOf "@")

key :: Parser NamedKey
key = do
  flag  <- oneOf "+*"
  dev   <- return Nothing --deviceName
  key   <- identifier
  return $ case dev of
    Just dev  -> NamedKey flag dev key
    Nothing   -> NamedKey flag []  key
    
macro :: KeyMap -> Parser [Key]
macro kmap = many1 $ key >>= translate kmap

translate :: KeyMap ->  NamedKey -> Parser Key
translate kmap (NamedKey flag dev key) =
  case lookupKey kmap dev key of
    Just k    -> return $ k { keyFlag = if flag == '+' then 0 else 1 }
    Nothing   -> fail $ "unknown key: " ++ (show key) 

{- ########################################################################################## -}

parseMacro :: KeyMap -> String -> Either String [Key]
parseMacro kmap str = case parse (macro kmap) str str of
  Left  err   -> Left $ show err
  Right res   -> Right res

{- ########################################################################################## -}
