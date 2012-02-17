module F.CommandD.Filter.MacroParser
( NamedKey(..)
, parseMacro
) where

{- ########################################################################################## -}
import Data.Bits ((.|.))
import F.CommandD.Util.KeyMap
import Text.ParserCombinators.Parsec hiding (spaces)
{- ########################################################################################## -}

data NamedKey = NamedKey Char String String deriving (Show)

deviceName :: Parser (Maybe String)
deviceName = optionMaybe $ do
  char ':'
  identifier

identifier :: Parser String
identifier = many1 (letter <|> digit <|> oneOf "@")

key :: Parser NamedKey
key = do
  flag  <- oneOf "+*!"
  key   <- identifier
  dev   <- deviceName
  return $ case dev of
    Just dev  -> NamedKey flag dev key
    Nothing   -> NamedKey flag []  key

macro :: KeyMap -> Parser [Key]
macro kmap = many1 $ key >>= translate kmap

parseFlag :: Char -> Int32
parseFlag '+' = flagFiltered
parseFlag '*' = flagModifier
parseFlag '!' = flagFiltered .|. flagModifier

translate :: KeyMap ->  NamedKey -> Parser Key
translate kmap (NamedKey flag dev key) =
  case lookupDevKey' kmap dev key of
    Just k    -> return $ k { keyFlag = parseFlag flag }
    Nothing   -> fail $ "unknown key: " ++ (show key) 

{- ########################################################################################## -}

parseMacro :: KeyMap -> String -> Either String [Key]
parseMacro kmap str = case parse (macro kmap) str str of
  Left  err   -> Left $ show err
  Right res   -> Right res

{- ########################################################################################## -}
