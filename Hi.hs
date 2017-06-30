{-# LANGUAGE DeriveDataTypeable, OverloadedStrings, RecordWildCards #-}
import Control.Applicative
import Control.Monad (void)
import Data.List
import Data.Scientific as S hiding (scientific)
import Data.Text.Lazy as T hiding (map, count)
import Data.Text.Lazy.IO as TIO
import Prelude hiding (exponent, id)
import Data.Attoparsec.Text.Lazy
import System.Console.CmdArgs
import System.Environment

-- Reading ploc using Attoparsec : fast but not helpful error messages.
-- For debug, use parseTest and ghci for each component.
--
-- The file format is
-- TIME
-- HEADER
-- [PARTICLE]
--
-- with
--
-- TIME = realtime = FLOAT [gamt = FLOAT]
-- HEADER = PART # XX YY ANGZ | ZZ  ALPHA BETA GAMMA ADX ADY ADZ
-- PARTICLE = INT FLOAT*9
data Particle = Particle {
  id :: !Int,
  pos :: !Coord,
  adX, adY, adZ :: !Int
}

data Coord = Coord { xx, yy, zz, alpha, beta, gamma :: !Double }

data Iteration = Iteration {
  realtime :: Scientific,
  particles :: [Particle]
}

toText :: Show a => a -> T.Text
toText = T.pack . show

addComma x = T.intercalate "," $ map toText x

printPart :: Particle -> T.Text
printPart (Particle i p adX adY adZ) =  T.intercalate "," l
    where l = [toText i, addComma $ coordToList p, addComma [adX, adY, adZ]]
          coordToList (Coord {..}) = [xx, yy, zz, alpha, beta, gamma]

printIter :: Iteration -> T.Text
printIter (Iteration t p) = T.intercalate "\n" $ map format p
      where format x = T.concat [toText t, ",", printPart x]

signedInt :: Parser Int
signedInt = signed decimal

mySep1 = some $ char ' '

mySep = many space

gamt = mySep >> asciiCI "gamt =" >> mySep >> scientific

-- time :: Parser Scientific
time = do
  mySep >> asciiCI "REALTIME =" >> mySep
  t <- scientific
  t' <- option 0 gamt
  return t

-- Helper
stringify x = mySep >> asciiCI x

-- Two headers are possible : the 5th column can be "zz" or "ANGZ"
header = do
  mapM_ stringify header0
  mySep *> (asciiCI "zz" <|> asciiCI "ANGZ")
  mapM_ stringify header1
  where
    header0 = [ "PART", "#" , "XX", "YY"]
    header1 = [ "ALPHA", "BETA", "GAMMA" , "ADX", "ADY", "ADZ"]

-- Read a particle coordinates
part :: Parser Particle
part = do
  id <- mySep >> decimal <* mySep1
  coord <- Coord <$> double <* mySep1
                 <*> double <* mySep1
                 <*> double <* mySep1
                 <*> double <* mySep1
                 <*> double <* mySep1
                 <*> double <* mySep1
  asd <- sepBy signedInt mySep1
  Particle id coord
    <$> signedInt <* mySep1
    <*> signedInt <* mySep1
    <*> signedInt <* mySep1

emptyLine = mySep >> endOfLine

 -- Read an iteration
iter :: Parser Iteration
iter = do
  t <- time <* endOfLine
  header  >> endOfLine
  allPart <- sepBy part endOfLine
  return $ Iteration t allPart

parseExpr = space >> sepBy iter space

readExpr input = case eitherResult . parse parseExpr $ input of
  Left err -> error "failed to read"
  Right val -> val
--
data ParserArgs = ParserArgs { input :: String
                             , output :: FilePath }
                   deriving (Show, Data, Typeable)

parserArgs = ParserArgs {
                input = def &= argPos 0 &= typ "INPUT"
                , output = def &= argPos 1 &= typ "OUTPUT"
                }

main = do
  args <- cmdArgs parserArgs
  txt <- TIO.readFile $ input args
  let d = readExpr txt
  let result = T.intercalate "\n" $ map printIter d
  TIO.writeFile (output args) result
  print "done"
