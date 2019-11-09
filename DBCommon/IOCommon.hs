{-# LANGUAGE DeriveDataTypeable #-} 
{-# LANGUAGE ScopedTypeVariables #-} 
{-# LANGUAGE FlexibleContexts #-} 
{- |
   Module      : DBCommon.IOCommon 
   Description : Input + Error + Output functions for CSV files 
   Copyright   : ( c ) Matthew Lawler 2018 
   License     : 2-Clause BSD
   Maintainer  : lawlermj1@gmail.com

   This module defines common Input + Error + Output functions for CSV files 
   
 -}
module DBCommon.IOCommon   
    (  
--    Parameters  
      ParmInput( .. ), 
      parmInputTSVFile, 
      
      ReadResult( .. ), 
      readResultDefault, 
      
      ToPrint( .. ), 
      toPrintDefault,     

--    Input  
      readWrapper, 
      read2StringList, 
      typeSplit,
      errors2String,

--    Output fns      
      listToPrint, 
      errorsToPrint,
      printFile,

     ) where
--------------------------------------------------------------------------------

import Control.Exception 

-- import Data.Char
import Data.Data  
import Data.Either 
import Data.List ( intercalate )
-- import Data.Typeable 

import System.Directory
import System.IO 

import qualified Text.ParserCombinators.Parsec as P 
import Text.Parsec.Error 
import Text.Parsec.Prim 

import Data.Functor.Identity

import DBCommon.PreludeGapFiller   
import DBCommon.Boilerplate 
import DBCommon.EnumType 

--------------------------------------------------------------------------------
--    parameter file for input files  
--    add is SubjectArea here 
data ParmInput = ParmInput { 
--    used to speed reading, by skipping over large files, if they are not needed for load 
                      inputDoNotLoadTypes :: [String] 
--    determines if type is not specific to a database, but applies to all 
                    , inputIsAllType :: [String] 
                    } deriving (  Eq, Ord, Typeable, Data  ) 
                    
--    contents for capturing results of text file read 
data ReadResult a = ReadResult { 
--    has a parse error message
                      readResultErrorParse :: String
--    has a the parsed strings 
                    , readResultStringsParsed :: [[String]] 
--    has a type error messages
                    , readResultTypeErrors :: [[String]] 
--    has clean strings before type loading 
                    , readResultStrings :: [[String]] 
--    loaded type 
                    , readResultType :: [a] 
                    } deriving (  Eq, Ord, Typeable, Data  )                     

--    contents for printing using printFile function 
data ToPrint = ToPrint { 
--    filename to write to 
                      toPrintFileName :: String
--    contents to write in filename 
                    , toPrintContents :: String
                    } deriving (  Eq, Ord, Typeable, Data  ) 
                    
--------------------------------------------------------------------------------
--    Boilerplate code 

parmInputDefault :: ParmInput
parmInputDefault = gdefaultU ( undefined::ParmInput ) 
toPrintDefault :: ToPrint
toPrintDefault = gdefaultU ( undefined::ToPrint ) 

readResultDefault :: p -> ReadResult a 
readResultDefault _ = ReadResult "" [] [] [] [] 

--------------------------------------------------------------------------------    
--    Boilerplate Shows 
instance Show ParmInput where show = gshowQ 
instance Show ToPrint where show = gshowQ 

--    need to make the a showable 
instance Show a => Show ( ReadResult a ) where 
    show ( ReadResult a b c d e ) 
        = intercalate " : " ["ReadResult a ", show a, show b, show c, show d, show e] 

--------------------------------
--    parameters  ActiveSubjectAreaOnlyIn 
--parmInputTSVFile = parmInputDefault { inputInFilter = NoInFilter, inputIsAllType = ["Domain","Authority","ColumnBP","Snippet2PhraseIn","PhraseIn"], inputDoNotLoadTypes = ["ProfileCV100","ProfileIV100"] } 
parmInputTSVFile :: ParmInput
parmInputTSVFile = parmInputDefault { inputIsAllType = ["Domain","Authority","ColumnBP","Snippet2PhraseIn","PhraseIn"], inputDoNotLoadTypes = ["ProfileCV100","ProfileIV100"] } 

-----------------------------------------------------------------------
--    PUBLIC FUNCTIONS 

--    P01 
--    wrapper for the reading text files 
readWrapper :: ( Typeable a, Data a ) => ParmInput -> String -> a -> ( [String] -> a ) -> IO ( ReadResult a )   
readWrapper parmInput file_name typeDefault typeLoad 
    = do parsedList <- read2StringList parmInput file_name typeDefault 
         let stringsList = tailSafe ( getRight parsedList ) 
         let stringsListTuple = typeSplit typeDefault stringsList  
         let stringsListGood = fst stringsListTuple 
         return ( ReadResult ( getLeft parsedList ) stringsList ( snd stringsListTuple ) stringsListGood ( map typeLoad stringsListGood ) ) 

--    P03 
--    allows all strings that fail the type test to be displayed 
--    these have already been filtered 
errors2String :: ( Data a ) => a -> [[String]] -> String 
errors2String aType ss 
    | ss == [] = " No Errors for " ++ getStaticType aType 
    | otherwise = " Errors for " ++ getStaticType aType ++ " : " ++ ( intercalate " - " ( map ( intercalate ", " ) ss ) ) 

--    P04 
--    uses a typed TSV show for contents so that output can be used by Excel, etc 
--      maybe pass in FileType ( TSV, TXT ) as a parm if the show type is different from file type 
listToPrint :: ( Typeable a, Show a, Eq a, Data a ) => FileType -> [a] -> ToPrint  
listToPrint filetype aList 
--    doesn't call head if list is empty 
    | length aList == 0 = toPrintDefault 
--    head is safe 
    | otherwise = toPrintDefault { 
            toPrintFileName = "common.out." ++ getStaticType ( head aList ) ++ "." ++ show filetype, 
            toPrintContents = ( if filetype == TSV then showTSV else show ) aList } 

--    P05 
--    a standard way to show error values that fail to pass the type test 
--    Assumes TSV output 
errorsToPrint :: ( Typeable a, Show a, Eq a, Data a ) => a -> [[String]] -> ToPrint  
errorsToPrint aType sList 
--    doesn't call head if list is empty 
    | length sList == 0 = toPrintDefault 
--    head is safe 
    | otherwise = toPrintDefault { 
            toPrintFileName = "common.out." ++ getStaticType aType ++ ".Error.TSV" , 
            toPrintContents = showTSVError sList } 
--    P06
--    prints outString to a file called fname 
printFile :: String -> String -> IO (  ) 
printFile fname outString = 
    bracketOnError ( openTempFile "." "temp" )
        ( \( tempName, tempHandle ) -> do
            hClose tempHandle
            removeFile tempName )
        ( \( tempName, tempHandle ) -> do
            hPutStr tempHandle outString 
            hClose tempHandle
            renameFile tempName fname ) 
    
--------------------------------------------------------------------------------
--    HIDDEN FUNCTIONS 

--    P01 
--    4th version of reading files into types 
--    naive monadic structure; improve by using the Bind Monad 
--    check if file exists, then parse into lists of string lists 
--    errorMessages :: ParseError -> [Message]   Extracts the list of error messages from the parse error 
--    messageString :: Message -> String     Extract the message string from an error message 
read2StringList :: ( Typeable a ) => ParmInput -> String -> a -> IO ( Either String [[String]] )   
read2StringList parmInput file_name typeDefault = do  
--    It is assumed that the file is in the same directory as this program 
    if ( elem showType ( inputDoNotLoadTypes parmInput ) ) 
        then return ( Left ( "Error!: The Type " ++ showType ++ " is in the inputDoNotLoadTypes set. "  ) ) 
        else 
            do
--    Does this file exist? 
                fileExists <- doesFileExist file_name 
--    if the file exists, get and parse the file, and remove from IO monad 
                if ( not fileExists ) 
                    then return ( Left ( "Error!: The File " ++ file_name ++ " does not exist. "  ) ) 
                    else 
                        do 
                            parseResult <- getParse file_name showType 
                            case parseResult of 
                                Left err -> return ( Left ( "Error!: The File " ++ file_name ++ " has Parse errors: " ++ intercalate ". " ( map messageString ( errorMessages err ) ) ) ) 
                                Right xs -> return ( Right xs )                         
                                where showType = ( getStaticType typeDefault ) 

--    P02 
--    simple way to split the parsed list by type error  
--    this does a single scan to get both lists in a tuple 
--    good type strings are fst, and failed strings are snd 
--    pattern: 
--    foldl ( \( evens,odds ) e -> if e `mod` 2 == 0 then ( e:evens, odds ) else ( evens, e:odds ) ) ( [],[] ) ( reverse [1..10] )
typeSplit :: ( Data a ) => a -> [[String]] -> ( [[String]],[[String]] ) 
typeSplit aType ss 
--    may not be needed for a fold? 
    | ss == [] = ( [],[] ) 
--    checks if the strings satisfy type values 
    | otherwise = foldl ( \( ok,notOk ) s -> if ( gcheckU aType s ) then ( s:ok, notOk ) else ( ok, s:notOk ) ) ( [],[] ) ss 

--    where errList = filter ( not.( gcheckQ aType ) ) ss  
    
--    H01 
--    produce tab separated output
showTSV :: ( Show a, Typeable a, Eq a, Data a ) => [a] -> String 
showTSV aList 
    | aList == [] = "Error! showTSV? Nothing in aList" 
--    head is safe 
    | otherwise = foldl ( \acc x -> acc ++ show x ++ "\n" ) ( ( gheaderShowQ ( head aList ) ) ++ "\n" ) aList 
    
--    H02 
--    produce tab separated output for errors concat
showTSVError :: [[String]] -> String 
showTSVError sList 
    = foldl ( \acc s -> acc ++ ( intercalate "\t" s ) ++ "\n" ) "" sList

--    H02 
--    use PARSEC on input file 
getParse :: String -> String -> IO ( Either P.ParseError [[String]] ) 
getParse file_name showType = do 
    inData <- readFile file_name
    return ( parseWrap showType inData ) 

--    H03 
--    simple, powerful parsing functions from Real World Haskell 
--    reads a CSV file strictly, and parses it 
--    simple parse wrapper 
parseWrap :: String -> String -> Either P.ParseError [[String]] 
parseWrap showType inData = 
    P.parse csvFile showType inData 
        where
--            csvFile = trace ( "csvFile: " ) ( P.endBy line eol )
            csvFile = P.endBy line eol
--    note use either , tab, or | as the separator 
            line = P.sepBy cell ( P.oneOf ",\t" ) 
--    cells can have quotes, or separated by , tab or | or returns 
            cell = quotedCell P.<|> P.many ( P.noneOf ",\t\n\r" ) 
--                    P.<|> P.many ( P.noneOf ",\t|\n\r" ) 

--    H04 
--    what is a quoted cell? 
quotedCell :: Text.Parsec.Prim.ParsecT
                      [Char] u Data.Functor.Identity.Identity [Char]
quotedCell = do
    _ <- P.char '"'
    content <- P.many quotedChar
    _ <- P.char '"' P.<?> "quote at end of cell" 
    return content
    
--    H05 
--    define quotes 
quotedChar :: Text.Parsec.Prim.ParsecT
                      [Char] u Data.Functor.Identity.Identity Char 
quotedChar = P.noneOf "\"" P.<|> P.try ( P.string "\"\"" >> return '"' )

--    H06 
--    eol parsec 
eol :: Text.Parsec.Prim.ParsecT
               [Char] u Data.Functor.Identity.Identity String 
eol = P.try ( P.string "\n\r" ) P.<|> P.try ( P.string "\r\n" ) P.<|> P.string "\n" P.<|> P.string "\r" P.<?> "end of line"
-- Testing 
-- parseCSV :: String -> Either P.ParseError [[String]] 
-- parseCSV input = P.parse csvFile "( unknown )" input

--    H07 
-- parse out all logical names 
-- This reads a whole file of logical names, and splits up all strings by nameFormatSeparatorLogicalIn. 
-- Useful for extracting each individual word. 
-- Retain these as valid logical names "#$<>+&%"
--parseLog :: String -> String -> [[String]] 
parseLog :: (Text.Parsec.Prim.Stream
                     s Data.Functor.Identity.Identity t1,
                   Text.Parsec.Prim.Stream s Data.Functor.Identity.Identity Char,
                   Text.Parsec.Prim.Stream s Data.Functor.Identity.Identity t2) =>
                  P.SourceName -> [Char] -> s -> [[[Char]]] 
parseLog showType nameFormatSeparatorLogicalIn inData = 
    tailSafe ( concat ( rights [P.parse logEnd showType inData] ) ) 
        where
            logEnd = P.endBy logSep logEol
            logSep = P.sepBy ( P.many ( P.noneOf nameFormatSeparatorLogicalIn ) ) ( P.oneOf "" ) 
            logEol = ( P.oneOf nameFormatSeparatorLogicalIn ) 
            
