{-# LANGUAGE DeriveDataTypeable #-} 
{- |
   Module      : DBGlossary.Glossary
   Description : Glossary functions
   Copyright   : ( c ) Matthew Lawler 2018 
   License     : 2-Clause BSD
   Maintainer  : lawlermj1@gmail.com

   This module defines glossary functions used to discover and verify parsed phrases extracted from a names list. This could be all columns in a DB Schema.  

 -}
module DBGlossary.GlossaryBP
    (  
--    PUBLIC Types: input
      Authority( .. ), 
      Domain( .. ), 
      Snippet2PhraseIn( .. ),           
      PhraseIn( .. ),
--    PUBLIC Types: output 
      Name2Snippet( .. ), 
      Snippet2Name( .. ),    
      Phrase2Name( .. ), 
      Name2Phrase( .. ), 
      Phrase( .. ),
      Snippet2Phrase( .. ), 
      Phrase2Snippet( .. ),
      Name2PhraseMin( .. ),
      Name2PhraseOut( .. ),
      Phrase2NameOut( .. ),
      ColumnName( .. ),
      
--    PUBLIC Type defaults  
      authorityDefault, 
      domainDefault, 
      snippet2PhraseInDefault, 
      phraseInDefault,
      name2SnippetDefault,      
      snippet2NameDefault,      
      phrase2NameDefault, 
      name2PhraseDefault, 
      phraseDefault,  
      columnNameDefault, 
      phrase2SnippetDefault, 
      name2PhraseMinDefault, 
      name2PhraseOutDefault, 
      phrase2NameOutDefault, 
      snippet2PhraseDefault, 
      
      authorityLoad, 
      domainLoad, 
      snippet2PhraseInLoad, 
      phraseInLoad, 
      columnNameLoad,       
      
--    M.Map functions 
      phrase2Map,
      snippet2PhraseMap, 
      phrase2SnippetMap, 
      name2PhraseMap, 
      
--    primary functions   
      phraseIn2Phrase, 
      filterS2PIn, 
      getSnippet2Phrase, 
      phrase2S2P,       
      makeTries, 
      name2Snippets, 
      parse2Name2Snippet,      
      getNameFreq, 
      name2Snippet2Name2Phrase,
      addNameFreq2Name2Phrase,
      addPhrase2Name2PhraseMap, 
      
--    secondary functions 
      getPhraseFreq,
      addPhraseFreq2Phrase,  
      invertSnippet2Phrase, 
      invertSnippet2Name,
      invertName2Phrase, 
      phrase2Phrase2Name, 
      addSnippets2Phrase2Name, 
      addNames2Phrase2Name, 
      phraseNotInS2PIn, 
      s2PNotInPhraseIn, 
      name2Phrase2Min, 
      maybeParseIssue,
      name2Phrase2Out,
      phrase2Name2Out,
      
--    used by other modules 
      definePhrase, 

     ) where
--------------------------------------------------------------------------------

import Data.Char 
import Data.Data  
import Data.Either 
import Data.List ( sortBy,nub,intercalate,isInfixOf,sort,( \\ ) ) 
import Data.Maybe 
import qualified Data.Map.Strict as M  
import Data.Ord  
import Data.Typeable 

import qualified Text.ParserCombinators.Parsec as P 

import DBCommon.PreludeGapFiller  
import DBCommon.Boilerplate  

import DBGlossary.EnumType 

--------------------------------------------------------------------------------
--    PUBLIC 
data Authority = Authority { authorityType :: AuthorityType 
                    , authorityName :: String
                    , authorityURL :: String 
                    } deriving (  Eq, Ord, Typeable, Data  ) 

data Domain = Domain { domainType :: DomainType 
                    , domainName :: String
                    } deriving (  Eq, Ord, Typeable, Data  ) 
                    
data Namespace = Namespace { namespaceType :: String 
                    , namespaceName :: String 
                    } deriving (  Eq, Ord, Typeable, Data  ) 

data Snippet2PhraseIn = Snippet2PhraseIn { snippet2PhraseInPhrase :: String 
                    , snippet2PhraseInSnippet :: String
                    } deriving (  Eq, Ord, Typeable, Data  ) 

data Snippet2Phrase = Snippet2Phrase { 
                      snippet2PhraseSnippet :: String 
                    , snippet2PhrasePhraseName :: String
                    , snippet2PhraseSnippetIn :: String
                    , snippet2PhrasePhraseType :: PhraseType 
                    , snippet2PhraseIsInPhrase :: Bool 
                    , snippet2PhrasePhrase :: Phrase 
                    } deriving (  Eq, Ord, Typeable, Data  ) 
                    
data Phrase2Snippet = Phrase2Snippet { 
                      phrase2SnippetPhrase :: String
                    , phrase2SnippetSnippets :: [String] 
                    } deriving (  Eq, Ord, Typeable, Data  ) 

data PhraseIn = PhraseIn { phraseInPhrase :: String
                    , phraseInPhraseType :: PhraseType 
                    , phraseInExpansion :: String
                    , phraseInDomain :: DomainType 
--    do not use this phrase in parsing. Useful to filter junk strings. 
                    , phraseInIgnore :: Bool 
--    use this phrase in parsing, but replace with Nothing 
                    , phraseInDiscard :: Bool                     
                    , phraseInMultiWordType :: MultiWordType 
                    , phraseInMultiBaseWord :: String 
                    , phraseInMBW1 :: String 
                    , phraseInMBW2 :: String 
                    , phraseInSubType :: String    
                    , phraseInAuthority :: String
                    , phraseInDefinition :: String
                    } deriving (  Eq, Ord, Typeable, Data  ) 
 
data Phrase = Phrase { 
                      phrasePhrase :: String
                    , phrasePhraseType :: PhraseType 
                    , phraseExpansion :: String
                    , phraseDomain :: DomainType 
                    , phraseMultiWordType :: MultiWordType 
                    , phraseLength :: Int 
                    , phraseIsAllUpper :: Bool
                    , phraseIsTitle :: Bool
                    , phraseHasUpperInTail :: Bool
                    , phraseHasNonAlphaNum :: Bool
                    , phraseHasSpaces :: Bool
                    , phraseUsedInNameCount :: Int        
                    , phraseIgnore :: Bool 
                    , phraseDiscard :: Bool 
                    , phraseMultiBaseWord :: String   
                    , phraseSubType :: String                     
                    , phraseAuthority :: String
                    , phraseDefinition :: String
                    , phraseCorrectWords :: [String] 
                    } deriving (  Eq, Ord, Typeable, Data  ) 
                    
data Name2Snippet = Name2Snippet { 
                      name2SnippetName :: String
                    , name2SnippetSnippets :: [String] 
                    , name2SnippetUnparsed :: String
                    , name2SnippetOrder :: String
                    } deriving (  Eq, Ord, Typeable, Data  ) 
                    
data Snippet2Name = Snippet2Name { 
                      snippet2NameSnippet :: String 
                    , snippet2NameNames :: [String] 
                    } deriving (  Eq, Ord, Typeable, Data  ) 
                    
data Name2Phrase = Name2Phrase { 
                      name2PhraseName :: String
                    , name2PhraseSnippets :: [String] 
                    , name2PhraseUnparsed :: String 
                    , name2PhraseIsNotOracleName :: Bool 
                    , name2PhraseIsAllParsed :: Bool 
                    , name2PhraseNoMissingStrings :: Bool 
                    , name2PhraseNameFrequency :: Int 
                    , name2PhraseSnippetsFinal :: [String] 
                    , name2PhraseNoMissingPhrases :: Bool  
                    , name2PhraseNoSingleD :: Bool 
                    , name2PhraseNoSingleS :: Bool 
                    , name2PhraseNoBaseMW :: Bool   
                    , name2PhraseBaseMWList :: [String] 
                    , name2PhraseNoInstanceMW :: Bool   
                    , name2PhraseInstanceMWList :: [String]                     
                    , name2PhraseUsesPlural :: Bool 
                    , name2PhraseUsesPastTense :: Bool 
                    , name2PhraseUsesAcronym :: Bool
                    , name2PhraseUsesLetter :: Bool 
                    , name2PhraseHasBlank :: Bool   
                    , name2PhraseNoExpansion :: Bool 
                    , name2PhraseSnippetsExpanded :: [String]                      
                    , name2PhraseSingleCharWordCount :: Int   
                    , name2PhraseDoubleCharWordCount :: Int 
                    , name2PhraseWordCount :: Int  
                    , name2PhrasePhrases :: [String] 
                    , name2PhraseCamelWithSep :: String 
                    , name2PhraseUpperWithSep :: String 
                    , name2PhraseCamelCase :: String 
                    , name2PhraseUpperNoSep :: String 
                    , name2PhrasePhraseList :: [Phrase] 
                    } deriving (  Eq, Ord, Typeable, Data  ) 
                    
data Name2PhraseMin = Name2PhraseMin { 
                      name2PhraseMinName :: String
                    , name2PhraseMinSnippets :: [String] 
                    , name2PhraseMinUnparsed :: String 
                    } deriving (  Eq, Ord, Typeable, Data  )        

data Name2PhraseOut = Name2PhraseOut { 
                      name2PhraseOutName :: String 
                    , name2PhraseOutSnippetsFinal :: [String] 
                    } deriving (  Eq, Ord, Typeable, Data  )                          
                    
data Phrase2Name = Phrase2Name { 
                      phrase2NamePhraseName :: String 
                    , phrase2NameSnippets :: [String] 
                    , phrase2NameUsedInNames :: [String] 
                    , phrase2NameNameCount :: Int 
                    , phrase2NamePhrase :: Phrase 
                    } deriving (  Eq, Ord, Typeable, Data  ) 
                    
data Phrase2NameOut = Phrase2NameOut { 
                      phrase2NameOutPhraseName :: String 
                    , phrase2NameOutPhraseType :: PhraseType 
                    , phrase2NameOutExpansion :: String 
                    , phrase2NameOutDomain :: DomainType 
                    , phrase2NameOutNameCount :: Int 
                    , phrase2NameOutUsedInNames :: [String] 
                    } deriving (  Eq, Ord, Typeable, Data  ) 
                    
data ColumnName = ColumnName { 
                      columnNameSchema :: String
                    , columnNameTableName :: String
                    , columnNamePosition :: Int 
                    , columnNameColumnName :: String
                    } deriving (  Eq, Ord, Typeable, Data  ) 

--------------------------------------------------------------------------------
--    Boilerplate DEFAULTS Public
authorityDefault = gdefaultU ( undefined::Authority ) 
domainDefault = gdefaultU ( undefined::Domain ) 
snippet2PhraseInDefault = gdefaultU ( undefined::Snippet2PhraseIn ) 
phraseInDefault = gdefaultU ( undefined::PhraseIn ) 
name2SnippetDefault = gdefaultU ( undefined::Name2Snippet ) 
snippet2NameDefault = gdefaultU ( undefined::Snippet2Name ) 
phraseDefault = gdefaultU ( undefined::Phrase ) 
phrase2SnippetDefault = gdefaultU ( undefined::Phrase2Snippet ) 
name2PhraseDefault = gdefaultU ( undefined::Name2Phrase ) 
snippet2PhraseDefault = gdefaultU ( undefined::Snippet2Phrase ) 
phrase2NameDefault = gdefaultU ( undefined::Phrase2Name ) 
name2PhraseMinDefault = gdefaultU ( undefined::Name2PhraseMin ) 
name2PhraseOutDefault = gdefaultU ( undefined::Name2PhraseOut ) 
phrase2NameOutDefault = gdefaultU ( undefined::Phrase2NameOut ) 
columnNameDefault = gdefaultU ( undefined::ColumnName ) 

--------------------------------------------------------------------------------
--    Boilerplate LOADS  
authorityLoad = gloadU ( undefined::Authority ) 
domainLoad = gloadU ( undefined::Domain ) 
snippet2PhraseInLoad = gloadU ( undefined::Snippet2PhraseIn ) 
phraseInLoad = gloadU ( undefined::PhraseIn ) 
columnNameLoad = gloadU ( undefined::ColumnName ) 

--------------------------------------------------------------------------------
--    Boilerplate SHOWS  
instance Show Authority where show = gshowQ  
instance Show Domain where show = gshowQ  
instance Show Snippet2PhraseIn where show = gshowQ  
instance Show PhraseIn where show = gshowQ  
instance Show Name2Snippet where show = gshowQ  
instance Show Snippet2Name where show = gshowQ  
instance Show Phrase2Name where show = gshowQ  
instance Show Name2Phrase where show = gshowQ  
instance Show Phrase where show = gshowQ  
instance Show Snippet2Phrase where show = gshowQ  
instance Show Phrase2Snippet where show = gshowQ  
instance Show Name2PhraseMin where show = gshowQ  
instance Show Name2PhraseOut where show = gshowQ  
instance Show Phrase2NameOut where show = gshowQ 
instance Show ColumnName where show = gshowQ 

--------------------------------------------------------------------------------
--    checking 
--    f p28             
--    phrases not in s2p 
phraseNotInS2PIn :: [PhraseIn] -> [Snippet2PhraseIn] -> [PhraseIn] 
phraseNotInS2PIn xs ys = xNotInY2 phraseInPhrase snippet2PhraseInPhrase xs ys 

--    f p28 
--    s2p not in phrase  
s2PNotInPhraseIn :: [Snippet2PhraseIn] -> [PhraseIn] -> [Snippet2PhraseIn] 
s2PNotInPhraseIn xs ys = xNotInY2 snippet2PhraseInPhrase phraseInPhrase xs ys 
        
--------------------------------------------------------------------------------
--    Map functions 
--    f p1 
--    convert n2p to a Map k v 
--    M.fromAscList has O( n ). Must Sort first! 
phrase2Map :: [Phrase] -> M.Map String Phrase 
phrase2Map phraseList  
    = M.fromAscList ( map ( \p -> ( phrasePhrase p, p ) ) phraseListSort ) 
        where phraseListSort = sortBy ( comparing phrasePhrase ) phraseList
        
--    f p2 
--    maps snippets and strings   
--    need to sort by snippet 
snippet2PhraseMap :: [Snippet2Phrase] -> M.Map String Phrase  
snippet2PhraseMap snippet2PhraseList 
    = M.fromAscList snippet2PhraseListSort 
        where snippet2PhraseListSort = sortBy ( comparing fst ) ( map ( \s -> ( snippet2PhraseSnippet s, snippet2PhrasePhrase s ) ) snippet2PhraseList )         
        
--    f p3  
--    maps snippets and phrases  
--    need to sort by snippet 
phrase2SnippetMap :: [Phrase2Snippet] -> M.Map String [String]  
phrase2SnippetMap phrase2SnippetList 
    = M.fromAscList phrase2SnippetListSort 
        where phrase2SnippetListSort = sortBy ( comparing fst ) ( map ( \w -> ( phrase2SnippetPhrase w, phrase2SnippetSnippets w ) ) phrase2SnippetList ) 
        
--    f p4   
--    maps phrases to N2P 
--    use gmapQ to Boilerplate this? 
name2PhraseMap :: [Name2Phrase] -> M.Map String Name2Phrase 
name2PhraseMap name2PhraseList 
    = M.fromAscList name2PhraseLU 
        where name2PhraseLU = sortBy ( comparing fst ) ( map ( \n2p -> ( name2PhraseName n2p, n2p ) ) name2PhraseList ) 
        
--------------------------------------------------------------------------------
--    primary public functions 
--    f p10 
--    initialise Phrase from PhraseIn   
phraseIn2Phrase :: PhraseIn -> Phrase 
phraseIn2Phrase pi 
    = phraseDefault {
          phrasePhrase = lp 
        , phrasePhraseType = phraseInPhraseType pi 
        , phraseExpansion = phraseInExpansion pi 
        , phraseAuthority = phraseInAuthority pi
        , phraseDefinition = phraseInDefinition pi 
        , phraseDomain = phraseInDomain pi 
        , phraseIsAllUpper = all isUpper lp 
        , phraseIsTitle = if lp /= [] then isUpper ( head lp ) && all isLower ( tail lp ) else False 
        , phraseHasUpperInTail = if lp /= [] then any isUpper ( tail lp ) else False 
        , phraseHasNonAlphaNum = not ( all isAlphaNum lp )
        , phraseHasSpaces = isInfixOf " " lp 
        , phraseLength = length lp 
        , phraseSubType = phraseInSubType pi 
        , phraseIgnore = phraseInIgnore pi 
        , phraseDiscard = phraseInDiscard pi 
        , phraseMultiBaseWord = phraseInMultiBaseWord pi 
        , phraseMultiWordType = phraseInMultiWordType pi         
--    currently limited to 2 word phrases only 
        , phraseCorrectWords = [phraseInMBW1 pi, phraseInMBW2 pi] 
        } 
    where lp = phraseInPhrase pi 

--    f p11 
--    filter for S2P based on phraseIgnore field 
filterS2PIn :: M.Map String Phrase -> Snippet2PhraseIn -> Bool 
filterS2PIn phraseMapLU s2pi 
    = ( not.phraseIgnore ) p 
    where 
        s = snippet2PhraseInPhrase s2pi 
        p = fromMaybe phraseDefault ( M.lookup s phraseMapLU ) 

--    f p12 
--    This converts input snippets into the main S2P type. 
--    takes 12 mins to print out 12,000 values from 7,000 s2pin 
--    can't decompose, as the sort is critical 
getSnippet2Phrase :: M.Map String Phrase -> [Snippet2PhraseIn] -> [Snippet2Phrase] 
getSnippet2Phrase s2pLU snippet2PhraseInLoad 
--    sort in reverse length, so Parsec always checks longer phrase first, before shorter ones  
    = reverse ( sortBy ( comparing ( \v -> length ( snippet2PhraseSnippet v ) ) ) snippet2PhraseLoad2 ) 
        where 
--    initialise snippet2Phrase
            snippet2PhraseLoad1 = fmap ( snippet2PhraseIn2Snippet2Phrase s2pLU ) snippet2PhraseInLoad  
--    add an underscore prefix to each phrase 
            snippet2PhraseLoad2 = snippet2PhraseLoad1 ++ ( map appendUnderscoreSnippet2Phrase snippet2PhraseLoad1 ) ++ ( map appendSpaceSnippet2Phrase snippet2PhraseLoad1 ) 

--    f p13 
--    Add Phrase for this S2P  field 
phrase2S2P :: M.Map String Phrase -> Snippet2Phrase -> Snippet2Phrase 
phrase2S2P phraseMapLU s2p 
    = s2p { snippet2PhrasePhrase = fromMaybe phraseDefault ( M.lookup ( snippet2PhrasePhraseName s2p ) phraseMapLU ) }  

--    f p14 
--    helper function to build a Parser for each phrase in list 
--    list should be in phrase frequency order for best performance 
--    choice :: [ReadP a] -> ReadP a
--    try :: ParsecT s u m a -> ParsecT s u m a
makeTries :: [String] -> P.Parser String 
makeTries xs = P.choice ( map ( \x -> P.try ( P.string x ) ) xs )  

--    f p15             
--    takes in a Trie list, a name, and outputs a list of snippets, the unparsed string, and an ordering string  
name2Snippets :: P.Parser String -> String -> ( String, [String], String, String )  
name2Snippets trieList x = ( x, parsed, unparsed, lastParsed ) 
        where 
            parsed = concat ( rights [P.parse ( P.many1 trieList ) "name2Snippets" x] ) 
            unparsed = drop ( length ( concat parsed ) ) x 
            lastParsed = if length unparsed == 0 then replicate 30 'Z' else ( ( if length parsed > 0 then ( last parsed ) else [] ) ++ unparsed ) 

--    f p16 
--    puts parse results into N2S 
parse2Name2Snippet :: ( String, [String], String, String ) -> Name2Snippet
parse2Name2Snippet ( w,x,y,z ) = 
    name2SnippetDefault { name2SnippetName = w 
                    , name2SnippetSnippets = x
                    , name2SnippetUnparsed = y 
                    , name2SnippetOrder = z } 

--    f p17 
--    initial creation of Name 2 Phrase 
name2Snippet2Name2Phrase :: Name2Snippet -> Name2Phrase 
name2Snippet2Name2Phrase n2s 
    = name2PhraseDefault { 
          name2PhraseName = nm 
        , name2PhraseSnippets = ss 
        , name2PhraseUnparsed = us 
        , name2PhraseIsNotOracleName = ( not.isOracleUpperCaseName ) nm 
--    check that the parsed snippets equal original string 
        , name2PhraseIsAllParsed = nm == concat ss 
--    check that the parsed + unparsed snippets equal original string 
        , name2PhraseNoMissingStrings = nm == concat ss ++ us 
        } 
        where 
            nm = name2SnippetName n2s 
            ss = name2SnippetSnippets n2s 
            us = name2SnippetUnparsed n2s 
 
--    f p18 
--    determines the frequency of each string ( name )  
--    listFrequency :: Ord a => [a] -> [( a,Int )]; also does a sort 
getNameFreq :: [String] -> M.Map String Int 
getNameFreq allNameList 
    = M.fromAscList ( listFrequency allNameList ) 
    
--    f p19 
--    look up to get Freq 
addNameFreq2Name2Phrase :: M.Map String Int -> Name2Phrase -> Name2Phrase 
addNameFreq2Name2Phrase n2fLU n2p 
    = n2p { name2PhraseNameFrequency = fromMaybe 0 ( M.lookup ( name2PhraseName n2p ) n2fLU ) } 
    
--    f p1A 
--    uses Map k v for efficiency 
--    M.lookup has O( log n ), so this should work faster. 
--    Version 0 without [Phrase] could handle 3,500 phrases with 2,800 names in 1 minutes. File size = 0.6 MB = 90 times 
--    Version 1 without Data.Map could not handle 3,500 phrases with 2,800 names in 70 minutes 
--    Version 2 with Data.Map was inefficient: handles 3,500 phrases with 2,800 names in 140 minutes. File size = 54 MB 
--    Version 3 with simple Phrase handles 3,500 phrases with 2,800 names in 3 minutes. File size = 1.2 MB = 45 times 
--    Version 4 with all Maps handles 3,500 phrases with 2,800 names in 4 minutes. File size = 1.2 MB = 45 times. slightly slower, but still OK, as it should scale better.
--    add filter for some strings    
addPhrase2Name2PhraseMap :: M.Map String Phrase -> Name2Phrase -> Name2Phrase 
addPhrase2Name2PhraseMap phraseMapLU n2p 
    = n2p { 
          name2PhrasePhraseList = ps 
        , name2PhraseNoBaseMW = and ( map ( ( /= BaseMW ).phraseMultiWordType ) ps  ) 
        , name2PhraseBaseMWList = sort ( nub ( map phrasePhrase ( filter ( ( == BaseMW ).phraseMultiWordType ) ps ) ) ) 
        , name2PhraseNoInstanceMW =  and ( map ( ( /= InstanceMW ).phraseMultiWordType ) ps  ) 
        , name2PhraseInstanceMWList = sort ( nub ( map phrasePhrase ( filter ( ( == InstanceMW ).phraseMultiWordType ) ps ) ) ) 
        , name2PhraseUsesPlural = or ( map ( ( == Plural ).phrasePhraseType ) ps  ) 
        , name2PhraseUsesPastTense = or ( map ( ( == PastTense ).phrasePhraseType ) ps  ) 
        , name2PhraseUsesAcronym = or ( map ( ( == Acronym ).phrasePhraseType ) ps  ) 
        , name2PhraseUsesLetter = or ( map ( ( == Letter ).phrasePhraseType ) ps  )         
        , name2PhraseSnippetsFinal = ss 
        , name2PhraseHasBlank = or ( map ( == "" ) ss )  
        , name2PhrasePhrases = ss 
        , name2PhraseCamelWithSep = intercalate " " ss
        , name2PhraseUpperWithSep = map toUpper ( intercalate "_" ss ) 
        , name2PhraseCamelCase = concat ss 
        , name2PhraseUpperNoSep = map toUpper ( concat ss ) 
        , name2PhraseWordCount = length ss
        , name2PhraseSingleCharWordCount = length ( filter ( ( == 1 ).length ) ss )  
        , name2PhraseDoubleCharWordCount = length ( filter ( ( == 2 ).length ) ss ) 
        , name2PhraseNoSingleD = and ( map ( /= "D" ) ss ) 
        , name2PhraseNoSingleS = and ( map ( /= "S" ) ss ) 
        , name2PhraseSnippetsExpanded = sx 
        , name2PhraseNoExpansion = length ( concat ss ) == length ( concat sx ) 
--    filter out underscores and turn into upper case to compare 
        , name2PhraseNoMissingPhrases = map toUpper ( concat ss ) == map toUpper ( filter ( /= '_' ) ( name2PhraseName n2p ) ) 
      }
        where 
            ps = map ( \w -> ( fromMaybe phraseDefault ( M.lookup w phraseMapLU ) ) ) ( name2PhraseSnippets n2p ) 
            s1 = map phrase2Strings ps 
            se = map phrase2StringsE ps 
            fixS xs = filter ( /= "" ) ( mapBoth concatDigits ( concat xs ) ) 
            ss = fixS s1 
            sx = fixS se 
            
--    reduce size of Name2Phrase to the essentials 
name2Phrase2Min :: Name2Phrase -> Name2PhraseMin 
name2Phrase2Min n 
    = name2PhraseMinDefault {
              name2PhraseMinName = name2PhraseName n
            , name2PhraseMinSnippets = name2PhraseSnippets n
            , name2PhraseMinUnparsed = name2PhraseUnparsed n
    } 

--    if this is true then the parse needs manual examination 
maybeParseIssue :: Name2Phrase -> Bool 
maybeParseIssue n = not ( name2PhraseIsAllParsed n ) || name2PhraseUsesLetter n 

--    final publishing N2P 
name2Phrase2Out :: Name2Phrase -> Name2PhraseOut 
name2Phrase2Out n 
    = name2PhraseOutDefault {
              name2PhraseOutName = name2PhraseName n
            , name2PhraseOutSnippetsFinal = name2PhraseSnippetsFinal n
    } 
    
--    final publishing P2N     
phrase2Name2Out :: Phrase2Name -> Phrase2NameOut 
phrase2Name2Out p 
    = phrase2NameOutDefault { 
              phrase2NameOutPhraseName = phrase2NamePhraseName p 
            , phrase2NameOutPhraseType = ( phrasePhraseType.phrase2NamePhrase ) p 
            , phrase2NameOutExpansion = ( phraseExpansion.phrase2NamePhrase ) p 
            , phrase2NameOutDomain = ( phraseDomain.phrase2NamePhrase ) p 
            , phrase2NameOutNameCount = phrase2NameNameCount p 
            , phrase2NameOutUsedInNames = phrase2NameUsedInNames p 
    } 

-------------------------- 
--    secondary functions 
--    f p20 
--    get phrase frequency 
--    this determines the number of times a phrase is used in the name list
--    It is useful to determining phrase usage. 
getPhraseFreq :: [Name2Phrase] -> M.Map String Int  
getPhraseFreq name2PhraseList 
    = M.fromAscList ( listFrequency ( concatMap name2PhrasePhrases name2PhraseList ) ) 
        
--    f p21 
--    adds Phrase Freq to Phrase 
--    handles 3,500 phrases with 2,800 names in 2 minutes. 
addPhraseFreq2Phrase ::  M.Map String Int -> Phrase -> Phrase  
addPhraseFreq2Phrase p2fLU w 
    = w { phraseUsedInNameCount = fromMaybe 0 ( M.lookup ( phrasePhrase w ) p2fLU ) } 
    
--    f p22 
--    invert Snippet 2 Phrases 
--    reorderTuples sorts by fst 
invertSnippet2Phrase :: [Snippet2Phrase] -> [Phrase2Snippet]   
invertSnippet2Phrase snippet2PhraseList 
    = map ( \( a,b ) -> phrase2SnippetDefault {phrase2SnippetPhrase = a, phrase2SnippetSnippets = b }  ) w2sList 
        where w2sList = reorderTuples "" "" ( map ( \s -> ( snippet2PhraseSnippet s, [snippet2PhrasePhraseName s] ) ) snippet2PhraseList ) 
        
--    f p23 
--    inverts name snippets 
--    reorderTuples :: ( Ord a, Ord b ) => a -> b -> [( a,[b] )] -> [( b,[a] )] 
--    can be made more efficient 
--    useful for isomorphism testing 
invertSnippet2Name :: [Name2Snippet] -> [Snippet2Name] 
invertSnippet2Name name2SnippetList 
    = map stringTuple2Snippet2Name s2nList 
        where s2nList = ( reorderTuples "" "" ( map ( \w -> ( name2SnippetName w, name2SnippetSnippets w ) ) name2SnippetList ) ) 
            
--    f p24 
--    invert Name 2 Phrases 
--    reorderTuples sorts by fst 
invertName2Phrase :: [Name2Phrase] -> M.Map String [String] 
invertName2Phrase name2PhraseList 
    = M.fromAscList w2nList 
        where w2nList = reorderTuples "" "" ( map ( \n -> ( name2PhraseName n, name2PhrasePhrases n ) ) name2PhraseList ) 
        
--    f p25 
--    build Phrase2Name from Phrase  
phrase2Phrase2Name :: Phrase -> Phrase2Name 
phrase2Phrase2Name p 
    = phrase2NameDefault { 
          phrase2NamePhraseName = phrasePhrase p 
        , phrase2NamePhrase = p 
        }  

--    f p26 
--    add Snippets 2 Phrase2Name 
--    change list to Map, and build Map before call 
addSnippets2Phrase2Name :: M.Map String [String] -> Phrase2Name -> Phrase2Name 
addSnippets2Phrase2Name pw2sLU p2n  
    =  p2n { 
          phrase2NameSnippets = fromMaybe [""] ( M.lookup ( phrase2NamePhraseName p2n ) pw2sLU ) } 
          
--    f p27 
--    add Names for each Phrase  
--    current running time on 200,000 names is 2 hours. Does not help to throttle using take 200 
addNames2Phrase2Name :: M.Map String [String] -> Phrase2Name -> Phrase2Name 
addNames2Phrase2Name p2nLU p2n 
    =  p2n { 
          phrase2NameUsedInNames = w2nNames 
        , phrase2NameNameCount = length ( filter ( /= "" ) w2nNames ) } 
            where w2nNames = fromMaybe [""] ( M.lookup ( phrase2NamePhraseName p2n ) p2nLU ) 

--------------------------
--    public but not used in Glossary
--    f p30    
--    uses phrase to create a readable definition. 
--    used in Column and Table , phraseAuthority , phraseDefinition and phraseDomain 
--    There will be a need to expand some full phrases if they belong to a domain that is not common. 
definePhrase :: Phrase -> String 
definePhrase p 
--    no need to expand these 
    | elem pt [AllPhrase,Number,MultipleWords] = "" 
--    acronyms and contractions need to be expanded 
    | elem pt [Acronym,Contraction] = lp ++ " expands to " ++ px ++ ". " 
--    expand company names 
    | pt == ProperNoun = lp ++ " is " ++ px ++ ". " 
--    just a Letter 
    | pt == Letter = lp ++ " is " ++ px ++ ". " 
    | otherwise = ""
    where 
        pt = phrasePhraseType p
        lp = phrasePhrase p 
        px = phraseExpansion p
  
----------------------------------------------------------------------
--    hidden glossary functions  
--    f h1  
--    Initialises S2P data to allow parsing 
snippet2PhraseIn2Snippet2Phrase :: M.Map String Phrase -> Snippet2PhraseIn -> Snippet2Phrase 
snippet2PhraseIn2Snippet2Phrase s2pLU s2pi 
    = snippet2PhraseDefault { 
          snippet2PhraseSnippet = sn 
        , snippet2PhraseSnippetIn = sn 
        , snippet2PhrasePhraseName = snippet2PhraseInPhrase s2pi 
        , snippet2PhrasePhraseType = phrasePhraseType ( fromMaybe phraseDefault ( M.lookup sn s2pLU ) ) 
        , snippet2PhraseIsInPhrase = True } 
        where sn = snippet2PhraseInSnippet s2pi 
        
--    f h2 
--    appends underscore to create an additional phrase for parsing  
appendUnderscoreSnippet2Phrase :: Snippet2Phrase -> Snippet2Phrase 
appendUnderscoreSnippet2Phrase s2p 
    = s2p { snippet2PhraseSnippet = concat ["_", snippet2PhraseSnippet s2p], snippet2PhraseIsInPhrase = False } 
    
--    f h2 
--    appends space to create an additional phrase for parsing  
appendSpaceSnippet2Phrase :: Snippet2Phrase -> Snippet2Phrase 
appendSpaceSnippet2Phrase s2p 
    = s2p { snippet2PhraseSnippet = concat [" ", snippet2PhraseSnippet s2p], snippet2PhraseIsInPhrase = False } 

--    f h3            
--    generic function to populate specific data type 
stringTuple2Snippet2Name :: ( String, [String] ) -> Snippet2Name
stringTuple2Snippet2Name ( a,bs ) = 
    snippet2NameDefault { snippet2NameSnippet = a 
                    , snippet2NameNames = bs } 

--    f h4 
--    use phrasePhraseType MultipleWords to either use the current snippet or replace 
phrase2Strings :: Phrase -> [String] 
phrase2Strings p 
--    discard things like "__" 
    | phraseDiscard p = [] 
--    expand out multi words 
    | phrasePhraseType p == MultipleWords = phraseCorrectWords p 
    | otherwise = [phrasePhrase p] 
    
--    f h5 
--    same as phrase2Strings, but with expansion as well... 
phrase2StringsE :: Phrase -> [String] 
phrase2StringsE p 
--    discard things like "__" 
    | phraseDiscard p = [] 
--    expand out multi words 
    | phrasePhraseType p == MultipleWords = phraseCorrectWords p 
--    expand out contractions 
    | phrasePhraseType p == Contraction = [phraseExpansion p] 
    | otherwise = [phrasePhrase p] 
    
--    f h6 
--    concats digit pairs only  
concatDigits :: String -> String -> ( String,String )
concatDigits x y  
    | all isDigit x && all isDigit y = ( "", x ++ y ) 
    | otherwise = ( x, y ) 
  
--    f h10
--    initialises phrase from S2P for testing purposes 
--    not used 
snippet2Phrase2PhraseIn :: Snippet2Phrase -> PhraseIn  
snippet2Phrase2PhraseIn s2p 
    = phraseInDefault { 
      phraseInPhrase = snippet2PhrasePhraseName s2p 
    , phraseInExpansion = if elem wt [Acronym,Contraction] then snippet2PhraseSnippet s2p  else "" 
    , phraseInPhraseType = wt } 
    where wt = snippet2PhrasePhraseType s2p 
