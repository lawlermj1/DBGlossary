{-# LANGUAGE DeriveDataTypeable #-} 
{- |
   File        : Glossary 
   Description : Parses phrases from a names set, using discovered snippets 
   Copyright   : ( c ) Matthew Lawler 2018 
   License     : 2-Clause BSD
   Maintainer  : lawlermj1@gmail.com 

   This file shows how to use the GlossaryBP functions to parse out phrases from a set of names, typically DB column names. 
   
   It assumes 5 inputs:
   Snippet2PhraseIn:- relates a snippet to a phrase e.g. the snippet 'WORK' is related to phrase 'Work'. 
   PhraseIn:- contains information about a phrase. e.g. the phrase 'WIP' is an Acronym, which stands for 'Work In Progress' 
   ColumnName:- contains all names needed for parsing 
   Authority:- contains information about an authority 
   Domain:- contains information about an domain  
   
   It produces 2 main outputs: 
   Name2PhraseOut:- relates an input name to its component phrases ( or words ) in a String List 
   Phrase2NameOut:- relates an input phrase to its usage in all input names  
   
   Various intermediate outputs can also be produced until the snippets and phrases are fully trained in all the underlying words used in the full name set. 

 -}

-- Prelude modules 
import Data.List 

-- Custom built modules 
import DBCommon.Boilerplate  
import DBCommon.PreludeGapFiller 
import DBCommon.EnumType  
import DBCommon.IOCommon  

import DBGlossary.EnumType 
import DBGlossary.GlossaryBP  

------------------------------------
--    current input default 
testTheData = True    

--------------------------------------------------------------

main = do
--    some parms 
    putStrLn $ " Glossary " 

-------------------------  
--    INPUTS 
--    NEW pattern #5 
--    read and parse the file 
    authorityResults <- readWrapper parmInputTSVFile "common.in.Authority.csv" authorityDefault authorityLoad     
--    display any Parse errors     
    putStrLn $ " Authority Parse error: " ++ ( readResultErrorParse authorityResults )     
--    display any typing errors         
    putStrLn $ errors2String ( undefined::Authority ) ( readResultTypeErrors authorityResults )  
--    collect for print out, if needed 
    let toPrintList = [listToPrint TSV ( readResultType authorityResults )] ++ [errorsToPrint ( undefined::Authority ) ( readResultTypeErrors authorityResults )]  
    
--    read and parse the file 
    domainResults <- readWrapper parmInputTSVFile "common.in.Domain.csv" domainDefault domainLoad     
--    display any Parse errors     
    putStrLn $ " Domain Parse error: " ++ ( readResultErrorParse domainResults )     
--    display any typing errors         
    putStrLn $ errors2String ( undefined::Domain ) ( readResultTypeErrors domainResults )  
--    collect for print out, if needed 
    let toPrintList2 = toPrintList ++ [listToPrint TSV ( readResultType domainResults )] ++ [errorsToPrint ( undefined::Domain ) ( readResultTypeErrors domainResults )] 
    
--    read and parse the file 
    snippet2PhraseInResults <- readWrapper parmInputTSVFile "common.in.Snippet2PhraseIn.csv" snippet2PhraseInDefault snippet2PhraseInLoad     
--    display any Parse errors     
    putStrLn $ " Snippet2PhraseIn Parse error: " ++ ( readResultErrorParse snippet2PhraseInResults )     
--    display any typing errors         
    putStrLn $ errors2String ( undefined::Snippet2PhraseIn ) ( readResultTypeErrors snippet2PhraseInResults )  
--    collect for print out, if needed 
    let toPrintList3 = toPrintList2 ++ [listToPrint TSV ( readResultType snippet2PhraseInResults )] ++ [errorsToPrint ( undefined::Snippet2PhraseIn ) ( readResultTypeErrors snippet2PhraseInResults )] 
    let snippet2PhraseInTypeList = readResultType snippet2PhraseInResults 
    
--    read and parse the file 
    phraseInResults <- readWrapper parmInputTSVFile "common.in.PhraseIn.csv" phraseInDefault phraseInLoad     
--    display any Parse errors     
    putStrLn $ " PhraseIn Parse error: " ++ ( readResultErrorParse phraseInResults )     
--    display any typing errors         
    putStrLn $ errors2String ( undefined::PhraseIn ) ( readResultTypeErrors phraseInResults )  
--    collect for print out, if needed 
    let toPrintList4 = toPrintList3 ++ [listToPrint TSV ( readResultType phraseInResults )] ++ [errorsToPrint ( undefined::PhraseIn ) ( readResultTypeErrors phraseInResults )] 
    let phraseInTypeList = readResultType phraseInResults 

--    read and parse the file 
    columnNameResults <- readWrapper parmInputTSVFile "common.in.ColumnName.csv" columnNameDefault columnNameLoad     
--    display any Parse errors     
    putStrLn $ " ColumnName Parse error: " ++ ( readResultErrorParse columnNameResults )     
--    display any typing errors         
    putStrLn $ errors2String ( undefined::ColumnName ) ( readResultTypeErrors columnNameResults )  
--    collect for print out, if needed 
    let toPrintList5 = toPrintList2 ++ [listToPrint TSV ( readResultType columnNameResults )] ++ [errorsToPrint ( undefined::ColumnName ) ( readResultTypeErrors columnNameResults )]  

--------------------------------------------------------------------
--    Some checks 
    
--    get all names in a single list 
    let allNameList = concatMap ( \c -> [columnNameSchema c, columnNameTableName c, columnNameColumnName c] ) ( readResultType columnNameResults ) 
    
--    check for duplicate phrases 
    let dupPhraseIn = getDups ( map phraseInPhrase phraseInTypeList )
--    simple test 
    if testTheData then putStrLn $ " duplicate PhraseIn: " ++ intercalate " - " dupPhraseIn else putStrLn $ " No duplicate phraseIn test " 

--    check for duplicate snippets  
    let dupS2PIn = getDups ( map snippet2PhraseInSnippet snippet2PhraseInTypeList )   
--    simple test 
    if testTheData then putStrLn $ " duplicate S2PIn: " ++ intercalate " - " dupS2PIn else putStrLn $ " No duplicate S2PIn test " 
        
--    check for missing phrases 
    let extraPhraseIn = phraseNotInS2PIn phraseInTypeList snippet2PhraseInTypeList 
--    simple test 
    if testTheData then putStrLn $ " phraseIn not in S2PIn: " ++ intercalate " - " ( map phraseInPhrase extraPhraseIn ) else putStrLn $ " No phraseIn in S2PIn test " 
    
--    check for missing S2Ps  
    let extraS2PIn = s2PNotInPhraseIn snippet2PhraseInTypeList phraseInTypeList 
--    simple test 
    if testTheData then putStrLn $ " S2PIn not in phraseIn: " ++ intercalate " - " ( nub ( map snippet2PhraseInPhrase extraS2PIn ) ) else putStrLn $ " No S2PIn in phraseIn test " 

--------------------------------------------------------------------
--    GLOSSARY or WORD DISCOVERY or PARSING 
    
--    get phrase list 
    let phraseList = map phraseIn2Phrase phraseInTypeList 
--    let toPrintListX = [listToPrint TSV phraseList] 
--    construct a Data.Map lookup 
    let phrase2MapLU = phrase2Map phraseList 
--    filter the s2P by removing ignores  
    let snippet2PhraseInTypeList1 = filter ( filterS2PIn phrase2MapLU ) snippet2PhraseInTypeList 
--    let toPrintListX = [listToPrint TSV snippet2PhraseInTypeList1] 
    
--    input snippet 2 Phrase 
--    takes 20 minutes to print 12,405 S2Ps 
    let snippet2PhraseList = map ( phrase2S2P phrase2MapLU ) ( getSnippet2Phrase phrase2MapLU snippet2PhraseInTypeList1 ) 
--   let toPrintListX = [listToPrint TSV snippet2PhraseList]     

--    prepare a Parsec tries list for all phrases 
    let trieList = makeTries ( map snippet2PhraseSnippet snippet2PhraseList ) 
    
--    parse all names into snippets  
--    handles 2,400 phrases with 27,000 names in 7 minutes 
--    handles 2,800 phrases with 27,000 names in 13 minutes  
--    could not handle 4,400 phrases with 200,000 names in < 5 hours 
    let name2SnippetList = map ( parse2Name2Snippet.( name2Snippets trieList ) ) ( nub allNameList ) 
--   let toPrintListX = [listToPrint TSV name2SnippetList]     

--    frequency of all names - useful for discovering boilerplates 
    let n2fLU = getNameFreq allNameList 
--    simple test     
--    putStrLn $ intercalate " " ( map show ( M.elems n2fLU ) ) 
--    lookup for snippet 2 Phrase 
    let s2pLU = snippet2PhraseMap snippet2PhraseList 
--    create the N2P with the N2S list 
    let name2PhraseList = ( map ( ( addPhrase2Name2PhraseMap s2pLU ).( addNameFreq2Name2Phrase n2fLU ).name2Snippet2Name2Phrase ) name2SnippetList )   
--    check for unparsed words  
--   let toPrintListX = [listToPrint TSV ( map name2Phrase2Min ( filter maybeParseIssue name2PhraseList ) )]     
--    final publication ( this is a bit inefficient with the memory  ) 
    let name2PhraseListOut = map name2Phrase2Out name2PhraseList 
    
--    creates name2PhraseListLU 
    let name2PhraseListLU = name2PhraseMap name2PhraseList 

--------------------------
--    secondary lists  
--    Get Phrase frequency usage across all names  
    let w2fLU = getPhraseFreq name2PhraseList   
--    add to phrase final 
    let phraseTypeList = map ( addPhraseFreq2Phrase w2fLU ) phraseList 
--   let toPrintListX = [listToPrint TSV phraseTypeList]     

--    invert snippet2PhraseList
    let phrase2SnippetList = invertSnippet2Phrase snippet2PhraseList 
--   let toPrintListX = [listToPrint TSV phrase2SnippetList]     

--    invert name2SnippetList 
    let snippet2NameList = invertSnippet2Name name2SnippetList 
--   let toPrintListX = [listToPrint TSV snippet2NameList]         

--    Build phrase 2 names 
    let pw2sLU = phrase2SnippetMap phrase2SnippetList 
    let p2nLU = invertName2Phrase name2PhraseList   
--    handles 3,500 phrases with 2,800 names in 3 minutes 
--    Acronyms only ( filter ( ( == Acronym ).phrasePhraseType ) 
--    parses 1,600 Acronyms used in 200,000 names in ?? minutes 
    let phrase2NameListOut = map phrase2Name2Out ( filter ( ( /= 0 ).phrase2NameNameCount ) ( map ( ( addNames2Phrase2Name p2nLU ).( addSnippets2Phrase2Name pw2sLU ).phrase2Phrase2Name ) ( filter ( ( == Acronym ).phrasePhraseType ) phraseList ) ) )  

-- OUTPUT ------------------------------------------------------------------

--   for detailed analysis 
--    let toPrintListX = [listToPrint TSV name2PhraseList] ++ [listToPrint TSV phrase2NameListOut]

--    for final publication 
    let toPrintListX = [listToPrint TSV name2PhraseListOut] ++ [listToPrint TSV phrase2NameListOut]

--    print multiple files toPrintListX 
--    mapM_ ( \x -> printFile ( toPrintFileName x ) ( toPrintContents x ) ) ( filter ( /= toPrintDefault ) toPrintList5 ) 
    mapM_ ( \x -> printFile ( toPrintFileName x ) ( toPrintContents x ) ) toPrintListX 

   
