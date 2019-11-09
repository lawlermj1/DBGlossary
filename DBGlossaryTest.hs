{-# LANGUAGE DeriveDataTypeable #-} 
{- |
   File        : DBGlossaryTest 
   Description : Property test of all GlossaryBP type boilerplate functions 
   Copyright   : ( c ) Matthew Lawler 2018 
   License     : 2-Clause BSD
   Maintainer  : lawlermj1@gmail.com 

   This file is a test of the boilerplate functions generated from the Glossary types defined in GlossaryBP. 
   It also performs property testing on the generated Boilerplate functions for all Glossary types. 
   It also reads a simple csv, checks the strings, loads into types and outputs the result. 

 -}
 module Main where 
-- Prelude modules  
-- import Data.List 

-- Custom built modules 
import DBCommon.Boilerplate 
-- import DBCommon.PreludeGapFiller 
import DBCommon.EnumType 
import DBCommon.IOCommon 

-- import DBGlossary.EnumType 
import DBGlossary.GlossaryBP 

------------------------------------- 
main :: IO () 
main = do
-- 
    putStrLn $ " DBGlossaryTest " 

    putStrLn $ "------- Primary Boilerplate functions  -------------------- "    
    putStrLn $ "------- Expected Result for all tests is: No crashes ------------- "    
    
    putStrLn $ "gheaderShowQ (  gdefaultU ( undefined::" ++ sepCharS ++ "Authority" ++ sepCharS ++ gheaderShowQ (  gdefaultU ( undefined::Authority ) )
    putStrLn $ "show (  gdefaultU ( undefined::" ++ sepCharS ++ "Authority" ++ sepCharS ++ show (  gdefaultU ( undefined::Authority ) )
    putStrLn $ "show (  gloadU ( undefined::" ++ sepCharS ++ "Authority" ++ sepCharS ++ show (  gloadU ( undefined::Authority ) ( gdSL ( undefined::Authority ) ) )
    putStrLn $ "gheaderShowQ (  gdefaultU ( undefined::" ++ sepCharS ++ "ColumnName" ++ sepCharS ++ gheaderShowQ (  gdefaultU ( undefined::ColumnName ) )
    putStrLn $ "show (  gdefaultU ( undefined::" ++ sepCharS ++ "ColumnName" ++ sepCharS ++ show (  gdefaultU ( undefined::ColumnName ) )
    putStrLn $ "show (  gloadU ( undefined::" ++ sepCharS ++ "ColumnName" ++ sepCharS ++ show (  gloadU ( undefined::ColumnName ) ( gdSL ( undefined::ColumnName ) ) )
    putStrLn $ "gheaderShowQ (  gdefaultU ( undefined::" ++ sepCharS ++ "Domain" ++ sepCharS ++ gheaderShowQ (  gdefaultU ( undefined::Domain ) )
    putStrLn $ "show (  gdefaultU ( undefined::" ++ sepCharS ++ "Domain" ++ sepCharS ++ show (  gdefaultU ( undefined::Domain ) )
    putStrLn $ "show (  gloadU ( undefined::" ++ sepCharS ++ "Domain" ++ sepCharS ++ show (  gloadU ( undefined::Domain ) ( gdSL ( undefined::Domain ) ) )
    putStrLn $ "gheaderShowQ (  gdefaultU ( undefined::" ++ sepCharS ++ "Name2Phrase" ++ sepCharS ++ gheaderShowQ (  gdefaultU ( undefined::Name2Phrase ) )
    putStrLn $ "show (  gdefaultU ( undefined::" ++ sepCharS ++ "Name2Phrase" ++ sepCharS ++ show (  gdefaultU ( undefined::Name2Phrase ) )
    putStrLn $ "show (  gloadU ( undefined::" ++ sepCharS ++ "Name2Phrase" ++ sepCharS ++ show (  gloadU ( undefined::Name2Phrase ) ( gdSL ( undefined::Name2Phrase ) ) )
    putStrLn $ "gheaderShowQ (  gdefaultU ( undefined::" ++ sepCharS ++ "Name2PhraseMin" ++ sepCharS ++ gheaderShowQ (  gdefaultU ( undefined::Name2PhraseMin ) )
    putStrLn $ "show (  gdefaultU ( undefined::" ++ sepCharS ++ "Name2PhraseMin" ++ sepCharS ++ show (  gdefaultU ( undefined::Name2PhraseMin ) )
    putStrLn $ "show (  gloadU ( undefined::" ++ sepCharS ++ "Name2PhraseMin" ++ sepCharS ++ show (  gloadU ( undefined::Name2PhraseMin ) ( gdSL ( undefined::Name2PhraseMin ) ) )
    putStrLn $ "gheaderShowQ (  gdefaultU ( undefined::" ++ sepCharS ++ "Name2PhraseOut" ++ sepCharS ++ gheaderShowQ (  gdefaultU ( undefined::Name2PhraseOut ) )
    putStrLn $ "show (  gdefaultU ( undefined::" ++ sepCharS ++ "Name2PhraseOut" ++ sepCharS ++ show (  gdefaultU ( undefined::Name2PhraseOut ) )
    putStrLn $ "show (  gloadU ( undefined::" ++ sepCharS ++ "Name2PhraseOut" ++ sepCharS ++ show (  gloadU ( undefined::Name2PhraseOut ) ( gdSL ( undefined::Name2PhraseOut ) ) )
    putStrLn $ "gheaderShowQ (  gdefaultU ( undefined::" ++ sepCharS ++ "Name2Snippet" ++ sepCharS ++ gheaderShowQ (  gdefaultU ( undefined::Name2Snippet ) )
    putStrLn $ "show (  gdefaultU ( undefined::" ++ sepCharS ++ "Name2Snippet" ++ sepCharS ++ show (  gdefaultU ( undefined::Name2Snippet ) )
    putStrLn $ "show (  gloadU ( undefined::" ++ sepCharS ++ "Name2Snippet" ++ sepCharS ++ show (  gloadU ( undefined::Name2Snippet ) ( gdSL ( undefined::Name2Snippet ) ) )
    putStrLn $ "gheaderShowQ (  gdefaultU ( undefined::" ++ sepCharS ++ "Phrase" ++ sepCharS ++ gheaderShowQ (  gdefaultU ( undefined::Phrase ) )
    putStrLn $ "show (  gdefaultU ( undefined::" ++ sepCharS ++ "Phrase" ++ sepCharS ++ show (  gdefaultU ( undefined::Phrase ) )
    putStrLn $ "show (  gloadU ( undefined::" ++ sepCharS ++ "Phrase" ++ sepCharS ++ show (  gloadU ( undefined::Phrase ) ( gdSL ( undefined::Phrase ) ) )
    putStrLn $ "gheaderShowQ (  gdefaultU ( undefined::" ++ sepCharS ++ "Phrase2Name" ++ sepCharS ++ gheaderShowQ (  gdefaultU ( undefined::Phrase2Name ) )
    putStrLn $ "show (  gdefaultU ( undefined::" ++ sepCharS ++ "Phrase2Name" ++ sepCharS ++ show (  gdefaultU ( undefined::Phrase2Name ) )
    putStrLn $ "show (  gloadU ( undefined::" ++ sepCharS ++ "Phrase2Name" ++ sepCharS ++ show (  gloadU ( undefined::Phrase2Name ) ( gdSL ( undefined::Phrase2Name ) ) )
    putStrLn $ "gheaderShowQ (  gdefaultU ( undefined::" ++ sepCharS ++ "Phrase2NameOut" ++ sepCharS ++ gheaderShowQ (  gdefaultU ( undefined::Phrase2NameOut ) )
    putStrLn $ "show (  gdefaultU ( undefined::" ++ sepCharS ++ "Phrase2NameOut" ++ sepCharS ++ show (  gdefaultU ( undefined::Phrase2NameOut ) )
    putStrLn $ "show (  gloadU ( undefined::" ++ sepCharS ++ "Phrase2NameOut" ++ sepCharS ++ show (  gloadU ( undefined::Phrase2NameOut ) ( gdSL ( undefined::Phrase2NameOut ) ) )
    putStrLn $ "gheaderShowQ (  gdefaultU ( undefined::" ++ sepCharS ++ "Phrase2Snippet" ++ sepCharS ++ gheaderShowQ (  gdefaultU ( undefined::Phrase2Snippet ) )
    putStrLn $ "show (  gdefaultU ( undefined::" ++ sepCharS ++ "Phrase2Snippet" ++ sepCharS ++ show (  gdefaultU ( undefined::Phrase2Snippet ) )
    putStrLn $ "show (  gloadU ( undefined::" ++ sepCharS ++ "Phrase2Snippet" ++ sepCharS ++ show (  gloadU ( undefined::Phrase2Snippet ) ( gdSL ( undefined::Phrase2Snippet ) ) )
    putStrLn $ "gheaderShowQ (  gdefaultU ( undefined::" ++ sepCharS ++ "PhraseIn" ++ sepCharS ++ gheaderShowQ (  gdefaultU ( undefined::PhraseIn ) )
    putStrLn $ "show (  gdefaultU ( undefined::" ++ sepCharS ++ "PhraseIn" ++ sepCharS ++ show (  gdefaultU ( undefined::PhraseIn ) )
    putStrLn $ "show (  gloadU ( undefined::" ++ sepCharS ++ "PhraseIn" ++ sepCharS ++ show (  gloadU ( undefined::PhraseIn ) ( gdSL ( undefined::PhraseIn ) ) )
    putStrLn $ "gheaderShowQ (  gdefaultU ( undefined::" ++ sepCharS ++ "Snippet2Name" ++ sepCharS ++ gheaderShowQ (  gdefaultU ( undefined::Snippet2Name ) )
    putStrLn $ "show (  gdefaultU ( undefined::" ++ sepCharS ++ "Snippet2Name" ++ sepCharS ++ show (  gdefaultU ( undefined::Snippet2Name ) )
    putStrLn $ "show (  gloadU ( undefined::" ++ sepCharS ++ "Snippet2Name" ++ sepCharS ++ show (  gloadU ( undefined::Snippet2Name ) ( gdSL ( undefined::Snippet2Name ) ) )
    putStrLn $ "gheaderShowQ (  gdefaultU ( undefined::" ++ sepCharS ++ "Snippet2Phrase" ++ sepCharS ++ gheaderShowQ (  gdefaultU ( undefined::Snippet2Phrase ) )
    putStrLn $ "show (  gdefaultU ( undefined::" ++ sepCharS ++ "Snippet2Phrase" ++ sepCharS ++ show (  gdefaultU ( undefined::Snippet2Phrase ) )
    putStrLn $ "show (  gloadU ( undefined::" ++ sepCharS ++ "Snippet2Phrase" ++ sepCharS ++ show (  gloadU ( undefined::Snippet2Phrase ) ( gdSL ( undefined::Snippet2Phrase ) ) )
    putStrLn $ "gheaderShowQ (  gdefaultU ( undefined::" ++ sepCharS ++ "Snippet2PhraseIn" ++ sepCharS ++ gheaderShowQ (  gdefaultU ( undefined::Snippet2PhraseIn ) )
    putStrLn $ "show (  gdefaultU ( undefined::" ++ sepCharS ++ "Snippet2PhraseIn" ++ sepCharS ++ show (  gdefaultU ( undefined::Snippet2PhraseIn ) )
    putStrLn $ "show (  gloadU ( undefined::" ++ sepCharS ++ "Snippet2PhraseIn" ++ sepCharS ++ show (  gloadU ( undefined::Snippet2PhraseIn ) ( gdSL ( undefined::Snippet2PhraseIn ) ) )

    putStrLn $ "------- Extra Unused Boilerplate functions -------------------- " 
    putStrLn $ "gheaderShowD phrase2SnippetDefault = " ++ sepCharS ++ show ( gheaderShowD phrase2SnippetDefault ) 
    putStrLn $ "gheaderFieldD phrase2SnippetDefault = " ++ sepCharS ++ show ( gheaderFieldD phrase2SnippetDefault ) 
    putStrLn $ "gunloadD phrase2SnippetDefault = " ++ sepCharS ++ show ( gunloadD ( guldU ( undefined::Phrase2Snippet ) ) phrase2SnippetDefault ) 
    putStrLn $ "gloadD phrase2SnippetDefault ( gdSL ( undefined::Phrase2Snippet ) ) = " ++ sepCharS ++ show (  gloadD phrase2SnippetDefault ( gdSL ( undefined::Phrase2Snippet ) )  ) 

    
    putStrLn $ "------- Equivalent Function Properties --- " 
    putStrLn $ "------- Expected Result for all tests is: True -------------------- "     
 
    putStrLn $ "show (  prop_gADTD2List_eq_gADTU2List ( undefined::" ++ sepCharS ++ "Authority" ++ sepCharS ++ show (  prop_gADTD2List_eq_gADTU2List ( undefined::Authority ) )
    putStrLn $ "show (  prop_gADTD2Tree_eq_gADTU2Tree ( undefined::" ++ sepCharS ++ "Authority" ++ sepCharS ++ show (  prop_gADTD2Tree_eq_gADTU2Tree ( undefined::Authority ) )
    putStrLn $ "show (  prop_gADTD2Tree_eq_gADTU2Tree ( undefined::" ++ sepCharS ++ "Authority" ++ sepCharS ++ show (  prop_gADTD2Tree_eq_gADTU2Tree ( undefined::Authority ) )
    putStrLn $ "show (  prop_gldD_eq_gldU ( undefined::" ++ sepCharS ++ "Authority" ++ sepCharS ++ show (  prop_gldD_eq_gldU ( undefined::Authority ) )
    putStrLn $ "show (  prop_guldD_eq_guldU ( undefined::" ++ sepCharS ++ "Authority" ++ sepCharS ++ show (  prop_guldD_eq_guldU ( undefined::Authority ) )
    putStrLn $ "show (  prop_gADTD2List_eq_gADTU2List ( undefined::" ++ sepCharS ++ "ColumnName" ++ sepCharS ++ show (  prop_gADTD2List_eq_gADTU2List ( undefined::ColumnName ) )
    putStrLn $ "show (  prop_gADTD2Tree_eq_gADTU2Tree ( undefined::" ++ sepCharS ++ "ColumnName" ++ sepCharS ++ show (  prop_gADTD2Tree_eq_gADTU2Tree ( undefined::ColumnName ) )
    putStrLn $ "show (  prop_gADTD2Tree_eq_gADTU2Tree ( undefined::" ++ sepCharS ++ "ColumnName" ++ sepCharS ++ show (  prop_gADTD2Tree_eq_gADTU2Tree ( undefined::ColumnName ) )
    putStrLn $ "show (  prop_gldD_eq_gldU ( undefined::" ++ sepCharS ++ "ColumnName" ++ sepCharS ++ show (  prop_gldD_eq_gldU ( undefined::ColumnName ) )
    putStrLn $ "show (  prop_guldD_eq_guldU ( undefined::" ++ sepCharS ++ "ColumnName" ++ sepCharS ++ show (  prop_guldD_eq_guldU ( undefined::ColumnName ) )
    putStrLn $ "show (  prop_gADTD2List_eq_gADTU2List ( undefined::" ++ sepCharS ++ "Domain" ++ sepCharS ++ show (  prop_gADTD2List_eq_gADTU2List ( undefined::Domain ) )
    putStrLn $ "show (  prop_gADTD2Tree_eq_gADTU2Tree ( undefined::" ++ sepCharS ++ "Domain" ++ sepCharS ++ show (  prop_gADTD2Tree_eq_gADTU2Tree ( undefined::Domain ) )
    putStrLn $ "show (  prop_gADTD2Tree_eq_gADTU2Tree ( undefined::" ++ sepCharS ++ "Domain" ++ sepCharS ++ show (  prop_gADTD2Tree_eq_gADTU2Tree ( undefined::Domain ) )
    putStrLn $ "show (  prop_gldD_eq_gldU ( undefined::" ++ sepCharS ++ "Domain" ++ sepCharS ++ show (  prop_gldD_eq_gldU ( undefined::Domain ) )
    putStrLn $ "show (  prop_guldD_eq_guldU ( undefined::" ++ sepCharS ++ "Domain" ++ sepCharS ++ show (  prop_guldD_eq_guldU ( undefined::Domain ) )
    putStrLn $ "show (  prop_gADTD2List_eq_gADTU2List ( undefined::" ++ sepCharS ++ "Name2Phrase" ++ sepCharS ++ show (  prop_gADTD2List_eq_gADTU2List ( undefined::Name2Phrase ) )
    putStrLn $ "show (  prop_gADTD2Tree_eq_gADTU2Tree ( undefined::" ++ sepCharS ++ "Name2Phrase" ++ sepCharS ++ show (  prop_gADTD2Tree_eq_gADTU2Tree ( undefined::Name2Phrase ) )
    putStrLn $ "show (  prop_gADTD2Tree_eq_gADTU2Tree ( undefined::" ++ sepCharS ++ "Name2Phrase" ++ sepCharS ++ show (  prop_gADTD2Tree_eq_gADTU2Tree ( undefined::Name2Phrase ) )
    putStrLn $ "show (  prop_gldD_eq_gldU ( undefined::" ++ sepCharS ++ "Name2Phrase" ++ sepCharS ++ show (  prop_gldD_eq_gldU ( undefined::Name2Phrase ) )
    putStrLn $ "show (  prop_guldD_eq_guldU ( undefined::" ++ sepCharS ++ "Name2Phrase" ++ sepCharS ++ show (  prop_guldD_eq_guldU ( undefined::Name2Phrase ) )
    putStrLn $ "show (  prop_gADTD2List_eq_gADTU2List ( undefined::" ++ sepCharS ++ "Name2PhraseMin" ++ sepCharS ++ show (  prop_gADTD2List_eq_gADTU2List ( undefined::Name2PhraseMin ) )
    putStrLn $ "show (  prop_gADTD2Tree_eq_gADTU2Tree ( undefined::" ++ sepCharS ++ "Name2PhraseMin" ++ sepCharS ++ show (  prop_gADTD2Tree_eq_gADTU2Tree ( undefined::Name2PhraseMin ) )
    putStrLn $ "show (  prop_gADTD2Tree_eq_gADTU2Tree ( undefined::" ++ sepCharS ++ "Name2PhraseMin" ++ sepCharS ++ show (  prop_gADTD2Tree_eq_gADTU2Tree ( undefined::Name2PhraseMin ) )
    putStrLn $ "show (  prop_gldD_eq_gldU ( undefined::" ++ sepCharS ++ "Name2PhraseMin" ++ sepCharS ++ show (  prop_gldD_eq_gldU ( undefined::Name2PhraseMin ) )
    putStrLn $ "show (  prop_guldD_eq_guldU ( undefined::" ++ sepCharS ++ "Name2PhraseMin" ++ sepCharS ++ show (  prop_guldD_eq_guldU ( undefined::Name2PhraseMin ) )
    putStrLn $ "show (  prop_gADTD2List_eq_gADTU2List ( undefined::" ++ sepCharS ++ "Name2PhraseOut" ++ sepCharS ++ show (  prop_gADTD2List_eq_gADTU2List ( undefined::Name2PhraseOut ) )
    putStrLn $ "show (  prop_gADTD2Tree_eq_gADTU2Tree ( undefined::" ++ sepCharS ++ "Name2PhraseOut" ++ sepCharS ++ show (  prop_gADTD2Tree_eq_gADTU2Tree ( undefined::Name2PhraseOut ) )
    putStrLn $ "show (  prop_gADTD2Tree_eq_gADTU2Tree ( undefined::" ++ sepCharS ++ "Name2PhraseOut" ++ sepCharS ++ show (  prop_gADTD2Tree_eq_gADTU2Tree ( undefined::Name2PhraseOut ) )
    putStrLn $ "show (  prop_gldD_eq_gldU ( undefined::" ++ sepCharS ++ "Name2PhraseOut" ++ sepCharS ++ show (  prop_gldD_eq_gldU ( undefined::Name2PhraseOut ) )
    putStrLn $ "show (  prop_guldD_eq_guldU ( undefined::" ++ sepCharS ++ "Name2PhraseOut" ++ sepCharS ++ show (  prop_guldD_eq_guldU ( undefined::Name2PhraseOut ) )
    putStrLn $ "show (  prop_gADTD2List_eq_gADTU2List ( undefined::" ++ sepCharS ++ "Name2Snippet" ++ sepCharS ++ show (  prop_gADTD2List_eq_gADTU2List ( undefined::Name2Snippet ) )
    putStrLn $ "show (  prop_gADTD2Tree_eq_gADTU2Tree ( undefined::" ++ sepCharS ++ "Name2Snippet" ++ sepCharS ++ show (  prop_gADTD2Tree_eq_gADTU2Tree ( undefined::Name2Snippet ) )
    putStrLn $ "show (  prop_gADTD2Tree_eq_gADTU2Tree ( undefined::" ++ sepCharS ++ "Name2Snippet" ++ sepCharS ++ show (  prop_gADTD2Tree_eq_gADTU2Tree ( undefined::Name2Snippet ) )
    putStrLn $ "show (  prop_gldD_eq_gldU ( undefined::" ++ sepCharS ++ "Name2Snippet" ++ sepCharS ++ show (  prop_gldD_eq_gldU ( undefined::Name2Snippet ) )
    putStrLn $ "show (  prop_guldD_eq_guldU ( undefined::" ++ sepCharS ++ "Name2Snippet" ++ sepCharS ++ show (  prop_guldD_eq_guldU ( undefined::Name2Snippet ) )
    putStrLn $ "show (  prop_gADTD2List_eq_gADTU2List ( undefined::" ++ sepCharS ++ "Phrase" ++ sepCharS ++ show (  prop_gADTD2List_eq_gADTU2List ( undefined::Phrase ) )
    putStrLn $ "show (  prop_gADTD2Tree_eq_gADTU2Tree ( undefined::" ++ sepCharS ++ "Phrase" ++ sepCharS ++ show (  prop_gADTD2Tree_eq_gADTU2Tree ( undefined::Phrase ) )
    putStrLn $ "show (  prop_gADTD2Tree_eq_gADTU2Tree ( undefined::" ++ sepCharS ++ "Phrase" ++ sepCharS ++ show (  prop_gADTD2Tree_eq_gADTU2Tree ( undefined::Phrase ) )
    putStrLn $ "show (  prop_gldD_eq_gldU ( undefined::" ++ sepCharS ++ "Phrase" ++ sepCharS ++ show (  prop_gldD_eq_gldU ( undefined::Phrase ) )
    putStrLn $ "show (  prop_guldD_eq_guldU ( undefined::" ++ sepCharS ++ "Phrase" ++ sepCharS ++ show (  prop_guldD_eq_guldU ( undefined::Phrase ) )
    putStrLn $ "show (  prop_gADTD2List_eq_gADTU2List ( undefined::" ++ sepCharS ++ "Phrase2Name" ++ sepCharS ++ show (  prop_gADTD2List_eq_gADTU2List ( undefined::Phrase2Name ) )
    putStrLn $ "show (  prop_gADTD2Tree_eq_gADTU2Tree ( undefined::" ++ sepCharS ++ "Phrase2Name" ++ sepCharS ++ show (  prop_gADTD2Tree_eq_gADTU2Tree ( undefined::Phrase2Name ) )
    putStrLn $ "show (  prop_gADTD2Tree_eq_gADTU2Tree ( undefined::" ++ sepCharS ++ "Phrase2Name" ++ sepCharS ++ show (  prop_gADTD2Tree_eq_gADTU2Tree ( undefined::Phrase2Name ) )
    putStrLn $ "show (  prop_gldD_eq_gldU ( undefined::" ++ sepCharS ++ "Phrase2Name" ++ sepCharS ++ show (  prop_gldD_eq_gldU ( undefined::Phrase2Name ) )
    putStrLn $ "show (  prop_guldD_eq_guldU ( undefined::" ++ sepCharS ++ "Phrase2Name" ++ sepCharS ++ show (  prop_guldD_eq_guldU ( undefined::Phrase2Name ) )
    putStrLn $ "show (  prop_gADTD2List_eq_gADTU2List ( undefined::" ++ sepCharS ++ "Phrase2NameOut" ++ sepCharS ++ show (  prop_gADTD2List_eq_gADTU2List ( undefined::Phrase2NameOut ) )
    putStrLn $ "show (  prop_gADTD2Tree_eq_gADTU2Tree ( undefined::" ++ sepCharS ++ "Phrase2NameOut" ++ sepCharS ++ show (  prop_gADTD2Tree_eq_gADTU2Tree ( undefined::Phrase2NameOut ) )
    putStrLn $ "show (  prop_gADTD2Tree_eq_gADTU2Tree ( undefined::" ++ sepCharS ++ "Phrase2NameOut" ++ sepCharS ++ show (  prop_gADTD2Tree_eq_gADTU2Tree ( undefined::Phrase2NameOut ) )
    putStrLn $ "show (  prop_gldD_eq_gldU ( undefined::" ++ sepCharS ++ "Phrase2NameOut" ++ sepCharS ++ show (  prop_gldD_eq_gldU ( undefined::Phrase2NameOut ) )
    putStrLn $ "show (  prop_guldD_eq_guldU ( undefined::" ++ sepCharS ++ "Phrase2NameOut" ++ sepCharS ++ show (  prop_guldD_eq_guldU ( undefined::Phrase2NameOut ) )
    putStrLn $ "show (  prop_gADTD2List_eq_gADTU2List ( undefined::" ++ sepCharS ++ "Phrase2Snippet" ++ sepCharS ++ show (  prop_gADTD2List_eq_gADTU2List ( undefined::Phrase2Snippet ) )
    putStrLn $ "show (  prop_gADTD2Tree_eq_gADTU2Tree ( undefined::" ++ sepCharS ++ "Phrase2Snippet" ++ sepCharS ++ show (  prop_gADTD2Tree_eq_gADTU2Tree ( undefined::Phrase2Snippet ) )
    putStrLn $ "show (  prop_gADTD2Tree_eq_gADTU2Tree ( undefined::" ++ sepCharS ++ "Phrase2Snippet" ++ sepCharS ++ show (  prop_gADTD2Tree_eq_gADTU2Tree ( undefined::Phrase2Snippet ) )
    putStrLn $ "show (  prop_gldD_eq_gldU ( undefined::" ++ sepCharS ++ "Phrase2Snippet" ++ sepCharS ++ show (  prop_gldD_eq_gldU ( undefined::Phrase2Snippet ) )
    putStrLn $ "show (  prop_guldD_eq_guldU ( undefined::" ++ sepCharS ++ "Phrase2Snippet" ++ sepCharS ++ show (  prop_guldD_eq_guldU ( undefined::Phrase2Snippet ) )
    putStrLn $ "show (  prop_gADTD2List_eq_gADTU2List ( undefined::" ++ sepCharS ++ "PhraseIn" ++ sepCharS ++ show (  prop_gADTD2List_eq_gADTU2List ( undefined::PhraseIn ) )
    putStrLn $ "show (  prop_gADTD2Tree_eq_gADTU2Tree ( undefined::" ++ sepCharS ++ "PhraseIn" ++ sepCharS ++ show (  prop_gADTD2Tree_eq_gADTU2Tree ( undefined::PhraseIn ) )
    putStrLn $ "show (  prop_gADTD2Tree_eq_gADTU2Tree ( undefined::" ++ sepCharS ++ "PhraseIn" ++ sepCharS ++ show (  prop_gADTD2Tree_eq_gADTU2Tree ( undefined::PhraseIn ) )
    putStrLn $ "show (  prop_gldD_eq_gldU ( undefined::" ++ sepCharS ++ "PhraseIn" ++ sepCharS ++ show (  prop_gldD_eq_gldU ( undefined::PhraseIn ) )
    putStrLn $ "show (  prop_guldD_eq_guldU ( undefined::" ++ sepCharS ++ "PhraseIn" ++ sepCharS ++ show (  prop_guldD_eq_guldU ( undefined::PhraseIn ) )
    putStrLn $ "show (  prop_gADTD2List_eq_gADTU2List ( undefined::" ++ sepCharS ++ "Snippet2Name" ++ sepCharS ++ show (  prop_gADTD2List_eq_gADTU2List ( undefined::Snippet2Name ) )
    putStrLn $ "show (  prop_gADTD2Tree_eq_gADTU2Tree ( undefined::" ++ sepCharS ++ "Snippet2Name" ++ sepCharS ++ show (  prop_gADTD2Tree_eq_gADTU2Tree ( undefined::Snippet2Name ) )
    putStrLn $ "show (  prop_gADTD2Tree_eq_gADTU2Tree ( undefined::" ++ sepCharS ++ "Snippet2Name" ++ sepCharS ++ show (  prop_gADTD2Tree_eq_gADTU2Tree ( undefined::Snippet2Name ) )
    putStrLn $ "show (  prop_gldD_eq_gldU ( undefined::" ++ sepCharS ++ "Snippet2Name" ++ sepCharS ++ show (  prop_gldD_eq_gldU ( undefined::Snippet2Name ) )
    putStrLn $ "show (  prop_guldD_eq_guldU ( undefined::" ++ sepCharS ++ "Snippet2Name" ++ sepCharS ++ show (  prop_guldD_eq_guldU ( undefined::Snippet2Name ) )
    putStrLn $ "show (  prop_gADTD2List_eq_gADTU2List ( undefined::" ++ sepCharS ++ "Snippet2Phrase" ++ sepCharS ++ show (  prop_gADTD2List_eq_gADTU2List ( undefined::Snippet2Phrase ) )
    putStrLn $ "show (  prop_gADTD2Tree_eq_gADTU2Tree ( undefined::" ++ sepCharS ++ "Snippet2Phrase" ++ sepCharS ++ show (  prop_gADTD2Tree_eq_gADTU2Tree ( undefined::Snippet2Phrase ) )
    putStrLn $ "show (  prop_gADTD2Tree_eq_gADTU2Tree ( undefined::" ++ sepCharS ++ "Snippet2Phrase" ++ sepCharS ++ show (  prop_gADTD2Tree_eq_gADTU2Tree ( undefined::Snippet2Phrase ) )
    putStrLn $ "show (  prop_gldD_eq_gldU ( undefined::" ++ sepCharS ++ "Snippet2Phrase" ++ sepCharS ++ show (  prop_gldD_eq_gldU ( undefined::Snippet2Phrase ) )
    putStrLn $ "show (  prop_guldD_eq_guldU ( undefined::" ++ sepCharS ++ "Snippet2Phrase" ++ sepCharS ++ show (  prop_guldD_eq_guldU ( undefined::Snippet2Phrase ) )
    putStrLn $ "show (  prop_gADTD2List_eq_gADTU2List ( undefined::" ++ sepCharS ++ "Snippet2PhraseIn" ++ sepCharS ++ show (  prop_gADTD2List_eq_gADTU2List ( undefined::Snippet2PhraseIn ) )
    putStrLn $ "show (  prop_gADTD2Tree_eq_gADTU2Tree ( undefined::" ++ sepCharS ++ "Snippet2PhraseIn" ++ sepCharS ++ show (  prop_gADTD2Tree_eq_gADTU2Tree ( undefined::Snippet2PhraseIn ) )
    putStrLn $ "show (  prop_gADTD2Tree_eq_gADTU2Tree ( undefined::" ++ sepCharS ++ "Snippet2PhraseIn" ++ sepCharS ++ show (  prop_gADTD2Tree_eq_gADTU2Tree ( undefined::Snippet2PhraseIn ) )
    putStrLn $ "show (  prop_gldD_eq_gldU ( undefined::" ++ sepCharS ++ "Snippet2PhraseIn" ++ sepCharS ++ show (  prop_gldD_eq_gldU ( undefined::Snippet2PhraseIn ) )
    putStrLn $ "show (  prop_guldD_eq_guldU ( undefined::" ++ sepCharS ++ "Snippet2PhraseIn" ++ sepCharS ++ show (  prop_guldD_eq_guldU ( undefined::Snippet2PhraseIn ) ) 
    
    putStrLn $ "------- Inverse Function Properties --- "     
    
    putStrLn $ "prop_inverse_gread " ++ sepCharS ++ "Authority" ++ sepCharS ++  show ( prop_inverse_gread authorityDefault )
    putStrLn $ "prop_inverse_gread " ++ sepCharS ++ "ColumnName" ++ sepCharS ++  show ( prop_inverse_gread columnNameDefault )
    putStrLn $ "prop_inverse_gread " ++ sepCharS ++ "Domain" ++ sepCharS ++  show ( prop_inverse_gread domainDefault )
    putStrLn $ "prop_inverse_gread " ++ sepCharS ++ "Name2Phrase" ++ sepCharS ++  show ( prop_inverse_gread name2PhraseDefault )
    putStrLn $ "prop_inverse_gread " ++ sepCharS ++ "Name2PhraseMin" ++ sepCharS ++  show ( prop_inverse_gread name2PhraseMinDefault )
    putStrLn $ "prop_inverse_gread " ++ sepCharS ++ "Name2PhraseOut" ++ sepCharS ++  show ( prop_inverse_gread name2PhraseOutDefault )
    putStrLn $ "prop_inverse_gread " ++ sepCharS ++ "Name2Snippet" ++ sepCharS ++  show ( prop_inverse_gread name2SnippetDefault )
    putStrLn $ "prop_inverse_gread " ++ sepCharS ++ "Phrase" ++ sepCharS ++  show ( prop_inverse_gread phraseDefault )
    putStrLn $ "prop_inverse_gread " ++ sepCharS ++ "Phrase2Name" ++ sepCharS ++  show ( prop_inverse_gread phrase2NameDefault )
    putStrLn $ "prop_inverse_gread " ++ sepCharS ++ "Phrase2NameOut" ++ sepCharS ++  show ( prop_inverse_gread phrase2NameOutDefault )
    putStrLn $ "prop_inverse_gread " ++ sepCharS ++ "Phrase2Snippet" ++ sepCharS ++  show ( prop_inverse_gread phrase2SnippetDefault )
    putStrLn $ "prop_inverse_gread " ++ sepCharS ++ "PhraseIn" ++ sepCharS ++  show ( prop_inverse_gread phraseInDefault )
    putStrLn $ "prop_inverse_gread " ++ sepCharS ++ "Snippet2Name" ++ sepCharS ++  show ( prop_inverse_gread snippet2NameDefault )
    putStrLn $ "prop_inverse_gread " ++ sepCharS ++ "Snippet2Phrase" ++ sepCharS ++  show ( prop_inverse_gread snippet2PhraseDefault )
    putStrLn $ "prop_inverse_gread " ++ sepCharS ++ "Snippet2PhraseIn" ++ sepCharS ++  show ( prop_inverse_gread snippet2PhraseInDefault )

 
    putStrLn $ "------- Simple File Test -------------------- " 

-- read and parse the file 
    authorityResults <- readWrapper parmInputTSVFile "common.in.Authority.csv" authorityDefault authorityLoad 
-- display any Parse errors 
    putStrLn $ " Authority Parse error: " ++ ( readResultErrorParse authorityResults ) 
-- display any typing errors 
    putStrLn $ errors2String ( undefined::Authority ) ( readResultTypeErrors authorityResults ) 
-- collect for print out, if needed 
    let toPrintList = [listToPrint TSV ( readResultType authorityResults )] ++ [errorsToPrint ( undefined::Authority ) ( readResultTypeErrors authorityResults )] 
    
-- CRASH! 
--    putStrLn $ "undefined = " ++ undefined 

    putStrLn $ "------- OUTPUT -------------------- " 
 
-- print multiple files toPrintListX 
-- mapM_ ( \x -> printFile ( toPrintFileName x ) ( toPrintContents x ) ) ( filter ( /= toPrintDefault ) toPrintList ) 
    mapM_ ( \x -> printFile ( toPrintFileName x ) ( toPrintContents x ) ) toPrintList 

