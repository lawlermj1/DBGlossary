
{- |
   File        : DBCommonTest 
   Description : Property test and demo of DBCommon boilerplate functions usage 
   Copyright   : ( c ) Matthew Lawler 2018 
   License     : 2-Clause BSD
   Maintainer  : lawlermj1@gmail.com

   This file is a test and demonstration of how to use the boilerplate functions from DBCommon. 
   It reads a simple csv, checks the strings, loads into types and outputs the result. 
   It also performs some property testing on the generated boilerplate functions. 

 -}
module Main where 
-- Prelude modules  
import Data.Generics    
import Data.List   
import Data.Time    

-- Custom built modules 
import DBCommon.Boilerplate  
import DBCommon.PreludeGapFiller 
import DBCommon.EnumType  
import DBCommon.IOCommon  
import DBCommon.Usage  

------------------------------------
--    current input default 
testTheData = True    

main = do
--    some parms 
    putStrLn $ " DBCommonTest " 

-------------------------  
--    INPUTS 
--    NEW pattern #5 
--    read and parse the file 
    dBCommonFileResults <- readWrapper parmInputTSVFile "common.in.DBCommonFile.csv" dBCommonFileDefault dBCommonFileLoad     
--    display any Parse errors     
    putStrLn $ " DBCommonFile Parse error: " ++ ( readResultErrorParse dBCommonFileResults )     
--    display any typing errors         
    putStrLn $ errors2String ( undefined::DBCommonFile ) ( readResultTypeErrors dBCommonFileResults )  
--    collect for print out, if needed 
    let toPrintList = [listToPrint TSV ( readResultType dBCommonFileResults )] ++ [errorsToPrint ( undefined::DBCommonFile ) ( readResultTypeErrors dBCommonFileResults )]  

--------------------------------------------------------------------
--    Some invariant checks 

    let dBCommonFileGood = ["TDS","SYS","1","9","9","9.1"] 
    let fileTypeGood = ["TDS"] 
    let sourceTypeGood = ["SYS"] 

    putStrLn $ "------- Demonstration tests of exported functions -------------------- "    
    putStrLn $ "-- These tests show how to call the functions, and what output to expect "        
    putStrLn $ "------- SHOW undefined Defaults -------------------- "   
--    double test of gdefaultU and gshowQ ( aka show ) for all data types  
    putStrLn $ "show (  gdefaultU ( undefined::FileType )   )  " ++ show (  gdefaultU ( undefined::FileType )   )  
    putStrLn $ "show (  gdefaultU ( undefined::SourceType )    )  " ++ show (  gdefaultU ( undefined::SourceType )    )  
    putStrLn $ "show (  gdefaultU ( undefined::DBCommonFile )    )  " ++ show (  gdefaultU ( undefined::DBCommonFile )    )  
    
--    test of gheaderShowQ  for all data types  
    putStrLn $ "gheaderShowQ (  gdefaultU ( undefined::FileType )   )  " ++ gheaderShowQ (  gdefaultU ( undefined::FileType )   )  
    putStrLn $ "gheaderShowQ (  gdefaultU ( undefined::SourceType )    )  " ++ gheaderShowQ (  gdefaultU ( undefined::SourceType )    )  
    putStrLn $ "gheaderShowQ (  gdefaultU ( undefined::DBCommonFile )    )  " ++ gheaderShowQ (  gdefaultU ( undefined::DBCommonFile )    )  

    putStrLn $ "------- HEADER -------------------- " 
    putStrLn $ "gheaderShowQ dBCommonFileDefault = " ++ gheaderShowQ dBCommonFileDefault  
    putStrLn $ "gshowQ dBCommonFileDefault = " ++ gshowQ dBCommonFileDefault 
   
    putStrLn $ "gheaderShowD dBCommonFileDefault = " ++ show ( gheaderShowD  dBCommonFileDefault ) 
    putStrLn $ "gheaderShowD dBCommonFileDefault = " ++ gheaderShowD dBCommonFileDefault ++ " : Count = " ++ show ( length ( words (  gheaderShowD dBCommonFileDefault ) ) )

    putStrLn $ "gheaderFieldD dBCommonFileDefault = " ++ show ( gheaderFieldD  dBCommonFileDefault ) 
    putStrLn $ "gheaderFieldD dBCommonFileDefault = " ++ ( gheaderFieldD  dBCommonFileDefault ) ++ " : Count = " ++show ( length ( words (  gheaderFieldD dBCommonFileDefault ) ) ) 
    
    putStrLn $ "------- SHOW Default  -------------------- "  
    putStrLn $ "show dBCommonFileDefault = " ++ show dBCommonFileDefault  
    putStrLn $ "gshow dBCommonFileDefault = " ++ gshow dBCommonFileDefault  
    putStrLn $ "wordsTab show dBCommonFileDefault = " ++ intercalate "-" ( wordsTab ( show dBCommonFileDefault ) ) 
    putStrLn $ "show gdQ = " ++ show ( gdQ ( gdefaultU ( undefined::DBCommonFile ) )  ) 
    putStrLn $ "show gdS = " ++ show ( gdS ( undefined::DBCommonFile )  )     
    
    putStrLn $ "------- COUNT -------------------- "  
    putStrLn $ "gcountFieldU ( undefined::DBCommonFile ) = " ++ show ( gcountFieldU ( undefined::DBCommonFile ) ) 
    putStrLn $ "gcountFieldD dBCommonFileDefault = " ++ show ( gcountFieldD dBCommonFileDefault ) 
    putStrLn $ "gcountShowU ( undefined::DBCommonFile ) = " ++ show ( gcountShowU ( undefined::DBCommonFile ) ) 
    putStrLn $ "gcountShowD dBCommonFileDefault = " ++ show ( gcountShowD dBCommonFileDefault ) 

    putStrLn $ "------- LOAD    -------------------- "    
    putStrLn $ "gloadD dBCommonFileDefault dBCommonFileGood = " ++ show (  gloadD dBCommonFileDefault dBCommonFileGood  )  
    putStrLn $ "gloadU dBCommonFileDefault dBCommonFileGood = " ++ show (  gloadU dBCommonFileDefault dBCommonFileGood  ) 
    
    putStrLn $ "------- UNLOAD -------------------- "     
    putStrLn $ "gunloadD dBCommonFileDefault = " ++ show ( gunloadD ( guldU ( undefined::DBCommonFile ) ) dBCommonFileDefault ) 

    putStrLn $ "------- Other Types  -------------------- " 
    putStrLn $ "gelemsU ( undefined::Int ) = " ++ show ( gelemsU ( undefined::Int ) ) 
--    ->    gelemsU ( undefined::Int ) = []    
    putStrLn $ "gelemsU ( undefined::Bool ) = " ++ show ( gelemsU ( undefined::Bool ) ) 
--    ->    gelemsU ( undefined::Bool ) = ["False","True"]  
    putStrLn $ "showGregorian epochDay = " ++ showGregorian epochDay   
    
    putStrLn $ "------- Property tests -------------------- "     
    putStrLn $ "-- These are mainly tests that result from isomorphisms --" 
    
    putStrLn $ "prop_gADTD2List_eq_gADTU2List ( undefined::DBCommonFile ) = " ++ show ( prop_gADTD2List_eq_gADTU2List ( undefined::DBCommonFile )  )     
    putStrLn $ "prop_gADTD2List_eq_gADTU2List gdefaultD ( undefined::DBCommonFile ) = " ++ show ( prop_gADTD2List_eq_gADTU2List ( gdefaultD ( undefined::DBCommonFile ) ) )  
    
    putStrLn $ "prop_gADTD2Tree_eq_gADTU2Tree ( undefined::DBCommonFile ) = " ++ show ( prop_gADTD2Tree_eq_gADTU2Tree ( undefined::DBCommonFile )  ) 
    putStrLn $ "prop_gADTD2Tree_eq_gADTU2Tree gdefaultD ( undefined::DBCommonFile ) = " ++ show ( prop_gADTD2Tree_eq_gADTU2Tree ( gdefaultD ( undefined::DBCommonFile ) ) ) 
    
    putStrLn $ "prop_gelems_eq_gelemsU ( undefined::DBCommonFile ) = " ++ show ( prop_gelems_eq_gelemsU ( undefined::DBCommonFile )  ) 
    putStrLn $ "prop_gcheckEnumType_eq_gcheckEnumTypeU ( undefined::FileType ) XYZ = " ++ show ( prop_gcheckEnumType_eq_gcheckEnumTypeU ( undefined::FileType ) "XYZ"  )     
    
    putStrLn $ "prop_gcount_eq_gcountU ( undefined::DBCommonFile ) = " ++ show ( prop_gcount_eq_gcountU ( undefined::DBCommonFile )  )     
    putStrLn $ "prop_gheader_length_eq_gcountU ( undefined::DBCommonFile ) = " ++ show ( prop_gheader_length_eq_gcountU ( undefined::DBCommonFile )  ) 
    putStrLn $ "prop_gheaderShowQ_eq_gheaderShowD dBCommonFileDefault is " ++ show ( prop_gheaderShowQ_eq_gheaderShowD dBCommonFileDefault ) 
    putStrLn $ "prop_inverse_gread dBCommonFileDefault is " ++ show ( prop_inverse_gread dBCommonFileDefault ) 

    putStrLn $ "prop_gcheck1D_eq_gcheck1U ( undefined::FileType ) = " ++ show ( prop_gcheck1D_eq_gcheck1U ( undefined::FileType )  )     
    putStrLn $ "prop_gcheck1D_eq_gcheck1U ( undefined::SourceType ) = " ++ show ( prop_gcheck1D_eq_gcheck1U ( undefined::SourceType )  )     
    putStrLn $ "prop_gcheck1D_eq_gcheck1U ( undefined::DBCommonFile ) = " ++ show ( prop_gcheck1D_eq_gcheck1U ( undefined::DBCommonFile )  )         

    putStrLn $ "prop_gcheckD_eq_gcheckU ( undefined::FileType ) = " ++ show ( prop_gcheckD_eq_gcheckU ( undefined::FileType ) fileTypeGood )     
    putStrLn $ "prop_gcheckD_eq_gcheckU ( undefined::SourceType ) = " ++ show ( prop_gcheckD_eq_gcheckU ( undefined::SourceType ) sourceTypeGood )     
    putStrLn $ "prop_gcheckD_eq_gcheckU ( undefined::DBCommonFile ) = " ++ show ( prop_gcheckD_eq_gcheckU ( undefined::DBCommonFile ) dBCommonFileGood ) 

    putStrLn $ "prop_gldD_eq_gldU ( undefined::DBCommonFile ) = " ++ show ( prop_gldD_eq_gldU ( undefined::DBCommonFile )  )     
    putStrLn $ "prop_gldD_eq_gldU gdefaultD ( undefined::DBCommonFile ) = " ++ show ( prop_gldD_eq_gldU ( gdefaultD ( undefined::DBCommonFile ) ) )   

    putStrLn $ "prop_guldD_eq_guldU ( undefined::DBCommonFile ) = " ++ show ( prop_guldD_eq_guldU ( undefined::DBCommonFile )  ) 
    putStrLn $ "prop_guldD_eq_guldU gdefaultD ( undefined::DBCommonFile ) = " ++ show ( prop_guldD_eq_guldU ( gdefaultD ( undefined::DBCommonFile ) ) ) 
    

--    CRASH! 
--    putStrLn $ "undefined = " ++ undefined 
    putStrLn $ "--------------------------" 


-- OUTPUT ------------------------------------------------------------------

--    print multiple files toPrintListX 
--    mapM_ ( \x -> printFile ( toPrintFileName x ) ( toPrintContents x ) ) ( filter ( /= toPrintDefault ) toPrintList ) 
    mapM_ ( \x -> printFile ( toPrintFileName x ) ( toPrintContents x ) ) toPrintList  

   
