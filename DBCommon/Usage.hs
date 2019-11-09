{-# LANGUAGE DeriveDataTypeable #-} 

{- |
   Module      : DBCommon.Usage
   Description : Usage of boilerplate functions
   Copyright   : ( c ) Matthew Lawler 2018 
   License     : 2-Clause BSD
   Maintainer  : lawlermj1@gmail.com

   This module uses a sample product type to demonstrate how to use the boilerplate functions to define a default, load and show functions.  

 -}
module DBCommon.Usage
    (  
--    PUBLIC Types 
      DBCommonFile( .. ), 
      
--    PUBLIC functions 
      dBCommonFileDefault, 
      dBCommonFileLoad, 

     ) where
--------------------------------------------------------------------------------

import Data.Data   
-- import Data.Typeable 

import DBCommon.Boilerplate  
-- import DBCommon.PreludeGapFiller  
import DBCommon.EnumType 

--------------------------------------------------------------------------------
--    sample Product Type  
data DBCommonFile = DBCommonFile { dBCommonFileType :: FileType  
                    , dBCommonFileSourceType :: SourceType 
                    , dBCommonFileName :: String
                    , dBCommonFileIsIn :: Bool 
                    , dBCommonFileCount :: Int   
                    , dBCommonFileBigInt :: Integer 
                    , dBCommonFileFraction :: Float                        
                    } deriving (  Eq, Ord, Typeable, Data  ) 

--------------------------------------------------------------------------------
--    Boilerplate DEFAULTS 
dBCommonFileDefault :: DBCommonFile
dBCommonFileDefault = gdefaultU ( undefined::DBCommonFile ) 

--------------------------------------------------------------------------------
--    Boilerplate LOADS  
dBCommonFileLoad :: [String] -> DBCommonFile
dBCommonFileLoad = gloadU ( undefined::DBCommonFile ) 

--------------------------------------------------------------------------------
--    Boilerplate SHOWS  
instance Show DBCommonFile where show = gshowQ  

