{-# LANGUAGE DeriveDataTypeable #-} 

{- |
   Module      : DBCommon.EnumType 
   Description : enums or Sum types for common functions
   Copyright   : ( c ) Matthew Lawler 2018 
   License     : 2-Clause BSD
   Maintainer  : lawlermj1@gmail.com

   This module defines enums or sum types needed for common functions.  
   These enums can be added independently of a functions module.  
   
 -}
module DBCommon.EnumType 
    (  
    
      FileType( .. ), 
      SourceType( .. ), 
      
     ) where
    
import Data.Data     
-- import Data.Typeable 

--------------------------------------------------------------------------------
--    File types  
data FileType = TSV | CSV | DDL | DML | DCL | HTM | SQL | TXT | XMLOut | TDS deriving (  Eq, Ord, Typeable, Show, Read, Data ) 

--    what type of source created the data 
--    this can often be used as part of a file name 
data SourceType = SYS | C2C | DKY | GEN | MAN | PRF | VWI | RLN deriving (  Eq, Ord, Typeable, Show, Read, Data ) 
--    SYS metadata is sourced from the RDBMS schema, such as Oracle's SYS, or application maintained metadata 
--    MAN metadata is manually defined 
--    PRF metadata is sourced from from data profiling SQL 
--    GEN metadata is generated from metadata. For example, pivoted metadata derived from more abstract RDF or EAV metadata 
--    C2C metadata is generated metadata from Column to Column mappings 
--    VWI metadata is generated metadata from parsing Views to determine view Column to physical Column mappings 
--    DKY metadata is generated metadata from key parsing 
--    RLN metadata is generated metadata from Relations; normally used to infer foreign keys 
