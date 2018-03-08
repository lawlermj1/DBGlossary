# DBGlossary


# Creating a glossary by parsing words from DB column names 

## Purpose 

For a Haskell programmer, who wants to extract all used words from a set of names, the DBGlossary is a simple set of modules to help parse out the vocabulary hidden in the names. 

## Background 

A previous version of this software was used to extract 6,000 words from a namespace of 200,000 column names. 

This was built in Haskell 8. This software has only been used by myself, for automating aspects of design. It was only run intermittently when a new design task was at hand. Therefore, the IO is very simple, consisting of CSV file handing. There is also no Cabal install.  

This is a waypost of the lambda journey I am on. None of this would be considered advanced material, but it is something. Or as somebody else put it: 
_'Now this is not the end. It is not even the beginning of the end. But it is, perhaps, the end of the beginning.'_ 

Any errors or misapplication of Haskell are my own. 

## Design - Haskell Modules 

The following Haskell modules, types and functions are used: 

0	 | 	Module	 | 	type	 | 	functions
 --	 | 	 --------	 | 	 ----	 | 	 ---------
1	 | 	import Control.Exception	 | 	Exception	 | 	bracketOnError
2	 | 	import Data.Char	 | 	Char	 | 	many functions 
3	 | 	import Data.Data	 | 	Data	 | 	
4	 | 	import Data.Either	 | 	Either	 | 	rights, 
5	 | 	import Data.Generics	 | 	Generic* 	 | 	gread, gshow, ext1Q , extQ
6	 | 	import Data.List	 | 	List	 | 	intercalate, sort, sortBy, unfoldr, \\, nub, foldl, ... 
7	 | 	import Data.List.Split	 | 	a	 | 	splitOneOf
8	 | 	import Data.Map.Strict	 | 	Map	 | 	fromAscList, lookup
9	 | 	import Data.Maybe	 | 	Maybe	 | 	fromMaybe, listToMaybe 
10	 | 	import Data.Ord	 | 	Ord	 | 	comparing
11	 | 	import Data.Time	 | 	Day, UTCTime 	 | 	
12	 | 	import Data.Typeable	 | 	Typeable	 | 	
13	 | 	import System.Directory	 | 	FilePath	 | 	doesFileExist, renameFile, removeFile
14	 | 	import System.IO	 | 	IO	 | 	bracketOnError, hClose, hPutStr
15	 | 	import Text.Parsec.Error	 | 	Message	 | 	errorMessages, messageString 
16	 | 	import Text.ParserCombinators.Parsec	 | 	ParseError	 | 	parse, endBy, sepBy, oneOf, <|>, noneOf, many, <?>, try

## Design - Custom Modules 

The following custom modules were developed: 

Module	 | 	Where 	 | 	Why
 --------	 | 	 --------	 | 	 ----
DBCommonTest	 | 	\	 | 	Property test and demo of DBCommon boilerplate functions usage 
DBGlossaryTest	 | 	\	 | 	Property test of all GlossaryBP type boilerplate functions 
Glossary	 | 	\	 | 	Parses phrases from a names set, using discovered snippets 
EnumType	 | 	\DB Glossary	 | 	Sum types needed for glossary functions
GlossaryBP	 | 	\DB Glossary	 | 	Defines glossary functions used to discover and verify parsed phrases extracted from a names list. This could be all columns in a DB Schema.
Boilerplate	 | 	\DBCommon	 | 	This module defines generic functions that traverse data types. These functions enable show, default values, load, unload and type checking. In addition, there are property functions to test if the generated boilerplate functions work correctly for each type.
EnumType	 | 	\DBCommon	 | 	Sum types needed for common functions
IOCommon	 | 	\DBCommon	 | 	Input + Error + Output functions for CSV files 
PreludeGapFiller	 | 	\DBCommon	 | 	Utility functions
Usage	 | 	\DBCommon	 | 	Example usage of boilerplate functions


## Design - Ordinary Features 

The package demonstrates the usage of: 
	1. Prelude classes like Eq, Ord, Num, Float, Show, Read, etc. 
	2. Prelude types like String, Char, Bool, etc. 
	3. Prelude functions like map, fold, id, (.), undefined, head, tail, concat, take, zip, etc. 
	4. Algebraic data type declarations, both Sum and Product types 
	5. Standard File IO 
	6. Other classes like Data, Traversable, Functor, Foldable, etc.
	7. Other types like IO, Map, Maybe, Either, Exception, FilePath, Day, UTCTime, etc. 	
	8. Parsing types like Message and ParseError 

## Design - Challenges

These occurred when building the Boilerplate module. 
For me, the main design challenges occurred were dealing with Generic types: 
	1. defining generic functions with undefined (i.e. uninstanced) types 
	2. defining a type that handled Algebraic Data type (ADT) term transformation and function equality 
	3. defining effective property type tests 
	
### Undefined Types 

An issue with using gmapQ is that it cannot work on an undefined instance of the data type. 
However, that is exactly the state when declaring the Algebraic Data type (ADT) . 
So, in order to create a default value, only the undefined data type is available. 
Oleg's gunfold only inspects the data type declaration, so gunfold can be used to create from an undefined data type generic default, load and show functions. 
The gmapQ approach is still useful, as this enables the creation of 'Equivalent Function Properties'. 

### Algebraic Data type (ADT) term transformation 

The ADT term transformation depended on creating a type that would contain the term transformation function for default, loading, etc. 
As the Lambda calculus cannot determine function equality, the simple expedient of using the function name was used to do Eq checking. 

### Design - Property Tests 

Two kinds of property tests were developed. 
    1. Equivalent Function Property
	2. Inverse Function Property 
	
#### Equivalent Function Property	
    That is, for all types, the equivalent functions should produce equal results.  
    The functions needs to process the immediate subterms of an algebraic data type and perform some transformation. 
    The two approaches used were, firstly, gmapQ from Data.Data and secondly, gunfold defined by Oleg Kiselyov at http://okmij.org/ftp/Haskell/generics.html 
    Oleg Kiselyov's blog discusses Data-Generic and Data-Extensible Programming in Haskell. 
    The used properties are: 
        prop_gADTD2Tree_eq_gADTU2Tree - DTTree or Data Type Tree of the algebraic data type is the same using gmapQ or gunfold. 
        prop_gADTD2List_eq_gADTU2List - Data Type List of the algebraic data type is the same using gmapQ or gunfold. 
        prop_gldD_eq_gldU - The load function generated by gmapQ or gunfold produces the same results. 
        prop_guldD_eq_guldU - The unload function generated by gmapQ or gunfold produces the same results. 
    These properties need to be True for all declared data types. 
    
    Other possible properties include: 
        gdefault == gd 
        gelemsD == geU 
        gcheckEnumTypeD == gcheckEnumTypeU  
        gcheck == gcheckUnfold         
        gcount == gcountFieldsUnfold 
        count gheader == count gcount 

#### The Inverse Function Property. 
	That is, for all types, composing the function and its inverse should equal the identity function.  
    The used properties are: 
        prop_inverse_gread - ( fst.head.gread.gshow ) a == id a or read and show are inverses 

	Note: The full gunfold is not used. Specifically, recursive data types are not handled.
 
