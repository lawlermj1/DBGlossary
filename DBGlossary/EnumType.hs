{-# LANGUAGE DeriveDataTypeable #-} 

{- |
   Module      : DBGlossary.EnumType 
   Description : enums or Sum types for glossary functions
   Copyright   : ( c ) Matthew Lawler 2018  
   License     : 2-Clause BSD
   Maintainer  : lawlermj1@gmail.com

   This module defines Sum types needed for glossary functions. 
   
 -}
module DBGlossary.EnumType 
    (  
      
      PhraseType( .. ), 
      MultiWordType( .. ),       
      DomainType( .. ), 
      AuthorityType( .. ),       
      
     ) where
    
import Data.Data     
import Data.Typeable 

--    AllPhrase is the superior or top type T PhraseType 
--    Acronym is any word formed from the initial letters of a group of words e.g. WIP for Work In Progress
--    Contraction is any shortened word with missing letters. E.g. disemvowelling would remove all vowels. Yr for Year.  
--    Letter is a single alphabetic character. e.g. E 
--    MultipleWords is a phrase that consists of more than 1 word. These are sometimes needed to parse plurals. E.g. Workstatus for [Work,Status] 
--    Number is a single numeric character. e.g. 9 
--    PastTense is a phrase that occurs in the past. These are sometimes needed to parse correctly. e.g. Worked 
--    Plural is a phrase that denotes quantity. These are sometimes needed to parse correctly. e.g. Works 
--    ProperNoun is any name, such as an organisation, system name, etc. e.g. Oracle 
--    Spelling is a common misspelling. US spelling is not considered an error. 
--    Term is used for multiple word phrases that are almost a single phrase. e.g. Macaddress 
--    ZRubbish is the bottom type or _|_ PhraseType. Misspellings and non standard contractions are a common issue in database column names e.g. Wtype 
data PhraseType = AllPhrase | Acronym | Contraction | Letter | MultipleWords | Number | PastTense | Plural | ProperNoun | Spelling | Term | ZRubbish deriving (  Eq, Ord, Typeable, Show, Read, Data ) 

--    defines types of MultiWords 
--    NotMW is a phrase that is not a MultiWord.  
--    InstanceMW is a phrase that is a MultiWord. E.g. Workstatus    is [Work,Status] 
--    BaseMW is a a phrase that is often used in MultiWords. E.g. Org, Appt, etc.  
--    MultiWords is any word that causes parsing errors. 
--    These are mostly plurals, but also a past tense regular verb ending in a vowel, like archived. 
--    They can also be acronyms that start with a 2 letter word, such as ASN, ISO, ISP, etc. 
--    e.g.  AccountSource should parse as "Account,Source", but parses as "Accounts,ource"
--    Adding MultipleWords like AccountSource prevents this issue. 
data MultiWordType = NotMW | InstanceMW | BaseMW deriving (  Eq, Ord, Typeable, Show, Read, Data ) 

--    DomainType represents the 'Where' of phrases. That is, which area uses this phrase. 
--    AllDomains is the the superior or top type T Domain. 
--    CRM | Engineering | Finance | Geography | HR | IT | Telco | TimeStd | Legal |  Marketing | Model | Organisation | OU | Project | Risk | Science | StandardEnglish | Standards | SupplyChain | Workforce are level 1,as they are business functions  
--    NBN | NEAD | NSO are level 2, and are department specific  
--    BMK | PNI | SLF | WWM | LNI are level 3, and tend to be system specific 
--    ZDomain is the bottom type or _|_ Domain - where junk words, mistakes, misspellings are kept 
data DomainType =  AllDomains | CRM | Engineering | Finance | Geography | HR | IT | NBN | Telco | TimeStd | Organisation | NEAD | NSO | BMK | PNI | SLF | CIS | CAMS | WWM | ZDomain deriving (  Eq, Ord, Typeable, Show, Read, Data ) 

--    AuthorityType represents the 'Who' of phrases. That is, which person or Org has defined this phrase. 
--    Standards bodies AuthorityType include ISO, ITU, OMG, ANSI, StdAust, OASIS, etc 
--    People AuthorityType include CJDate, Kimball, etc 
--    Org AuthorityType include IBM, Oracle, etc. 
--    Book AuthorityType include LawDict, etc. 
--    Government AuthorityType include ABS, ATO, USGovt, EC, etc. 
--    Wiki is the bottom type or _|_ AuthorityType - which is the Womb of Ignorance, Kraziness and Incomprehension 
data AuthorityType = Wiki | ABS | ACCC | ALII | ANSI | ATO | BIPM | BPMI | BSI | CJDate | CS | EC | GAT | HK | IASB | IBM | IETF | ISO | ITU | Kimball | LawDict | MktgMgt | MS | NESSI | NYSSCPA | OASIS | OGC | OMG | Oracle | Primavera | StdAust | SCC | SEI | UML | USGovt | W3C deriving (  Eq, Ord, Typeable, Show, Read, Data ) 



