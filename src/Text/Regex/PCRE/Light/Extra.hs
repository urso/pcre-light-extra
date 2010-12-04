{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}

-- | pcre-light utility functions for more user friendly usage.
-- Instead of adding execution and compile options to the matching and
-- compiling functions, the options are added to the regular expression to be
-- compiled or run. Furthermore support for different matching return types
-- (using typeclass 'MatchResult') and different regular expression types
-- (compiled or uncompiled using typeclass 'RegexLike') are supported.
--
-- examples using GHC's -XOverloadedStrings flag:
--
-- simple matching with uncompiled pattern "abc" of type ByteString:
--
-- >>> ("abc" =~ ("abc" :: ByteString)) :: Bool
-- True
--
-- case insensitive matching with uncompiled pattern of type ByteString:
--
-- >>> ("AbCasf" =~ caseSensitive False "abc") :: Bool
-- True
--
-- >>> ("AbCasf" =~ caseSensitive False "abc") :: Maybe [ByteString]
-- Just ["AbC"]
--
module Text.Regex.PCRE.Light.Extra where

import Control.Arrow (right)
import Data.ByteString (ByteString)
import Data.Maybe (isJust)

import qualified Text.Regex.PCRE.Light as R

-- | Uncompiled regular expression with compile and execution options.
-- Compiles to RegexExecPattern preserving execution options.
data RegexPattern = Regex [R.PCREOption] [R.PCREExecOption] ByteString

-- | Compiled regular expression with execution results.
data RegexExecPattern = RegexExec [R.PCREExecOption] R.Regex

-- | Typeclass defining automatic conversion of PCRE matching results
-- to user defined type. Used in typeclass 'RegexLike' and function ('=~').
class MatchResult x where
  convert :: Maybe [ByteString] -> x

instance MatchResult (Maybe [ByteString]) where 
  convert = id

instance MatchResult Bool where 
  convert = isJust

-- | RegexLike types can be compiled to regular epxression or directly used
-- as regular expression.
class RegexLike rl where
  -- | Type of compiled regular expression for instance rl.
  type RegexType rl 

  -- | compiles a perl-compatible regular expression to an executable
  -- regular expression. See 'Text.Regex.PCRE.Light.compileM'.
  compile :: rl -> Either String (RegexType rl)

  -- | matches a Bytestring with a regular expression of type rl.
  -- If rl needs to be compiled (for example when rl ~ ByteString or
  -- rl ~ RegexPattern) but is invalid, an exception will be thrown. It is
  -- recommended to use compiled patterns with match for better error handling.
  match   :: (MatchResult res) => rl -> ByteString -> res

instance RegexLike R.Regex where
  type RegexType R.Regex = R.Regex

  compile = Right . id
  match regex str = convert $ R.match regex str []

instance RegexLike ByteString where
  type RegexType ByteString = R.Regex

  compile regex     = R.compileM regex []
  match pattern str = either error (`match` str) $ compile pattern

instance RegexLike RegexExecPattern where
  type RegexType RegexExecPattern = RegexExecPattern

  compile = Right . id
  match (RegexExec opts regex) str = convert $ R.match regex str opts

instance RegexLike RegexPattern where
  type RegexType RegexPattern = RegexExecPattern

  compile (Regex opt execOpt pattern) = 
    right (RegexExec execOpt) $ R.compileM pattern opt

  match r str = either error (`match` str) $ compile r

-- | matches a ByteString with a regular expression.
(=~) :: (RegexLike regex, MatchResult ret) => ByteString -> regex -> ret
(=~) = flip match

-- | create regular expression with compile and execution options.
cfg :: [R.PCREOption] -> [R.PCREExecOption] -> ByteString -> RegexPattern
cfg = Regex

-- | Add execution options to compiled regular expression of pcre-light's 
--   compiled regular expression type 'R.Regex'
withExecOpts :: [R.PCREExecOption] -> R.Regex -> RegexExecPattern
withExecOpts = RegexExec

-- | Create case sensitive (first parameter is 'True') or case insensitive regular expression from pattern.
caseSensitive :: Bool -> ByteString -> RegexPattern
caseSensitive False = cfg [R.caseless, R.no_utf8_check] []
caseSensitive True  = cfg [R.no_utf8_check] []

