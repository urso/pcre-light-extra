{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}

module Text.Regex.PCRE.Light.Extra where

import Control.Arrow (right)
import Data.ByteString (ByteString)
import qualified Text.Regex.PCRE.Light as R
import Data.Maybe (isJust)

data RegexPattern = Regex [R.PCREOption] [R.PCREExecOption] ByteString
data RegexExecPattern = RegexExec [R.PCREExecOption] R.Regex

class MatchResult x where
  convert :: Maybe [ByteString] -> x

instance MatchResult (Maybe [ByteString]) where convert = id
instance MatchResult Bool where convert = isJust

class RegexLike rl where
  type RegexType rl

  compile :: rl -> Either String (RegexType rl)
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

(=~) :: (RegexLike regex, MatchResult ret) => ByteString -> regex -> ret
(=~) = flip match

cfg :: [R.PCREOption] -> [R.PCREExecOption] -> ByteString -> RegexPattern
cfg = Regex

withExecOpts :: [R.PCREExecOption] -> R.Regex -> RegexExecPattern
withExecOpts = RegexExec

caseInsensitive = Regex [R.caseless, R.no_utf8_check] []

