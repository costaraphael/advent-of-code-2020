{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Day4
  ( runPart1,
    runPart2,
  )
where

import Control.Monad.Trans.Except (ExceptT, throwE)
import Data.Functor.Identity (Identity (..))
import Data.List.Split
import Data.Maybe
import Data.Valor
import Text.Regex.TDFA
import Util

input :: String
input = "../inputs/day4.txt"

data Passport' a = Passport
  { byr :: Validatable a String (Maybe Int),
    iyr :: Validatable a String (Maybe Int),
    eyr :: Validatable a String (Maybe Int),
    hgt :: Validatable a String (Maybe String),
    hcl :: Validatable a String (Maybe String),
    ecl :: Validatable a String (Maybe String),
    pid :: Validatable a String (Maybe String),
    cid :: Validatable a String (Maybe String)
  }

type Passport = Passport' Identity

deriving instance Show Passport

type PassportError = Passport' Validate

deriving instance Show PassportError

defaultPassport :: Passport
defaultPassport =
  Passport
    { byr = Nothing,
      iyr = Nothing,
      eyr = Nothing,
      hgt = Nothing,
      hcl = Nothing,
      ecl = Nothing,
      pid = Nothing,
      cid = Nothing
    }

parsePassport :: [String] -> Passport
parsePassport parts = parts |> map (splitOn ":") |> foldr fillPassport defaultPassport
  where
    fillPassport ["byr", byr] p = p {byr = Just $ read byr}
    fillPassport ["iyr", iyr] p = p {iyr = Just $ read iyr}
    fillPassport ["eyr", eyr] p = p {eyr = Just $ read eyr}
    fillPassport ["hgt", hgt] p = p {hgt = Just hgt}
    fillPassport ["hcl", hcl] p = p {hcl = Just hcl}
    fillPassport ["ecl", ecl] p = p {ecl = Just ecl}
    fillPassport ["pid", pid] p = p {pid = Just pid}
    fillPassport ["cid", cid] p = p {cid = Just cid}
    fillPassport _ _ = error "Invalid passport attribute"

readInput :: IO [Passport]
readInput = do
  fileContents <- readFile input
  fileContents
    |> lines
    |> partitionBy (== "")
    |> filter (/= [""])
    |> map (\line -> line |> unwords |> words |> parsePassport)
    |> pure

required :: Monad m => Maybe a -> ExceptT String m (Maybe a)
required Nothing = throwE "can't be empty"
required (Just value) = pure (Just value)

inRange :: Monad m => Int -> Int -> Maybe Int -> ExceptT String m (Maybe Int)
inRange _ _ Nothing = pure Nothing
inRange min max (Just value)
  | min <= value && value <= max = pure (Just value)
  | otherwise = throwE "value out of range"

memberOf :: (Monad m, Foldable t, Eq a) => t a -> Maybe a -> ExceptT String m (Maybe a)
memberOf _ Nothing = pure Nothing
memberOf coll (Just value)
  | value `elem` coll = pure (Just value)
  | otherwise = throwE "value invalid"

hasFormat :: Monad m => String -> Maybe String -> ExceptT String m (Maybe String)
hasFormat _ Nothing = pure Nothing
hasFormat format (Just value)
  | value =~ format = pure (Just value)
  | otherwise = throwE "value invalid"

ignore :: Monad m => a -> ExceptT String m a
ignore = pure

isValid :: Validator i Identity a -> i -> Bool
isValid validator value =
  value
    |> validatePure validator
    |> isNothing

validatePart1 :: Monad m => Validator Passport m PassportError
validatePart1 =
  Passport
    <$> check byr required
    <*> check iyr required
    <*> check eyr required
    <*> check hgt required
    <*> check hcl required
    <*> check ecl required
    <*> check pid required
    <*> check cid ignore

solvePart1 :: [Passport] -> String
solvePart1 passports =
  passports
    |> filter (isValid validatePart1)
    |> length
    |> show

validHeight :: Monad m => Maybe String -> ExceptT String m (Maybe String)
validHeight Nothing = pure Nothing
validHeight (Just value) =
  if valid then pure (Just value) else throwE "invalid height"
  where
    valid =
      case value =~ "^([0-9]+)(in|cm)$" :: (String, String, String, [String]) of
        ("", _, "", [amount, "in"]) -> let parsedAmount = read amount in 59 <= parsedAmount && parsedAmount <= 76
        ("", _, "", [amount, "cm"]) -> let parsedAmount = read amount in 150 <= parsedAmount && parsedAmount <= 193
        _ -> False

validatePart2 :: Monad m => Validator Passport m PassportError
validatePart2 =
  Passport
    <$> checks byr [required, inRange 1920 2002]
    <*> checks iyr [required, inRange 2010 2020]
    <*> checks eyr [required, inRange 2020 2030]
    <*> checks hgt [required, validHeight]
    <*> checks hcl [required, hasFormat "^#[0-9a-f]{6}$"]
    <*> checks ecl [required, memberOf ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]]
    <*> checks pid [required, hasFormat "^[0-9]{9}$"]
    <*> check cid ignore

solvePart2 :: [Passport] -> String
solvePart2 passports =
  passports
    |> filter (isValid validatePart2)
    |> length
    |> show

runPart1 :: IO ()
runPart1 = do
  passports <- readInput
  putStrLn $ solvePart1 passports

runPart2 :: IO ()
runPart2 = do
  passports <- readInput
  putStrLn $ solvePart2 passports