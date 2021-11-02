#! /usr/bin/env stack 
--stack --resolver lts-12.21 script --package ini --package directory --package text --package unordered-containers --package directory --package ansi-terminal --package either

{-# LANGUAGE OverloadedStrings #-}

import System.Environment (getArgs, lookupEnv)
import Data.Ini
import Data.Text
import qualified Data.HashMap.Strict as H
import System.Directory
import qualified Data.List as L
import System.Console.ANSI
import Control.Exception

data Command = Init | Set | Show | List deriving Show

eitherCredsFile :: IO (Either AWSProfileSetUpError String)
eitherCredsFile = do
      awsCredsFile <- lookupEnv "AWS_CREDENTIALS" 
      case awsCredsFile of
        Nothing  -> return $ Left envVarAndFileRequirements
        Just filepath -> return $ Right filepath

awsAccessKeyId :: Text
awsAccessKeyId = "aws_access_key_id"

awsSecretAccessKey :: Text
awsSecretAccessKey = "aws_secret_access_key"

type AWSProfileSetUpError = String
envVarAndFileRequirements :: AWSProfileSetUpError
envVarAndFileRequirements = "You must set the the environment variable:\n" ++
                            "AWS_CREDENTIALS=/absolute/path/to/your/.aws/credentials\n"
type Comment = String

currentProfileComment :: Text -> Comment
currentProfileComment profile = ";aws-profile: " ++ unpack profile

type InvalidProfileError = String

invalidProfileErr :: [String] -> InvalidProfileError
invalidProfileErr validProfiles = "You must include a valid profile name. Your available AWS profiles are: " ++ show validProfiles

validProfiles ini = L.filter (/= "default") (allIniKeys ini)

getEitherProfileArg :: [String] -> Ini -> IO (Either InvalidProfileError Text)
getEitherProfileArg args ini = do
    case args of
        [] -> return $ Left $ invalidProfileErr $ validProfiles ini
        x:_ -> if x `L.elem` validProfiles ini then return $ Right (pack x) else return $ Left $ invalidProfileErr $ validProfiles ini

catchExc :: IOException -> IO String
catchExc _ = return []

type Profile = String
parseComment :: String -> Either String Profile
parseComment comment = case L.words comment of
    [";aws-profile:", profile] -> Right profile
    _                        -> Left "You've not yet selected a profile using aws-profile."


allIniKeys :: Ini -> [String]
allIniKeys ini = fmap unpack (sections ini)

showProfileName :: String -> IO ()
showProfileName fp = do
    fileData <- catch (readFile fp) catchExc
    case fileData of
        [] -> putStrLn "You've not yet selected a profile using aws-profile."
        f -> case parseComment $ L.last $ L.lines f of
                Left err     -> putStrLn err
                Right profile -> do
                    setSGR [SetColor Foreground Vivid Green]
                    putStrLn $ "You're current default profile is " ++ profile ++ "."

listProfiles :: String -> IO ()
listProfiles fp = do
    errorOrIni <- readIniFile fp
    case errorOrIni of 
        Left err -> putStrLn err
        Right ini -> do
            setSGR [SetColor Foreground Vivid Green]
            mapM_ putStrLn (L.sort $ validProfiles ini)




setProfile :: [String] -> String -> IO ()
setProfile args awsCredsFile =  do
    errorOrIni <- readIniFile awsCredsFile
    case errorOrIni of
        Left err  -> putStrLn err
        Right ini -> do
            eitherProfileArg <- getEitherProfileArg args ini
            case eitherProfileArg of
                Left err -> putStrLn err
                Right profile -> do
                    let accessKey = lookupValue profile awsAccessKeyId ini
                    let secret =  lookupValue profile awsSecretAccessKey ini
                    case (accessKey, secret) of
                        (Left err, _) -> print err
                        (_, Left err) -> print err
                        (Right a, Right s) -> do
                            let newIni = Ini {unIni = H.insert "default" (H.fromList [(awsAccessKeyId, a), (awsSecretAccessKey, s)]) (unIni ini)}
                            let settings = WriteIniSettings EqualsKeySeparator
                            writeFile awsCredsFile $ unpack $ printIniWith settings newIni
                            appendFile awsCredsFile $ currentProfileComment profile
                            
                            
                            -- sets output text to green
                            setSGR [SetColor Foreground Vivid Green]
                            putStrLn $ "Your AWS profile is set to " ++ unpack profile


parseCommand :: String -> Either String Command          
parseCommand str = case str of
    "set"  -> Right Set
    "show" -> Right Show
    "list" -> Right List
    _      -> Left str

commands :: [String]
commands = ["set", "show", "list"]

main :: IO ()
main = do
    -- sets output text to red for errors
    setSGR [SetColor Foreground Vivid Red]
    args <- getArgs
    case args of 
        []   -> putStrLn $ "You must specify a command. Supported commands are: " ++ show commands
        x:xs -> do
            let cmd = parseCommand x
            awsCredsFile <- eitherCredsFile
            case (cmd, awsCredsFile) of
                (_, Left err) -> putStrLn err
                (Left str, _)    -> putStrLn $ str ++ " is not a valid command. Supported comands are: " ++ show commands
                (Right Set, Right file)   -> setProfile xs file 
                (Right List, Right file)  -> listProfiles file
                (Right Show, Right file)  -> showProfileName file


   