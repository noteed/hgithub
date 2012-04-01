{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import Data.Version (showVersion)
import Control.Applicative ((<$>))
import Paths_hgithub (version)
import System.Console.CmdArgs.Implicit
import System.Directory (doesFileExist)
import System.Exit

import Network.GitHub

main :: IO ()
main = (processCmd =<<) $ cmdArgs $
  modes
    [ cmdRepositoryList
    , cmdRepositoryCreate
    ]
  &= summary versionString
  &= program "hgithub"

versionString :: String
versionString =
  "hgithub " ++ showVersion version ++ " Copyright (c) 2012 Vo Minh Thu."

data Cmd =
    CmdRepositoryList
  | CmdRepositoryCreate
  { cmdRepositoryCreateName :: String
  , cmdRepositoryCreateDescription :: Maybe String
  }
  deriving (Data, Typeable)

cmdRepositoryList :: Cmd
cmdRepositoryList = CmdRepositoryList
  &= help "List repositories you have access to."
  &= explicit
  &= name "list-repositories"

cmdRepositoryCreate :: Cmd
cmdRepositoryCreate = CmdRepositoryCreate
  { cmdRepositoryCreateName = def
    &= typ "NAME"
    &= explicit
    &= name "name"
    &= help "Repository name."
  , cmdRepositoryCreateDescription = def
    &= typ "STRING"
    &= explicit
    &= name "description"
    &= help "Repository description."
  } &= help "Create a new repository."
    &= explicit
    &= name "create-repository"

processCmd :: Cmd -> IO ()
processCmd CmdRepositoryList{..} = do
  usernamePassword <- readUsernamePassword "github-username-password.txt"
  mrepos <- repositoryList usernamePassword
  case mrepos of
    Nothing -> putStrLn "Some error occured."
    Just repos -> mapM_ print repos

processCmd CmdRepositoryCreate{..} = do
  usernamePassword <- readUsernamePassword "github-username-password.txt"
  mrepo <- repositoryCreate usernamePassword
    cmdRepositoryCreateName
    cmdRepositoryCreateDescription
  case mrepo of
    Nothing -> putStrLn "Some error occured."
    Just repo -> print repo

readUsernamePassword :: FilePath -> IO String
readUsernamePassword filename = do
  b <- doesFileExist filename
  if b
    then (head . lines) <$> readFile filename
    else do
      putStrLn $ "API key file `" ++ filename ++ "` not found."
      exitWith (ExitFailure 1)
