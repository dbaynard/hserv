-- File: Main.hs
-- Copyright rejuvyesh <mail@rejuvyesh.com>, 2014

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE FlexibleContexts #-}
module Main where

import           Network.Wai.Application.Static       (defaultFileServerSettings,
                                                       staticApp, ssAddTrailingSlash)
import           Network.Wai.Handler.Warp             (run)
import           Network.Wai.Middleware.RequestLogger (logStdoutDev)
import           Paths_hserv                          (version)
import           Data.Version                         (showVersion)
import           System.Console.CmdArgs               (Data, Typeable, cmdArgs, help,
                                                       opt, summary, (&=), typ)
import           Network.Wai.Middleware.AddHeaders    (addHeaders)
import           Data.ByteString                      (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import           Data.Semigroup
import           Data.ByteString.Base64.URL.Type      (getEncodedByteString64, mkBS64)

data Hserv = Hserv
             { port :: Int
             , cookies :: [CookiePair]
             , verbose :: Bool
             }
             deriving (Data, Typeable)

type CookiePair = (String, FilePath)

type Header = (ByteString, ByteString)

main :: IO()
main = do
  hserv <- cmdArgs $ Hserv
           { port = 8888 &= help "Port on which server should run" &= opt (8888::Int)
           , cookies = [] &= help "List of (name, file) pairs for cookies; files will be base64 encoded."
               &= opt ([] :: [CookiePair]) &= typ "[COOKIE-NAME,FILE]"
           , verbose = False &= help "Log each request" }
           &= summary ("hserv " ++ showVersion version)
  let Hserv {port=p, cookies=c, verbose=v} = hserv
  let logging = if v then logStdoutDev else id
  cs <- readCookies c
  let cookied = if null cs then id else addHeaders cs
  let middleware = logging . cookied
  putStrLn $ "Running hserv on port " ++ (show p)
  putStrLn $ "Go to http://0.0.0.0:" ++ (show p)
  run p $ middleware $ staticApp 
        $ (defaultFileServerSettings ".") {ssAddTrailingSlash = True}

readCookies :: [CookiePair] -> IO [Header]
readCookies = traverse readCookie
  where
    readCookie (name, filepath) = do
      contents <- mkBS64 <$> BS.readFile filepath
      pure ("Set-Cookie", BS8.pack name <> "=" <> getEncodedByteString64 contents)

