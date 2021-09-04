-- Copyright 2021 John MacFarlane (pandoc-server)
-- Copyright 2021 Lassi Kortela (pandoc-tar)
-- SPDX-License-Identifier: BSD-3-Clause

{-# LANGUAGE FlexibleContexts #-}

module Main where

import Text.Pandoc
import Data.Text (Text)
import Data.Text.Encoding
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import Data.Maybe (fromMaybe)
import Data.Char (isAlphaNum)

import qualified Codec.Archive.Tar as Tar
import qualified Codec.Archive.Tar.Entry as Tar.Entry
import qualified Data.ByteString.Lazy as BS
import System.Console.GetOpt
import System.Environment
import Control.Monad.Except

data Mode
  = Convert
  | Version
  | Help
  deriving (Show)

data Options = Options
  { mode           :: Mode
  , verbose        :: Bool
  , from           :: Maybe String
  , to             :: String
  , wrapText       :: Maybe WrapOption
  , columns        :: Maybe Int
  , standalone     :: Maybe Bool
  , template       :: Maybe Text
  } deriving (Show)

defaultOptions :: Options
defaultOptions = Options
  { mode       = Convert
  , verbose    = False
  , from       = Nothing
  , to         = "html"
  , wrapText   = Nothing
  , columns    = Nothing
  , standalone = Nothing
  , template   = Nothing
  }

-- We use runPure for the pandoc conversions, which ensures that
-- they will do no IO.  This makes the server safe to use.  However,
-- it will mean that features requiring IO, like RST includes, will not work.
-- Changing this to
--    handleErr =<< liftIO (runIO (convertDocument' options))
-- will allow the IO operations.
convertDocument :: MonadError (IO a) m => Options -> Text -> m Text
convertDocument options text =
  handleErr $ runPure (convertDocument' options text)

convertDocument' :: PandocMonad m => Options -> Text -> m Text
convertDocument' options text = do
  let readerFormat = T.pack (fromMaybe "markdown" $ from options)
  let writerFormat = T.pack (to options)
  (readerSpec, readerExts) <- getReader readerFormat
  (writerSpec, writerExts) <- getWriter writerFormat
  let isStandalone = fromMaybe False (standalone options)
  let toformat     = T.toLower $ T.takeWhile isAlphaNum $ writerFormat
  mbTemplate <- if isStandalone
    then case template options of
      Nothing -> Just <$> compileDefaultTemplate toformat
      Just t  -> do
        res <- runWithPartials
          (compileTemplate ("custom." <> T.unpack toformat) t)
        case res of
          Left  e   -> throwError $ PandocTemplateError (T.pack e)
          Right tpl -> return $ Just tpl
    else return Nothing
  -- We don't yet handle binary formats:
  reader <- case readerSpec of
    TextReader r -> return r
    _ ->
      throwError
        $  PandocAppError
        $  readerFormat
        <> (T.pack " is not a text reader")
  writer <- case writerSpec of
    TextWriter w -> return w
    _ ->
      throwError
        $  PandocAppError
        $  readerFormat
        <> (T.pack " is not a text reader")
  reader
      def { readerExtensions = readerExts, readerStandalone = isStandalone }
      text
    >>= writer def { writerExtensions = writerExts
                   , writerWrapText   = fromMaybe WrapAuto (wrapText options)
                   , writerColumns    = fromMaybe 72 (columns options)
                   , writerTemplate   = mbTemplate
                   }

handleErr :: MonadError (IO a1) m => Either PandocError a2 -> m a2
handleErr (Right t) = return t
handleErr (Left err) =
  throwError $ ioError (userError (T.unpack (renderError err)))

convertTarEntry :: Options -> Tar.Entry -> Tar.Entry
convertTarEntry options entry = case Tar.entryContent entry of
  Tar.NormalFile bytes _ ->
    ( ( let oldPath = Tar.Entry.entryPath entry
            newPath =
              ( case Tar.Entry.toTarPath False oldPath of
                Left  _        -> Tar.Entry.entryTarPath entry
                Right newPath' -> newPath'
              )
        in  case
              convertDocument
                options
                (Data.Text.Encoding.decodeUtf8 (BS.toStrict bytes))
            of
              Left _ -> entry
              Right newText ->
                ( Tar.Entry.fileEntry newPath
                                      (TLE.encodeUtf8 (TL.fromStrict newText))
                )
      )
    )
  _ -> entry

convertTarEntries :: Options -> Tar.Entries e -> [Tar.Entry.Entry]
convertTarEntries options entries = Tar.foldEntries
  (\entry newEntries -> (convertTarEntry options entry) : newEntries)
  []
  (\_error -> [])
  entries

commandLineOptions :: [OptDescr (Options -> IO Options)]
commandLineOptions =
  [ Option ['h']
           ["help"]
           (NoArg (\opt -> return opt { mode = Help }))
           "show usage and exit"
  , Option ['V']
           ["version"]
           (NoArg (\opt -> return opt { mode = Version }))
           "show version and exit"
  , Option ['v']
           ["verbose"]
           (NoArg (\opt -> return opt { verbose = True }))
           "write details to stderr"
  , Option ['f']
           ["from"]
           (ReqArg (\arg opt -> return opt { from = return arg }) "FORMAT")
           "read this markup format"
  , Option ['t']
           ["to"]
           (ReqArg (\arg opt -> return opt { to = arg }) "FORMAT")
           "write this markup format"
  ]

parseCommandLine :: [String] -> IO (Options)
parseCommandLine args = case getOpt Permute commandLineOptions args of
  (actions, [], []) -> do
    options <- foldl (>>=) (return defaultOptions) actions
    return options
  (_, nonOptions, errors) ->
    ioError (userError (concat errors ++ usageInfo header commandLineOptions))
  where header = "Usage: pandoc-tar [-f FORMAT] -t FORMAT <in.tar >out.tar"

main :: IO ()
main = do
  args     <- getArgs
  options  <- parseCommandLine args
  contents <- BS.getContents
  BS.putStr (Tar.write (convertTarEntries options (Tar.read contents)))
