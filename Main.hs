-- Copyright 2021 John MacFarlane (pandoc-server)
-- Copyright 2021 Lassi Kortela (pandoc-tar)
-- Copyright 2021 Wolfgang Corcoran-Mathe (pandoc-tar)
-- SPDX-License-Identifier: BSD-3-Clause

{-# LANGUAGE FlexibleContexts #-}

module Main where

import Text.Pandoc
import Data.Text (Text)
import Data.Text.Encoding
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import Data.Char (isAlphaNum)

import qualified Codec.Archive.Tar as Tar
import qualified Codec.Archive.Tar.Entry as Tar.Entry
import qualified Data.ByteString.Lazy as BS
import Options.Applicative hiding (columns)
import Control.Monad.Except

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
convertDocument' options text =
  let { readerFormat = from options;
        writerFormat = to options;
        isStandalone = standalone options;
        toformat     = T.toLower $ T.takeWhile isAlphaNum $ writerFormat }
  in do {
    (readerSpec, readerExts) <- getReader readerFormat;
    (writerSpec, writerExts) <- getWriter writerFormat;

    mbTemplate <- if isStandalone;
      then case Nothing of {  -- TODO
        Nothing -> Just <$> compileDefaultTemplate toformat;
        Just t  -> do {
          res <- runWithPartials
            (compileTemplate ("custom." <> T.unpack toformat) t);
          case res of {
            Left  e   -> throwError $ PandocTemplateError (T.pack e);
            Right tpl -> return $ Just tpl }; } }
      else return Nothing;

    -- We don't yet handle binary formats:
    reader <- case readerSpec of {
      TextReader r -> return r;
      _ ->
        throwError
          $  PandocAppError
          $  readerFormat
          <> (T.pack " is not a text reader") };

    writer <- case writerSpec of {
      TextWriter w -> return w;
      _ ->
        throwError
          $  PandocAppError
          $  readerFormat
          <> (T.pack " is not a text reader") };

    reader
        def { readerExtensions = readerExts, readerStandalone = isStandalone }
        text
      >>= writer def { writerExtensions = writerExts
                     , writerWrapText   = wrapText options
                     , writerColumns    = columns options
                     , writerTemplate   = mbTemplate
                     } }

handleErr :: MonadError (IO a1) m => Either PandocError a2 -> m a2
handleErr (Right t) = return t
handleErr (Left err) =
  throwError $ ioError (userError (T.unpack (renderError err)))

convert_entry :: Options -> Tar.Entry -> Tar.Entry;
convert_entry options entry = case (Tar.entryContent entry) of {
  Tar.NormalFile bytes _ ->
    let { path = translate_path False (Tar.Entry.entryPath entry) } in
      convert_regular options
         (Data.Text.Encoding.decodeUtf8 (BS.toStrict bytes))
         path;
  _ -> entry }

-- The check here becomes relevant when we are converting between
-- different tar, since the valid path length may differ.
translate_path :: Bool -> FilePath -> Tar.Entry.TarPath;
translate_path is_dir old = either error id (Tar.Entry.toTarPath is_dir old)

convert_regular :: Options -> Text -> Tar.Entry.TarPath -> Tar.Entry;
convert_regular options text path =
  case (convertDocument options text) of {
    Left _          -> error "Decoding failed.";  -- TODO
    (Right newText) -> Tar.Entry.fileEntry path
                         (TLE.encodeUtf8 (TL.fromStrict newText)) }

convert_entries :: Options -> Tar.Entries Tar.FormatError -> [Tar.Entry.Entry];
convert_entries options = Tar.foldEntries
  (\entry newEntries -> (convert_entry options entry) : newEntries)
  []
  (\err -> error ("Error while reading archive: " ++ describe err));

-- Basic pretty-printing of FormatErrors.
describe :: Tar.FormatError -> String;
describe Tar.TruncatedArchive         = "Truncated archive.";
describe Tar.ShortTrailer             = "Short trailer.";  -- What?
describe Tar.BadTrailer               = "Bad trailer.";    -- Ditto.
describe Tar.TrailingJunk             = "Trailing junk data.";
describe Tar.ChecksumIncorrect        = "Invalid checksum.";
describe Tar.NotTarFormat             = "Input is not valid tar data.";
describe Tar.UnrecognisedTarFormat    = "Unrecognized tar file format.";
describe Tar.HeaderBadNumericEncoding = "Invalid tar header.";

data Options = Options
  { verbose        :: Bool
  , from           :: String
  , to             :: String
  , wrapText       :: WrapOption
  , columns        :: Int
  , standalone     :: Bool
-- TODO
--  , template       :: Text
  } deriving (Show)

cli_parser :: ParserInfo Options;
cli_parser =
  let { pv = infoOption "pandoc-tar 0.1"
                        (long "version" <> help "Show version.");
        p = Options
              <$> switch (short 'v'
                          <> long "verbose"
                          <> help "Write details to standard output")
              <*> strOption (short 'f'
                             <> long "from"
                             <> metavar "FORMAT"
                             <> value "markdown"
                             <> showDefaultWith id
                             <> help "Input markup format")
              <*> strOption (short 't'
                             <> long "to"
                             <> metavar "FORMAT"
                             <> help "Output markup format")
              <*> option auto
                         (short 'w'
                          <> long "wrap"
                          <> metavar "WRAPOPT"
                          <> value WrapAuto
                          <> showDefault
                          <> help "Text-wrapping style for output.")
              <*> option auto
                         (short 'c'
                          <> long "columns"
                          <> metavar "INT"
                          <> value 72
                          <> showDefault
                          <> help "Width of output in columns.")
              <*> switch (short 's'
                          <> long "standalone"
                          <> help "Produce stand-alone output documents."); }
  in
    info (helper <*> pv <*> p)
         (fullDesc <> header "pandoc-tar: pandoc over tar archives.");

main :: IO ();
main = do {
  options  <- execParser cli_parser;
  contents <- BS.getContents;
  BS.putStr (Tar.write (convert_entries options (Tar.read contents))); }
