-- Copyright 2021 John MacFarlane (pandoc-server)
-- Copyright 2021 Lassi Kortela (pandoc-tar)
-- Copyright 2021 Wolfgang Corcoran-Mathe (pandoc-tar)
-- SPDX-License-Identifier: BSD-3-Clause

module Main where

import Control.Exception
import Control.Monad.Except
import Data.Text (Text)
import Data.Text.Encoding as TE
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import qualified Data.ByteString.Lazy as BSL
import System.FilePath
import System.IO

import qualified Codec.Archive.Tar as Tar
import qualified Codec.Archive.Tar.Entry as Tar.Entry
import Data.Aeson hiding (Options)
import Options.Applicative hiding (columns)
import Text.Pandoc

import Extensions

version :: String;
version = "pandoc-tar 0.1";

-- We use runPure for the pandoc conversions, which ensures that
-- they will do no IO.  This makes the server safe to use.  However,
-- it will mean that features requiring IO, like RST includes, will not work.
convertDocument :: Options -> FilePath -> Text -> Either PandocError Text;
convertDocument options fromPath text =
  runPure (convertDocument' options fromPath text)

get_input_format :: Maybe String -> FilePath -> Maybe String;
get_input_format (Just fmt) _        = Just fmt
get_input_format Nothing    fromPath = format_from_path fromPath

convertDocument' :: PandocMonad m => Options -> FilePath -> Text -> m Text;
convertDocument' options fromPath text =
  let { readerFormat' =
          fmap T.pack (get_input_format (from options) fromPath);
        writerFormat = T.pack (to options);
        isStandalone = standalone options }
  in do {
    readerFormat <- case readerFormat' of {
      Just rf -> return rf;
      _       -> throwError $
                   PandocAppError
                     (T.pack ("Cannot auto-detect format of " <>
                              (show fromPath))); };

    (readerSpec, readerExts) <- getReader readerFormat;
    (writerSpec, writerExts) <- getWriter writerFormat;

    mbTemplate <- if isStandalone;
      then case (template options) of {  -- TODO
        Nothing -> Just <$> compileDefaultTemplate writerFormat;
        Just t  -> do {
          res <- runWithPartials
            (compileTemplate ("custom." <> T.unpack writerFormat) t);
          case res of {
            Left  e   -> throwError $ PandocTemplateError (T.pack e);
            Right tpl -> return $ Just tpl }; } }
      else return Nothing;

    -- We don't yet handle binary formats.

    reader <- case readerSpec of {
      TextReader r -> return r;
      _            -> throwError $
                       (PandocAppError
                        (readerFormat <> (T.pack " is not a text reader"))) };

    writer <- case writerSpec of {
      TextWriter w -> return w;
      _            -> throwError $
                       (PandocAppError
                        (readerFormat <> (T.pack " is not a text writer"))) };

    reader (def { readerExtensions = readerExts
                , readerStandalone = isStandalone })
           text
      >>= writer def { writerExtensions = writerExts
                     , writerWrapText   = wrapText options
                     , writerColumns    = columns options
                     , writerTemplate   = mbTemplate
                     } }

convert_entry :: Options -> String -> Tar.Entry -> Tar.Entry;
convert_entry options toExt entry =
  let fromPath = Tar.Entry.entryPath entry in
    case Tar.entryContent entry of {
      Tar.NormalFile bytes _ ->
        convert_regular
           options
           fromPath
           (replaceExtension fromPath toExt)
           (TE.decodeUtf8 (BSL.toStrict bytes));
      Tar.Directory -> entry;
      ec -> error (fromPath ++ ": invalid filetype: " ++ show ec) }

convert_regular :: Options -> FilePath -> FilePath -> Text -> Tar.Entry;
convert_regular options fromPath toPath text =
  case convertDocument options fromPath text of {
    Left pe         -> error (T.unpack (renderError pe));
    (Right newText) ->
      let { tp = either error id (Tar.Entry.toTarPath False toPath) } in
        Tar.Entry.fileEntry tp
                            (TLE.encodeUtf8 (TL.fromStrict newText)) }

-- Fusion of Tar.mapEntries and list conversion, handling format errors.
maplist_entries :: (Tar.Entry -> a) -> Tar.Entries Tar.FormatError -> [a]
maplist_entries f =
  Tar.foldEntries (\e xs -> f e : xs)
                  []
                  (\err -> error ("Tar format error: " ++ describe err))

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

type Errors = String  -- TODO: Should be [{"filename": [error, ...]}, ...]

encode_errors_as_json :: Errors -> BSL.ByteString
encode_errors_as_json errors =
  Data.Aeson.encode errors

encode_errors_as_text :: Errors -> BSL.ByteString
encode_errors_as_text errors =
  TLE.encodeUtf8 (TL.pack (show errors))

write_errors :: Options -> Errors -> IO ()
write_errors options errors = do {
  case jsonLogFile options of {
    Nothing  -> BSL.hPutStr stderr (encode_errors_as_text errors);
    Just "-" -> BSL.hPutStr stderr (encode_errors_as_json errors);
    Just jsonLogFilePath ->
      System.IO.withFile
        jsonLogFilePath
        WriteMode
        (\h -> do BSL.hPutStr h (encode_errors_as_json errors)); } };

data Options = Options
  { verbose        :: Bool
  , jsonLogFile    :: Maybe String
  , from           :: Maybe String
  , to             :: String
  , wrapText       :: WrapOption
  , columns        :: Int
  , standalone     :: Bool
  , template       :: Maybe Text
  } deriving (Show)

cli_parser :: ParserInfo Options;
cli_parser =
  let { pv = infoOption version
                        (long "version" <> help "Show version.");
        p = Options
              <$> switch (short 'v'
                          <> long "verbose"
                          <> help "Write details to standard output")
              <*> (optional $
                     strOption
                       (long "log"
                         <> metavar "FILE"
                         <> help "Write JSON log to FILE ('-' is stderr"))
              <*> (optional $
                     strOption (short 'f'
                                <> long "from"
                                <> metavar "FORMAT"
                                <> help "Force input markup format"))
              <*> strOption
                  (short 't'
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
                          <> help "Produce stand-alone output documents.")
              <*> (optional $
                    option auto
                           (short 'm'
                            <> long "template"
                            <> metavar "TEMPLATE"
                            <> help "Pandoc template to use.")); }
  in
    info (helper <*> pv <*> p)
         (fullDesc <> header "pandoc-tar: pandoc over tar archives.");

get_output_extension :: String -> String;
get_output_extension fmt =
  case (extension_from_format fmt) of {
    Just toExt -> toExt;
    Nothing  ->
      error ("Unrecognized output format: " ++ fmt); }

failIfTerminal :: System.IO.Handle -> String -> IO ()
failIfTerminal fileHandle displayName = do {
  isTerminal <- hIsTerminalDevice fileHandle;
  when isTerminal $ do
    error (displayName <> " is a terminal"); }

main_with_options :: Options -> IO ();
main_with_options options = do {
  toExt    <- return (get_output_extension (to options));
  failIfTerminal System.IO.stdin "standard input";
  failIfTerminal System.IO.stdout "standard output";
  contents <- BSL.getContents;
  BSL.putStr
   (Tar.write
    (maplist_entries (convert_entry options toExt) (Tar.read contents))); }

main :: IO ();
main = do {
  options <- execParser cli_parser;
  catch (main_with_options options)
        (\e -> do write_errors options (show (e :: SomeException))); }
