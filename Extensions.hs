module Extensions (extension_from_format, format_from_path)
  where

import Data.Char
import Data.List
import Data.Maybe
import System.FilePath

-- The following lookup table is based on the pandoc source file
-- "src/Text/Pandoc/App/FormatHeuristics.hs". It will need future
-- curation.
--
-- For each format, the first extension listed is the default one.
--
-- .1 is a mindless default for man.
-- .doc and .pdf give us an "unknown reader" error if we try to read them.
-- .txt is a correct extension for asciidoc, according to its creator.
format_extensions :: [(String, [String])];
format_extensions =
  [ ("asciidoc", [".adoc", ".asciidoc", ".txt"])
  , ("biblatex", [".bib"])
  , ("context", [".context", ".ctx"])
  , ("csv", [".csv"])
  , ("doc", [".doc"])
  , ("docbook", [".db"])
  , ("docx", [".docx"])
  , ("dokuwiki", [".dokuwiki"])
  , ("epub", [".epub"])
  , ("fb2", [".fb2"])
  , ("html", [".html", ".htm", ".xhtml"])
  , ("icml", [".icml"])
  , ("ipynb", [".ipynb"])
  , ("json", [".json"])
  , ("latex", [".tex", ".latex", ".ltx"])
  , ("man", [['.',i] | i <- ['1'..'9']])
  , ("markdown", [".md", ".markdown", ".mdown", ".mdwn", ".mkd", ".mkdn",
                  ".rmd", ".text", ".txt"])
  , ("markdown+lhs", [".lhs"])
  , ("mediawiki", [".wiki"])
  , ("ms", [".ms", ".roff"])
  , ("muse", [".muse"])
  , ("native", [".native"])
  , ("odt", [".odt"])
  , ("opml", [".opml"])
  , ("org", [".org"])
  , ("pdf", [".pdf"])
  , ("pptx", [".pptx"])
  , ("rst", [".rst"])
  , ("rtf", [".rtf"])
  , ("s5", [".s5"])
  , ("t2t", [".t2t"])
  , ("tei", [".tei", ".tei.xml"])
  , ("texinfo", [".texi", ".texinfo"])
  , ("textile", [".textile"])];

format_from_path :: FilePath -> Maybe String;
format_from_path = format_from_extension . takeExtension

format_from_extension :: String -> Maybe String;
format_from_extension ext =
  let lowerExt = map toLower ext
  in fmap fst (find (\(_, exts) -> lowerExt `elem` exts) format_extensions);

extension_from_format :: String -> Maybe String;
extension_from_format fmt =
  maybe Nothing listToMaybe (lookup fmt format_extensions);
