module Extensions (extension_from_format, format_from_path)
  where

import System.FilePath

extension_from_format :: String -> Maybe String;
extension_from_format fmt = lookup fmt format_extensions;

-- The following are based on pandoc sources.  They will need
-- future curation.
format_extensions :: [(String, String)];
format_extensions =
   [ ("asciidoc", ".txt")   -- Correct, according to asciidoc's creator.
   , ("biblatex", ".bib")
   , ("context", ".ctx")
   , ("csv", ".csv")
   , ("doc", ".doc")
   , ("docbook", ".db")
   , ("docx", ".docx")
   , ("dokuwiki", ".dokuwiki")
   , ("epub", ".epub")
   , ("fb2", ".fb2")
   , ("html", ".html")
   , ("html", ".xhtml")
   , ("icml", ".icml")
   , ("ipynb", ".ipynb")
   , ("json", ".json")
   , ("latex", ".tex")
   , ("man", ".1")         -- Mindless default.
   , ("markdown", ".md")
   , ("markdown+lhs", ".lhs")
   , ("mediawiki", ".wiki")
   , ("ms", ".ms")
   , ("muse", ".muse")
   , ("native", ".native")
   , ("odt", ".odt")
   , ("opml", ".opml")
   , ("org", ".org")
   , ("pdf", ".pdf")
   , ("pptx", ".pptx")
   , ("rst", ".rst")
   , ("rtf", ".rtf")
   , ("s5", ".s5")
   , ("t2t", ".t2t")
   , ("tei", ".tei")
   , ("texinfo", ".texinfo")
   , ("textile", ".textile")];

format_from_path :: FilePath -> Maybe String;
format_from_path = format_from_extension . takeExtension

format_from_extension :: String -> Maybe String;
format_from_extension s =
  case s of {
    ".adoc"     -> Just "asciidoc";
    ".asciidoc" -> Just "asciidoc";
    ".context"  -> Just "context";
    ".ctx"      -> Just "context";
    ".db"       -> Just "docbook";
    ".doc"      -> Just "doc";  -- so we get an "unknown reader" error
    ".docx"     -> Just "docx";
    ".dokuwiki" -> Just "dokuwiki";
    ".epub"     -> Just "epub";
    ".fb2"      -> Just "fb2";
    ".htm"      -> Just "html";
    ".html"     -> Just "html";
    ".icml"     -> Just "icml";
    ".json"     -> Just "json";
    ".latex"    -> Just "latex";
    ".lhs"      -> Just "markdown+lhs";
    ".ltx"      -> Just "latex";
    ".markdown" -> Just "markdown";
    ".mkdn"     -> Just "markdown";
    ".mkd"      -> Just "markdown";
    ".mdwn"     -> Just "markdown";
    ".mdown"    -> Just "markdown";
    ".Rmd"      -> Just "markdown";
    ".md"       -> Just "markdown";
    ".ms"       -> Just "ms";
    ".muse"     -> Just "muse";
    ".native"   -> Just "native";
    ".odt"      -> Just "odt";
    ".opml"     -> Just "opml";
    ".org"      -> Just "org";
    ".pdf"      -> Just "pdf";  -- so we get an "unknown reader" error
    ".pptx"     -> Just "pptx";
    ".roff"     -> Just "ms";
    ".rst"      -> Just "rst";
    ".rtf"      -> Just "rtf";
    ".s5"       -> Just "s5";
    ".t2t"      -> Just "t2t";
    ".tei"      -> Just "tei";
    ".tei.xml"  -> Just "tei";
    ".tex"      -> Just "latex";
    ".texi"     -> Just "texinfo";
    ".texinfo"  -> Just "texinfo";
    ".text"     -> Just "markdown";
    ".textile"  -> Just "textile";
    ".txt"      -> Just "markdown";
    ".wiki"     -> Just "mediawiki";
    ".xhtml"    -> Just "html";
    ".ipynb"    -> Just "ipynb";
    ".csv"      -> Just "csv";
    ".bib"      -> Just "biblatex";
    ['.',y]     | y `elem` ['1'..'9'] -> Just "man";
    _           -> Nothing }
