# pandoc-tar

## What is it?

`pandoc-tar` is a simple command line tool to help with batch
conversions of documents using [Pandoc](https://pandoc.org/). It reads
a [tar archive](https://en.wikipedia.org/wiki/Tar_(computing)) of
documents from standard input, converts them to the desired format,
and writes a tar archive of the converted documents to standard
output.

## Why do batch runs using stdin/stdout?

Running the `pandoc` command separately for each document can incur
several seconds' worth of startup overhead for a batch with tens of
documents. This is too much for interactive use.

Running `pandoc *.md`, i.e. giving many source filenames on the
command line, requires the source documents to be disk files, and the
destination documents will be concatenated into one file, making it
hard to split them apart later.

A separate tool called
[`pandoc-server`](https://github.com/jgm/pandoc-server) runs a web
server which provides a `/convert-batch` endpoint. This is fast and
avoids temp files, but even localhost-only web servers come with
security implications and other complexities.

## Why use tar as the format?

Tar is a simple, ubiquitous format that is easy to read and write from
countless programming languages. Tar files are well suited for pipes.

JSON would also be good for sending over a pipe, but is less standard
than tar, and how to handle mixed character encodings and arbitrary
binary data is less clear.

## Installation

You will need [`stack`](https://docs.haskellstack.org/en/stable/README/)
or `cabal`. With `stack`:

```
% stack install
```

A `pandoc-tar` executable will be put in `~/.local/bin`.

## Usage

```
produce-markdown-tar | pandoc-tar --from markdown --to json | consume-json-tar

-f FORMAT, -r FORMAT, --from=FORMAT, --read=FORMAT
-t FORMAT, -w FORMAT, --to=FORMAT, --write=FORMAT
```

## Acknowledgements

`pandoc-tar` is a straightforward adaptation of `pandoc-server` to use
stdio and tar instead of HTTP and JSON. `pandoc-server` is written by
Pandoc's author, John MacFarlane, who gracefully listened to but
ultimately denied my feature request to support tar in `pandoc`
itself.
