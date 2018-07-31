# PAPERBOY

![import screen](https://raw.githubusercontent.com/2mol/pboy/master/doc/import.png)

[![Build Status](https://travis-ci.org/2mol/pboy.svg?branch=master)](https://travis-ci.org/2mol/pboy)

Paperboy is a small .pdf management utility.

I was frustrated that most PDFs have pretty useless file names.

This tool helps renaming those files without too much fuss. It will rename/move documents to a specified folder, and it even gives some filename suggestions by looking at the file content and the pdf metadata.

Paperboy aims to keep its file management dumb (no keeping files in a database or hidden library folder), so you can uninstall it at any time and your files will remain perfectly accessible.

# Usage

- Open a new file import dialog with <kbd>Space</kbd> or <kbd>Enter</kbd>.
- Switch between the library and the inbox with <kbd>Tab</kbd>.
- Open a file from the library with <kbd>Enter</kbd> or <kbd>Space</kbd>.
- Quit the application with <kbd>Esc</kbd> or <kbd>Ctrl + c</kbd>.

# Install

## Homebrew on macOS

If you're a Homebrew user, you can install the latest version and its dependencies from the repo's tap:

```
$ brew install 2mol/tools/pboy
```

## Get the binary release

Download the archive for your operating system from [https://github.com/2mol/pboy/releases](https://github.com/2mol/pboy/releases). Extract and install it with

```
$ tar zxvf pboy*.tar.gz
$ mv pboy ~/.local/bin/
```

For the latter to work, `~/.local/bin/` needs to exist and be in your `PATH`. Alternatively, put it in `/usr/local/bin`.

## Stack

Make sure you have `poppler` installed, which will provide both `pdftotext` and `pdfinfo`. On Linux, install `poppler` with your package manager of choice. If you are on Mac and using Homebrew you can do `brew install poppler`.

Assuming you have stack, the following will compile, then install the `pboy` executable in your `.local/bin`:

```
$ git clone git@github.com:2mol/pboy.git
$ cd pboy
$ stack install
```

## Nix

If you have Nix, then you can build with:

```
$ git clone git@github.com:2mol/pboy.git
$ cd pboy
$ nix-build
```

Alternatively, you can  XXX TODO

# Config

Paperboy creates a `.pboy.toml` in your home directory. Use this to change your library and incoming folders, as well as to specify whether you want to move the imported files or just copy them.

# Current Limitations

For large files, `pdftotext` can take quite a long time to parse the document, which is stupid because we're only using the first couple of lines for file name suggestions.

# Contribute

Feel free to open issues, fix the Readme or send pull requests against the spec file https://github.com/2mol/pboy/blob/master/Spec.md. You're generally very welcome to share any opinions, documentation improvements, fixes, refactoring suggestions etc.

See the abovementioned document to get an idea of what some of the next priotities are, especially the section **Next actionable**.

# Thanks

[`brick` is lovely](https://github.com/jtdaugherty/brick/).

The name for this tool is inspired by [this atrocity](https://en.wikipedia.org/wiki/Paperboy_(video_game)), which I had for the NES and never quite mastered.
