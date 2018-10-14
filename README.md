# PAPERBOY

![import screen](https://raw.githubusercontent.com/2mol/pboy/master/doc/import.png)

[![Build Status](https://travis-ci.org/2mol/pboy.svg?branch=master)](https://travis-ci.org/2mol/pboy)

Paperboy is a tiny .pdf management utility.

I wanted a way to efficiently rename and file the mess of pdf documents piling up in my download folder. Since they tend to have names like 'I08.pdf' and '1412.4880.pdf', it becomes tedious to find the files saved for later reading.

This tool helps with that. It will rename/move documents to a specified folder, and it even gives some filename suggestions by looking at the file content and the pdf metadata.

Paperboy keeps its file management dumb on purpose (no keeping files in a database or hidden library folder), so you can uninstall it at any time and your files will remain perfectly accessible.

# Usage

- Open a new file import dialog with <kbd>Enter</kbd> or <kbd>Space</kbd>.
- Switch between inbox and library with <kbd>Tab</kbd>.
- Open a file from the library with <kbd>Enter</kbd> or <kbd>Space</kbd>.
- Rename an already imported file with <kbd>r</kbd>.
- Quit the application with <kbd>Esc</kbd> or <kbd>q</kbd> or abort with <kbd>Ctrl + c</kbd>.

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

If you have Nix, then you can install `pboy` with one command:

```
$ nix-env -if https://github.com/2mol/pboy/tarball/master
```

If you only build it, then simply do

```
$ git clone git@github.com:2mol/pboy.git
$ cd pboy
$ nix-build
```

# Config

Paperboy creates a `.pboy.toml` in your home directory. Use this to change your library and incoming folders, as well as to specify whether you want to move the imported files or just copy them.

# Current Limitations

For large files, `pdftotext` can take quite a long time to parse the document, which is stupid because we're only using the first couple of lines for file name suggestions.

# Contribute

You're very welcome to open issues, fix the Readme or send pull requests.

If you have feature suggestions, please open an issue, or send a PR against the todo file https://github.com/2mol/pboy/blob/master/Todo.md.

If you are just looking for a place to help, please see the abovementioned document to get an idea of what some of the next priotities are.

# Thanks

- [brick](https://github.com/jtdaugherty/brick) is a lovely way to a write a command-line UI.
- [nmattia](https://github.com/nmattia) did the work to get Paperboy to build with [nix](https://github.com/NixOS/nix) and patiently explained some of the basics to me.

The name 'Paperboy' is a reference to [this game](https://en.wikipedia.org/wiki/Paperboy_(video_game)), which I had for the NES and never quite mastered.
