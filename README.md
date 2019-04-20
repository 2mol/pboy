![import screen](https://raw.githubusercontent.com/2mol/pboy/master/doc/import.png)

[![Build Status](https://travis-ci.org/2mol/pboy.svg?branch=master)](https://travis-ci.org/2mol/pboy)

Paperboy is a tiny .pdf management utility.

If you download papers and other pdf documents, you might have noticed that filenames like `1412.4880.pdf` are not terribly helpful for finding anything later on. Especially if your download folder _also_ contains about eighty files along the lines of `catloaf.jpg`, `David_Lynch_Teaches_Typing.zip`, and `160502_0001.wav`.

This tool helps with that. It will offer to rename and move files to a specified folder, and it even gives some filename suggestions by looking at the content and the pdf metadata.

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

## Linux/Mac binary release

Download the archive for your operating system from [https://github.com/2mol/pboy/releases](https://github.com/2mol/pboy/releases). Extract and install it with

```
$ tar zxvf pboy*.tar.gz
$ mv pboy ~/.local/bin/
```

For the latter to work, `~/.local/bin/` needs to exist and be in your `PATH`. Alternatively, put it in `/usr/local/bin`.

## Linux, any distro

I am still looking to package Paperboy for Debian/Ubuntu, Arch/Manjaro, Fedora, Doge Linux, or whatever else people install these days.

Any pointers or help with regards to generate `.deb`, `.rpm`, AUR `PKGBUILD`, etc is appreciated. Ideally this could be mostly automated in CI, in the end Paperboy is just a single binary with a dependency or two. How do other packages do it? If you got a good example or link, open a GitHub issue!

## Cabal/Stack

Make sure you have `poppler` installed, which will provide both `pdftotext` and `pdfinfo`. On Linux, install `poppler` with your package manager of choice. If you are on Mac and using Homebrew you can do `brew install poppler`.

Assuming you have cabal or stack, the following will compile, then install the `pboy` executable in your `.local/bin`:

```
$ git clone git@github.com:2mol/pboy.git
$ cd pboy
$ stack install
```

Replace `stack install` with `cabal new-install` at your leisure.

## Nix

If you have Nix, then you can install `pboy` with a single command:

```
$ nix-env -if https://github.com/2mol/pboy/tarball/master
```

# Config

Paperboy creates a `pboy.ini` in your XDG config directory. This is probably in `~/.config/pboy/pboy.ini`, the welcome or help screen will tell you. Use this to change your library and incoming folders, as well as to specify whether you want to move the imported files or just copy them.

**Note**: The config file location & format changed in version 1.1. Sorry to existing users, but the update should be trivial. Simply tweak the inbox and library folders in the new config. You can then delete `~/.pboy.toml`.

# Current Limitations

Paperboy doesn't do anything fancy with providing renaming patterns yet. For example, some people requested to be able to specify a format like `author-document_name-date.pdf`, others have asked if they could compose multiple suggestions into one. I haven't figured out a way to do this while keeping the UI simple and straightforward, so the idea needs a bit of design work first.

# Contribute

You're very welcome to suggest new features or open issues. See the Roadmap https://github.com/2mol/pboy/blob/master/Roadmap.md to get an idea about what's planned for future releases.

# Thanks

- [brick](https://github.com/jtdaugherty/brick) is a lovely way to a write a command-line UI.
- [nmattia](https://github.com/nmattia) did the work to get Paperboy to build with [nix](https://github.com/NixOS/nix) and patiently explained some of the basics to me.

The name 'Paperboy' is a reference to [this game](https://en.wikipedia.org/wiki/Paperboy_(video_game)), which I had for the NES and never quite mastered.
