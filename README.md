# PAPERBOY

![import screen](https://raw.githubusercontent.com/2mol/pboy/master/doc/import.png)

PaperBoy is a small .pdf management utility.

I was frustrated that most PDFs have pretty useless file names.

This tool helps to rename those files without too much fuss. It will rename/move files to a specified specified folder, and it even gives some suggestions for the filename by looking at the file metadata and the content.

PaperBoy aims to keep its file management dumb (no keeping files in a database or hidden library folder), so you can uninstall it at any time and your files will remain perfectly accessible.

# Usage

- Open a new file import dialog with <kbd>Space</kbd> or <kbd>Enter</kbd>.
- Switch between the library and the inbox with <kbd>Tab</kbd>.
- Open a file from the library with <kbd>Enter</kbd> or <kbd>Space</kbd>.
- Quit the application with <kbd>Esc</kbd> or <kbd>Ctrl + c</kbd>.

# Install

For now you need stack (cabal probably works too):

```
git clone github.com/2mol/pboy
cd pboy
stack install
```

This will give you an executable names `pboy` in your local bin folder.

# Config

PaperBoy creates a `.pboy.toml` in your home directory. Use this to change your library and incoming folders, as well as to specify whether you want to move the imported files or just copy them.

# Current Limitations

The program's state is very alpha. There is not a lot of exception handling for missing folders or missing utility programs.

Two command line tools are required for the suggestions: `pdftotext` and `pdfinfo`. Both come with the poppler package as far as I'm aware.

For large files, `pdftotext` can take quite a long time to parse the document.

# Contribute

You can open issues, send pull requests against the spec file https://github.com/2mol/pboy/blob/master/SPEC.md, and you're generally very welcome to share any opinions and fixes.

Some ideas for what the next priorities are:

- exception handling if `pdftotext` or `pdfinfo` are missing.
- nicer handling in case of a missing or malformed config file. Right now we simply (over)write the config file with a fresh one. It would be good to have an initial setup dialog asking for the inbox and library folder paths.

# Thanks

[`brick` is lovely](https://github.com/jtdaugherty/brick/).

The name for this tool is a nod to [this atrocity](https://en.wikipedia.org/wiki/Paperboy_(video_game)), which I had for the NES and never quite mastered.
