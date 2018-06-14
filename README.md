# PAPERBOY

![import screen](https://raw.githubusercontent.com/2mol/pboy/master/doc/import.png)

PaperBoy is a small .pdf management utility.

I was frustrated that most PDFs have pretty useless file names.

`paperboy` helps to rename those files without too much fuss. It will rename/move files to a specified specified folder, and it even gives some suggestions for the filename by looking at the file metadata and the content.

Paperboy aims to keep its file management dumb (no keeping files in a database or hidden library folder), so you can uninstall it at any time and your files will remain perfectly accessible.

# Config

PaperBoy creates a `.pboy.toml` in your home directory. Use this to change your library and incoming folders, as well as to specify whether you want to move the imported files or just copy them.

# Limitations

Current state is super alpha. There is not a lot of exception handling for missing folders or missing utility programs.

Two command line tools are required for the suggestions: `pdftotext` and `pdfinfo`. Both come with the poppler package as far as I'm aware.
