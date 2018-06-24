# PAPERBOY specifications

## Description

- `pboy` offers to import .pdf files from specified inbox folder (for example `~/Downloads`) into a documents folder.

- It helps rename the file according to its title. To that end it suggests possible titles extracted from the file metadata and the raw text content.


## Design goals

- avoid all temptations of micro-management.
- redundant application and not holding files hostage: all files are nicely named and accessible in a visible folder. Because of this, the application can be uninstalled at any time without losing any real data or organizational structure.
- put love and care towards documentation.


## Features

- renaming of files based on title.
- mapping between titles and filenames [UTF8, spaces allowed] -> [ASCII and underscores instead of spaces]. Problem: won't be bijective.
- can select a documents folder on first start: offer some sane defaults (Dropbox, OS Documents subfolder etc.).
- offers some title choices on import. based on pdftotext (poppler) for the first 4 or 10 lines that are not empty and longer than 5 characters or similar rule.
- can watch downloads folder and offer to ingest documents automatically as a sort of "Inbox"?
- keeps a copy of the original file that is strictly read-only. On edit, creates a copy (cf Lightroom). Warns if original somehow gets modified (keeps hash? - how do we link documents that are mostly "the same"?).


## Next actionable

- [ ] exception handling if `pdftotext` or `pdfinfo` are missing.
- [ ] warn when importing an already existing filename.
- [ ] option to open the document while in the middle of a rename/import. This way ambiguities about the title can be clarified.
- [ ] allow renaming files after they have been imported.
- [ ] refresh if any files move outside of the application.
- [ ] nicer "first-use experience": Right now we simply write a default config file. It would be good to have an initial setup dialog asking for the inbox and library folder paths.
- [ ] compiled releases for Mac & Linux so that people other than Haskellers with 24Gb worth of stack/GHC installs can actually use this.


## Possible future features

- ability to mark documents as 'to-read'.
- make the utility work without a UI -> import documents using only command-line flags.
- search functionality -> build a search index based on the content we get from `pdftotext`.
- tagging or organizing into subfolders.
- send files to a personalized email address, pboy will check and pull that.
- tag files to sync to phone (or kindle).


## Questions

- opening files: rely on `open` system command. Is that always available?

