# PBOY specifications

## Design goals

- minimize micro-management
- application should remain redundant: files are nicely named and accessible in a visible folder.


## Features

- renaming of files based on title.
- mapping between titles and filenames [UTF8, spaces allowed] <-> [ASCII and underscores instead of spaces]. To solve: won't be bijective
- can select a documents folder on first start: offer some sane defaults (Dropbox, OS Documents subfolder etc.)
- offers some title choices on import. based on pdftotext (poppler) for the first 4 or 10 lines that are not empty and longer than 5 characters or similar rule.
- can watch downloads folder and offer to ingest documents automatically as a sort of "Inbox"?
- keeps a copy of the original file that is strictly read-only. On edit, creates a copy (cf Lightroom). Warns if original somehow gets modified (keeps hash? - how do we link documents that are mostly "the same"?)

## Maybe

- tagging or organizing into subfolders
- 

