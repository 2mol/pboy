# PBOY specifications

## Design goals

- minimize micro-management
- application should be removable: files are nicely named and accessible in a visible folder.
- great documentation.


## Features

- renaming of files based on title.
- mapping between titles and filenames [UTF8, spaces allowed] -> [ASCII and underscores instead of spaces]. Problem: won't be bijective.
- can select a documents folder on first start: offer some sane defaults (Dropbox, OS Documents subfolder etc.).
- offers some title choices on import. based on pdftotext (poppler) for the first 4 or 10 lines that are not empty and longer than 5 characters or similar rule.
- can watch downloads folder and offer to ingest documents automatically as a sort of "Inbox"?
- keeps a copy of the original file that is strictly read-only. On edit, creates a copy (cf Lightroom). Warns if original somehow gets modified (keeps hash? - how do we link documents that are mostly "the same"?).


## Possible future features

- tagging or organizing into subfolders.
- send files to a personalized email address, pboy will check and pull that.
- tag files to sync to phone (or kindle).
- personal (or global) machine learning model to categorize documents.


## Technical

- opening files: rely on `open` system command. > Is that always available? <
- title suggestions: pdftotext (poppler, not xpdf). > should/can we fork the code and ship it included? <

