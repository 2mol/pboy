# PAPERBOY Roadmap & Todo


## Future releases

- [ ] don't create any folders unless the first file import is triggered.
- [ ] improve performance of parsing large pdfs
- [ ] ability to mark documents as 'to-read'.
- [ ] warn when importing an already existing filename.
- [ ] refresh if any files move outside of the application.


## Done

- [x] show version number in the UI.
- [x] use xdg path for writing config file instead of home direcory.
- [x] use http://hackage.haskell.org/package/path for filepaths.
- [x] 'Ctrl-o' to open the document while in the middle of a rename/import.
- [x] go back to stack CI for release
- [x] fix that there are more suggestions than space in the import screen
- [x] rename existing files
- [x] improve color scheme
- [x] first-launch screen, show proposed config path, inbox and library paths, as well as keyboard shortcuts.
- [x] change config format to config-ini, ditch htoml
- [x] 'q' to exit app from main screen
- [x] ability to specify multiple folders as inboxes.
- [x] help screen

- [x] use nix for CI and releases.
- [x] compiled releases for Mac & Linux so that people other than Haskellers with 24Gb worth of stack/GHC installs can actually use this.
- [x] homebrew for mac
- [x] exception handling if `pdftotext` or `pdfinfo` are missing.
- [x] pin nixpkgs version.


## random ideas & wishes

want:

- release .deb and .rpm packages.
- make the utility work without a UI -> import documents using only command-line flags.
- search functionality -> build a search index based on the content we get from `pdftotext`.
- send files to a personalized email address, pboy will check and pull that.


maybe:

- try out circleCI
- move away from ghr for releases and use inbuilt travis uploads instead.
- use https://github.com/tfausak/github-release
- subfolders.
- tag files to sync to phone (or kindle).
- try various open commands with `asum` from `Data.Foldable`
- option to switch between underscores and spaces.