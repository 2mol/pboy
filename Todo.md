# PAPERBOY Todo


## Next release

- [x] show version number in the UI.
- [x] use xdg path for writing config file instead of home direcory.
    - [ ] show a screen on first launch to inform where the config file is stored.
    - [ ] change config format to config-ini while, ditch htoml.
- [ ] warn when importing an already existing filename.
- [x] use http://hackage.haskell.org/package/path for filepaths.
- [ ] asum for exception handling of missing executables.
- [x] 'Ctrl-o' to open the document while in the middle of a rename/import.
- [ ] https://vaibhavsagar.com/blog/2018/01/03/static-haskell-nix/
- [ ] ditch nix for releases, use https://github.com/tfausak/github-release
- [x] fix that there are more suggestions than space in the import screen


## Next-NEXT release

- [ ] improve color scheme
- [ ] ability to mark documents as 'to-read'.


## Done

- [x] use nix for CI and releases.
- [x] compiled releases for Mac & Linux so that people other than Haskellers with 24Gb worth of stack/GHC installs can actually use this.
- [x] homebrew for mac
- [x] exception handling if `pdftotext` or `pdfinfo` are missing.
- [x] pin nixpkgs version.


## Possible future features

- allow renaming files after they have been imported.
- refresh if any files move outside of the application.
- try out circleCI
- move away from ghr for releases and use inbuilt travis uploads instead.
- release .deb and .rpm packages.
- subfolders.
- make the utility work without a UI -> import documents using only command-line flags.
- search functionality -> build a search index based on the content we get from `pdftotext`.
- send files to a personalized email address, pboy will check and pull that.
- tag files to sync to phone (or kindle).
