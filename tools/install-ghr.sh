set -o errexit -o verbose

if test ! "$TRAVIS_TAG"
then
  echo 'This is not a release build.'
else
    echo "Installing ghr"
    if [ "$TRAVIS_OS_NAME" = "linux" ]
    then
        URL="https://github.com/tcnksm/ghr/releases/download/v0.10.0/ghr_v0.10.0_linux_386.tar.gz"
        curl -L ${URL} > ghr.tgz
        tar xzvvf ghr.tgz
    else
        URL="https://github.com/tcnksm/ghr/releases/download/v0.10.0/ghr_v0.10.0_darwin_386.zip"
        curl -L ${URL} > ghr.zip
        unzip ghr.zip
    fi

  mkdir -p "$HOME/bin"
  mv */ghr "$HOME/bin/"
  export PATH="$HOME/bin:$PATH"

  rm -rf ghr*
fi

