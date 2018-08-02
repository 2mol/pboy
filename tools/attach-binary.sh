set -o errexit -o verbose

if test ! "$TRAVIS_TAG"
then
  echo 'This is not a release build.'
elif test ! "$GITHUB_TOKEN"
then
  echo 'The GITHUB_TOKEN environment variable is not set!'
  exit 1
else
  echo "Attaching binary for $TRAVIS_OS_NAME to $TRAVIS_TAG..."
  OWNER="$(echo "$TRAVIS_REPO_SLUG" | cut -f1 -d/)"
  REPO_NAME="$(echo "$TRAVIS_REPO_SLUG" | cut -f2 -d/)"
  BUNDLE_NAME="$REPO_NAME-$TRAVIS_TAG-$TRAVIS_OS_NAME.tar.gz"
  cp "result/bin/$REPO_NAME" "."
  tar -czf "$BUNDLE_NAME" "$REPO_NAME"
  echo "SHA256:"
  shasum -a 256 "$BUNDLE_NAME"
  ghr -t "$GITHUB_TOKEN" -u "$OWNER" -r "$REPO_NAME" --replace "$(git describe --tags)" "$BUNDLE_NAME"
fi
