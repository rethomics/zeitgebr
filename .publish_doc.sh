#!/bin/sh

set -e

echo "Working on branch $TRAVIS_BRANCH"
echo "R version: $TRAVIS_R_VERSION"

[ -z "${GITHUB_PAT}" ] && exit 0
echo "GITHUB_PAT: OK"
[ "${TRAVIS_BRANCH}" != "master" ] && exit 0
echo "BRANCH: master"

git config user.name "rapporter-travis"
git config user.email "travis"

ls 

cd 
git clone -b master https://${GITHUB_PAT}@github.com/${TRAVIS_REPO_SLUG}.git repo.git
cd repo.git
make clean
make pdf
make README.md
git add *.pdf README.md
git commit -m"Automatic deployment after $TRAVIS_COMMIT [ci skip]" || true
git push -q origin master
