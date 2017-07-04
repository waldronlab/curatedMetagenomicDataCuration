#!/bin/bash

rm -rf out || exit 0;
mkdir out;

GH_REPO="@github.com/waldronlab/curatedMetagenomicDataCuration.git"

FULL_REPO="https://$GH_TOKEN$GH_REPO"

for files in '*.tar.gz'; do
        tar xfz $files
done

cd out
git init
git config user.name "lwaldron-travis"
git config user.email "travis"
cp ../curatedMetagenomicDataCuration/inst/doc/curatedMetagenomicDataCuration.html docs/index.html

git add .
git commit -m "deployed vignette to github pages"
git push --force --quiet $FULL_REPO master
