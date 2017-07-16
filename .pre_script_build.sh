#!/bin/bash

GH_REPO="@github.com/waldronlab/curatedMetagenomicDataCuration.git"
FULL_REPO="https://$GH_TOKEN$GH_REPO"

pwd
cd ..
rm -rf out || exit 0;
mkdir out;

R CMD build curatedMetagenomicDataCuration

for files in '*.tar.gz'; do
        tar xfz $files
done

cd out
git init
git config user.name "lwaldron-travis"
git config user.email "travis"
cp ../curatedMetagenomicDataCuration/inst/doc/curatedMetagenomicDataCuration.html index.html

git add .
git commit -m "deployed vignette to github pages"
git push --force --quiet $FULL_REPO master:gh-pages

cd ..
pwd
rm *.tar.gz
cd curatedMetagenomicDataCuration/
pwd