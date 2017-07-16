This package checks metadata curation for curatedMetagenomicData. To use it:

1. Look at the [template](https://github.com/waldronlab/curatedMetagenomicDataCuration/blob/master/inst/extdata/template.csv) and [already curated files](https://github.com/waldronlab/curatedMetagenomicDataCuration/tree/master/inst/curated) to see what curated files should look like.
2. Place a tab-separated file ending in "_metadata.tsv" in a directory under inst/curation. Commit and create a pull 
request.  Or if you don't like git, use Github.com's ["Upload Files" button](https://github.com/waldronlab/curatedMetagenomicDataCuration/tree/master/inst/curated) to upload to create a pull request without the need to use git.
3. Does the new dataset pass all checks? It takes 3-4 minutes for the check to be completed here: &nbsp; [![Travis-CI Build Status](https://travis-ci.org/waldronlab/curatedMetagenomicDataCuration.svg?branch=master)](https://travis-ci.org/waldronlab/curatedMetagenomicDataCuration)
4. See the [detailed syntax-checking report](https://waldronlab.github.io/curatedMetagenomicDataCuration).

If you don't want to wait for travisCI to finish checking, you can check on your own computer:

```
BiocInstaller::biocLite("waldronlab/curatedMetagenomicDataCuration", build_vignettes=TRUE)
browseVignettes("curatedMetagenomicDataCuration")
```

*Note:* the package gets built twice as a work-around to [this Travis-CI for R issue](https://github.com/travis-ci/travis-ci/issues/8038). If this issue is resolved, the build can be made a little faster by reverting to the .travis.yml and .push_gh_pages.sh of [this version](https://github.com/waldronlab/curatedMetagenomicDataCuration/tree/114e245308fa872a35b8eb96908f8ff61e041c8a).

For more information on contributing to [curatedMetagenomicData](https://waldronlab.github.io/curatedMetagenomicData), see [CONTRIBUTING.md](https://github.com/waldronlab/curatedMetagenomicData/blob/master/CONTRIBUTING.md).
