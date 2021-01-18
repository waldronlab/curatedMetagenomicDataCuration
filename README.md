# Quick Start

This package checks metadata curation for curatedMetagenomicData. To use it:

1. Look at the [template](https://github.com/waldronlab/curatedMetagenomicDataCuration/blob/master/inst/extdata/template.csv) and [already curated files](https://github.com/waldronlab/curatedMetagenomicDataCuration/tree/master/inst/curated) to see what curated files should look like.
2. Place a tab-separated file ending in "_metadata.tsv" in a directory under inst/curation. Commit and create a pull 
request.  Or if you don't like git, use Github.com's ["Upload Files" button](https://github.com/waldronlab/curatedMetagenomicDataCuration/tree/master/inst/curated) to upload to create a pull request without the need to use git.
3. Does the new dataset pass all checks? It takes 3-4 minutes for the check to be completed here: &nbsp; [![Travis-CI Build Status](https://travis-ci.org/waldronlab/curatedMetagenomicDataCuration.svg?branch=master)](https://travis-ci.org/waldronlab/curatedMetagenomicDataCuration)
4. See the [detailed syntax-checking report](https://waldronlab.io/curatedMetagenomicDataCuration/articles/curatedMetagenomicDataCuration.html).

If you don't want to wait for GitHub Actions to finish checking, you can check on your own computer:

```
BiocInstaller::biocLite("waldronlab/curatedMetagenomicDataCuration", build_vignettes=TRUE)
browseVignettes("curatedMetagenomicDataCuration")
```

# More complete introduction

The complete reference for curators is provided at this page's [wiki](https://github.com/waldronlab/curatedMetagenomicDataCuration/wiki).
