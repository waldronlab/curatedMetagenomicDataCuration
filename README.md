[![Travis-CI Build Status](https://travis-ci.org/waldronlab/curatedMetagenomicData.svg?branch=master)](https://travis-ci.org/waldronlab/curatedMetagenomicData)

# curatedMetagenomicDataCuration

This package checks metadata curation for curatedMetagenomicData. It's simple to use:

1. Look at the [template](https://github.com/waldronlab/curatedMetagenomicDataCuration/blob/master/inst/extdata/template.csv) and [already curated files](https://github.com/waldronlab/curatedMetagenomicDataCuration/tree/master/inst/curated) to see what curated files should look like.
2. Place a tab-separated file ending in "_metadata.txt" in a directory under inst/curation.
3. Commit and push your changes. Or if you don't like git, use Github.com's "Upload Files" button to upload to https://github.com/waldronlab/curatedMetagenomicDataCuration/tree/master/inst/curated
4. Check the *Travis-CI build status* here to see whether the new dataset passed all checked.
5. If there are issues, you can see a detailed report by doing this:

```
BiocInstaller::biocLite("waldronlab/curatedMetagenomicDataCuration")
browseVignettes("curatedMetagenomicDataCuration")
```

For more information on contributing to `curatedMetagenomicData` please see https://waldronlab.github.io/curatedMetagenomicData/ and [CONTRIBUTING.md](https://github.com/waldronlab/curatedMetagenomicData/blob/master/CONTRIBUTING.md).
