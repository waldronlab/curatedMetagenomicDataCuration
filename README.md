&nbsp;

\ 

&nbsp;

This package checks metadata curation for curatedMetagenomicData. To use it:

1. Look at the [template](https://github.com/waldronlab/curatedMetagenomicDataCuration/blob/master/inst/extdata/template.csv) and [already curated files](https://github.com/waldronlab/curatedMetagenomicDataCuration/tree/master/inst/curated) to see what curated files should look like.
2. Place a tab-separated file ending in "_metadata.txt" in a directory under inst/curation. Commit and create a pull 
request.  Or if you don't like git, use Github.com's "Upload Files" button to upload at https://github.com/waldronlab/curatedMetagenomicDataCuration/tree/master/inst/curated to create a pull request without the need to use git.
3. Does the new dataset passed all checks? &nbsp; [![Travis-CI Build Status](https://travis-ci.org/waldronlab/curatedMetagenomicDataCuration.svg?branch=master)](https://travis-ci.org/waldronlab/curatedMetagenomicDataCuration)
4. If there are issues, you can see a detailed report by doing this:

```
BiocInstaller::biocLite("waldronlab/curatedMetagenomicDataCuration")
browseVignettes("curatedMetagenomicDataCuration")
```

There is an example vignette output at http://rpubs.com/lwaldron/curatedMetagenomicDataCuration.

For more information on contributing to `curatedMetagenomicData` please see https://waldronlab.github.io/curatedMetagenomicData/ and [CONTRIBUTING.md](https://github.com/waldronlab/curatedMetagenomicData/blob/master/CONTRIBUTING.md).
