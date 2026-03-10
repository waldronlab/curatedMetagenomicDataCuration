[![DOI](https://zenodo.org/badge/95220777.svg)](https://doi.org/10.5281/zenodo.17498347)

# Curation
## Currently avaialble curated metadata
We are actively adding new curated microbiome metadata into curatedMetagenomicData.
You can check currently avialable sample-level metadata using the script below.

```
meta <- curatedMetagenomicDataCuration::makeCombinedMetadata()
meta
```

You can review the high-level details of them in [this page](https://waldronlab.io/curatedMetagenomicDataCuration/).


## Schema updating
We are implementing new metadata schema that is more compliant to FAIR 
principle. To help you understand this change, we linked the metadata tables 
for 93 studies in the prvious schema (`cMD3 version`) and in the updated 
schema (`cMD4 version`).

+ [cMD3 version](https://github.com/shbrief/curatedMetagenomicDataCuration/blob/master/inst/extdata/sampleMetadata.csv)
+ [cMD4 version](https://github.com/shbrief/curatedMetagenomicDataCuration/blob/master/inst/extdata/cMD_curated_metadata_release.csv)


# Local validation
```
Rscript R/scripts/validate_workflow.R
```

# To contribute

This package orchestrate cMD metadata curation processes, including 
initiation, status check, and deploy. To contribute a new data to cMD,

1. Look at the [data dictionary][]. You can reference [already curated studies][].

2. To add a new study, open an issue using the template _**'curatedMetagenomicData study curation by DOI'**_. 
This will create two curation template files in a directory under `inst/curated`.
  + the name of the dedicated directory for your study is `{last name of the first author}{initial of the first author’s first name}_{year published}` (e.g., 'AsnicarF_2017')
  + two files in this directories are `{dir_name}_study.tsv` and `{dir_name}_sample.tsv` 
  (e.g., 'AsnicarF_2017_study.tsv' and 'AsnicarF_2017_sample.tsv')

3. Starting from these templates, please curate your metadata. Commit and 
create a pull request. Or if you don't like git, use Github.com's 
["Upload Files" button](https://github.com/waldronlab/curatedMetagenomicDataCuration/tree/master/inst/curated) 
to upload to create a pull request without the need to use git.

4. Add a 'ready to review' label if your metadata is ready for the team's final 
review. cMD curation team will review/validate your metadata. If it passes, it 
will be added into cMD package. Any validation fails will be reported back to
you for your revision/update. For local validation, you can use `validateStudy` 
function:

```
validateStudy("/path/to/your/metadata.tsv")
```

[data dictionary]: https://github.com/waldronlab/curatedMetagenomicDataCuration/blob/master/inst/extdata/cMD_data_dictionary.csv
[already curated studies]: https://github.com/waldronlab/curatedMetagenomicDataCuration/tree/master/inst/curated



# More complete introduction

The complete reference for curators is provided at this page's 
[wiki](https://github.com/waldronlab/curatedMetagenomicDataCuration/wiki).
