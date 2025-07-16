# Quick Start

This package orchestrate cMD metadata curation processes, including 
initiation, status check, and deploy. To contribute a new data to cMD,
(Note: For cMD curation team, ODM should handle steps 3 and 4.)

1. Look at the [template][] and [already curated studies][] to see what curated files should look like.
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
you for your revision/update.

[template]: https://github.com/shbrief/curatedMetagenomicDataCuration/blob/master/inst/extdata/cMD_data_dictionary.csv
[already curated studies]: https://github.com/waldronlab/curatedMetagenomicDataCuration/tree/master/inst/curated


# Announcement
Starting from the upcoming cMD4 release (expected Fall 2025), we are 
implementing new metadata schema. To help you understand this change, here 
are the metadata tables for 93 cMD3 studies following the current and new 
metadata schema.

+ [current version](https://github.com/shbrief/curatedMetagenomicDataCuration/blob/master/inst/extdata/sampleMetadata.csv)
+ [new version](https://github.com/shbrief/curatedMetagenomicDataCuration/blob/master/inst/extdata/cMD_curated_metadata_release.csv)


# More complete introduction

The complete reference for curators is provided at this page's 
[wiki](https://github.com/waldronlab/curatedMetagenomicDataCuration/wiki).
