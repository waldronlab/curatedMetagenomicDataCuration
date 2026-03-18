### cMD_data_dictionary.csv
The most up-to-date data dictionary/schema for curatedMetagenomicDataCuration.
Validation is done based on this dictionary/schema.

### cMD_data_dictionary_codebook.csv
Codebook describing the meaning and allowed values of each column in
`cMD_data_dictionary.csv`. Columns documented: `col.name`, `col.class`,
`unique`, `required`, `multiplevalues`, `description`, `allowedvalues`,
`static.enum`, `dynamic.enum`, `dynamic.enum.property`, `delimiter`,
`separator`, `corpus.type`, `display.order`, `display.group`.


### cMD_curated_metadata_release.csv
The curated metadata, following the newest schema (`cMD_data_dictionary.csv`) 
in this directory. 


### sampleMetadata.csv
The curated sample-level metadata available through cMD3 package. This metadata
is following the cMD3 data schema (before harmonization). 