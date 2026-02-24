# inst/ Directory Structure

This directory contains all data and scripts bundled with the
`curatedMetagenomicDataCuration` package.

---

## curated/

Study metadata in the **active curation format** (one `_metadata.tsv` per
study). These files follow the flat, single-table schema used in cMD3 and are
still maintained for studies that have not yet been fully migrated to the
harmonized schema.

Each subdirectory is named after the study (e.g. `ArtachoA_2021/`) and
contains a single file:

```
ArtachoA_2021/
└── ArtachoA_2021_metadata.tsv
```

---

## harmonized/

Study metadata in the **new harmonized schema**, split into three typed tables
per study. This is the target format for all studies going forward.

Each sub-directory contains up to three files:

```
AsnicarF_2017/
├── AsnicarF_2017_sample.tsv    # per-sample clinical/phenotypic metadata
├── AsnicarF_2017_sra_meta.tsv  # SRA run-level sequencing metadata
└── AsnicarF_2017_study.tsv     # study-level metadata (title, condition)
```

The `makeCombinedMetadata()` R function reads from this directory.

---

## legacy/

Archive of the **original curated metadata** files that were previously stored
in `inst/curated/` (cMD3 version) before migration to the harmonized schema. 
Includes `_metadata.tsv` files as well as any helper scripts (`.py`, `.txt`) 
that accompanied those studies.

These files are kept for reference and reproducibility but are no longer
actively maintained.

---

## extdata/

Reference data and data dictionary files used across the package.

| File | Description |
|---|---|
| `cMD_data_dictionary.csv` | Column definitions and controlled vocabularies for the cMD4 schema |
| `cMD_curated_metadata_release.csv` | Combined cMD3 metadata release following the data dictionary |
| `biosamples_catalog.csv` | Catalog of BioSample accessions |
| `sampleMetadata.csv` | Aggregated sample-level metadata |

---

## scripts/

Utility R scripts for package maintenance tasks.

| File | Description |
|---|---|
| `validate_harmonized_meta.R` | Validates all harmonized metadata files in the `inst/harmonized` directory |
