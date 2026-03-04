# inst/ Directory Structure

This directory contains all data and scripts bundled with the
`curatedMetagenomicDataCuration` package.

---

## curated/

Study metadata in the **final curation format**, split into three typed tables
per study. This is the target format for all studies going forward.

Each sub-directory contains up to three files:

```
AsnicarF_2017/
├── AsnicarF_2017_sample.tsv    # per-sample clinical/phenotypic metadata
├── AsnicarF_2017_sra_meta.tsv  # SRA run-level sequencing metadata
```

The `makeCombinedMetadata()` R function reads from this directory.

---

## extdata/

---

## scripts/

Utility R scripts for package maintenance tasks.

| File | Description |
|---|---|
| `validate_harmonized_meta.R` | Validates all harmonized metadata files in the `inst/curated` directory |

