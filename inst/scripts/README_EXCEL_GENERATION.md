# Dynamic Excel Template Generation for curatedMetagenomicData

## Overview

This package now includes functionality to dynamically generate the Excel data entry template (`cMD_data_entry_v7_4_2.xlsx`) based on:

1. **Data Dictionary** ([inst/extdata/cMD_data_dictionary.csv](inst/extdata/cMD_data_dictionary.csv))
2. **Ontology Lookup Service** (OLS) for dynamic enum values
3. **Existing Curated Metadata** ([inst/extdata/cMD_curated_metadata_release.csv](inst/extdata/cMD_curated_metadata_release.csv)) for large enums

This eliminates the need to manually maintain the Excel file and ensures that dropdown lists are always up-to-date with the latest ontology terms.

## Quick Start

### Prerequisites

```r
# Install required packages
install.packages(c("httr2", "readr", "openxlsx2"))
```

### Generate the Excel Template

#### Option 1: From Command Line

```bash
Rscript inst/scripts/generate_excel_template.R
```

#### Option 2: From R Console

```r
# Source the functions
source("R/generate_data_entry_excel.R")

# Generate the Excel file
result <- create_data_entry_excel(
  dict_file = "inst/extdata/cMD_data_dictionary.csv",
  metadata_file = "inst/extdata/cMD_curated_metadata_release.csv",
  output_file = "inst/extdata/cMD_data_entry_generated.xlsx"
)

# View summary
print(result)
```

### Test Before Generation

```bash
# Run component tests
Rscript inst/scripts/test_excel_generation.R
```

## How It Works

### 1. Data Dictionary Processing

The system reads [inst/extdata/cMD_data_dictionary.csv](inst/extdata/cMD_data_dictionary.csv) which defines:

- Column names and data types
- Validation rules (required, unique, etc.)
- Allowed values for enum fields
- Ontology mappings for dynamic enums

### 2. Ontology Lookup Service (OLS) Integration

For columns with `corpus.type = "dynamic_enum"`, the system:

- Queries the [EBI Ontology Lookup Service](https://www.ebi.ac.uk/ols4/) API
- Retrieves child or descendant terms based on `dynamic.enum.property`
- Formats results as "Label|OntologyID" for validation
- Falls back to existing curated values if the result set is too large (>1000 terms by default)

### 3. Smart Enum Handling

The system intelligently handles different enum types:

| Type | Source | Example |
|------|--------|---------|
| **static_enum** | Data dictionary | `sex`: Female, Male |
| **custom_enum** | Data dictionary | `dietary_restriction`: omnivore, vegetarian, vegan |
| **dynamic_enum** | OLS API or curated metadata | `disease`, `country`, `ancestry` |
| **dynamic_enum;static_enum** | Both OLS and dictionary | `treatment` |
| **binary** | Data dictionary | `antibiotics_current_use`: Yes, No |

### 4. Excel File Creation

The generated Excel file includes:

- **Curator_Entry**: Main data entry sheet with column headers and validation
- **Lists**: Hidden sheet containing all dropdown values
- **Export**: Formula-based projection of base columns from `Curator_Entry`
- **README**: Documentation and instructions
- **Data Validation Rules**: Dropdowns on appropriate columns (rows 3-10000)

## Key Functions

### `query_ols(ontology_ids, relationship, size_threshold)`

Queries the Ontology Lookup Service for terms.

**Parameters:**
- `ontology_ids`: Character vector of ontology IDs (e.g., "NCIT:C7057")
- `relationship`: "children" or "descendant"
- `size_threshold`: Maximum number of terms (default: 1000)

**Returns:** Character vector of "Label|ID" pairs, or NULL if too large

**Example:**
```r
# Get children of "Female" term
terms <- query_ols("NCIT:C16576", "children", 100)
```

### `get_curated_values(metadata_file, column_name)`

Extracts unique values from existing curated metadata.

**Parameters:**
- `metadata_file`: Path to curated metadata CSV
- `column_name`: Column to extract values from

**Returns:** Character vector of unique values

**Example:**
```r
# Get unique sex values
sex_values <- get_curated_values(
  "inst/extdata/cMD_curated_metadata_release.csv",
  "sex"
)
```

### `generate_validation_lists(dict_file, metadata_file, ols_size_threshold)`

Generates all validation lists based on data dictionary.

**Parameters:**
- `dict_file`: Path to data dictionary
- `metadata_file`: Path to curated metadata
- `ols_size_threshold`: Threshold for switching to curated values

**Returns:** Named list of validation options for each column

**Example:**
```r
lists <- generate_validation_lists(
  "inst/extdata/cMD_data_dictionary.csv",
  "inst/extdata/cMD_curated_metadata_release.csv",
  100
)

# View validation options for a specific column
print(lists$sex)
```

### `create_data_entry_excel(dict_file, metadata_file, output_file, ols_size_threshold, example_data)`

Main function to create the complete Excel file.

**Parameters:**
- `dict_file`: Path to data dictionary
- `metadata_file`: Path to curated metadata
- `output_file`: Path for output Excel file
- `ols_size_threshold`: OLS size threshold
- `example_data`: If `TRUE`, prefill rows 3-6 in `Curator_Entry` with demonstration entries

**Returns:** List with summary information

**Example:**
```r
result <- create_data_entry_excel(
  dict_file = "inst/extdata/cMD_data_dictionary.csv",
  metadata_file = "inst/extdata/cMD_curated_metadata_release.csv",
  output_file = "inst/extdata/cMD_data_entry_generated.xlsx",
  ols_size_threshold = 1000,
  example_data = TRUE
)
```

## Architecture

```
┌─────────────────────────────────────────────────────────────┐
│                    Data Dictionary CSV                       │
│  (defines columns, types, validation rules, ontologies)      │
└────────────────────┬────────────────────────────────────────┘
                     │
                     ▼
        ┌────────────────────────────┐
        │ generate_validation_lists()│
        └────────────┬───────────────┘
                     │
        ┌────────────┼────────────┐
        │            │            │
        ▼            ▼            ▼
  ┌─────────┐  ┌─────────┐  ┌─────────────┐
  │ Static  │  │   OLS   │  │  Curated    │
  │  Enum   │  │  API    │  │  Metadata   │
  └────┬────┘  └────┬────┘  └──────┬──────┘
       │            │              │
       └────────────┼──────────────┘
                    │
                    ▼
        ┌───────────────────────┐
        │ Validation Lists      │
        │ (named list)          │
        └──────────┬────────────┘
                   │
                   ▼
        ┌──────────────────────┐
        │ create_data_entry_   │
        │ excel()               │
        └──────────┬───────────┘
                   │
                   ▼
        ┌──────────────────────┐
        │  Excel File with     │
        │  Data Validation     │
        └──────────────────────┘
```

## Column Type Examples

### Static Enum

**Data Dictionary:**
```csv
col.name,corpus.type,allowedvalues,static.enum
sex,static_enum,Female|Male,NCIT:C16576|NCIT:C20197
```

**Result:** Dropdown with "Female|NCIT:C16576" and "Male|NCIT:C20197"

### Dynamic Enum (Small - uses OLS)

**Data Dictionary:**
```csv
col.name,corpus.type,dynamic.enum,dynamic.enum.property
ancestry,dynamic_enum,HANCESTRO:0004,children
```

**Process:**
1. Queries OLS for children of HANCESTRO:0004
2. Returns ~50 ancestry terms
3. Creates dropdown with all terms

### Dynamic Enum (Large - uses curated metadata)

**Data Dictionary:**
```csv
col.name,corpus.type,dynamic.enum,dynamic.enum.property
disease,dynamic_enum,NCIT:C7057;EFO:0000408,descendant
```

**Process:**
1. Queries OLS for descendants of NCIT:C7057 and EFO:0000408
2. Finds >1000 terms (exceeds threshold)
3. Falls back to existing values in curated metadata
4. Extracts unique disease values from existing data
5. Creates dropdown with those values only

### Mixed Enum

**Data Dictionary:**
```csv
col.name,corpus.type,allowedvalues,static.enum,dynamic.enum,dynamic.enum.property
treatment,dynamic_enum;static_enum,NA,NCIT:C41132,NCIT:C1908,descendant
```

**Process:**
1. Gets static enum: "NA|NCIT:C41132"
2. Queries OLS for NCIT:C1908 descendants
3. Combines both into one dropdown list

## Customization

### Adjust OLS Size Threshold

```r
# More aggressive use of curated metadata (fallback at 50 terms)
create_data_entry_excel(
  ols_size_threshold = 50
)

# More permissive (allow up to 200 OLS terms)
create_data_entry_excel(
  ols_size_threshold = 200
)
```

### Use Custom Files

```r
create_data_entry_excel(
  dict_file = "path/to/custom_dictionary.csv",
  metadata_file = "path/to/custom_metadata.csv",
  output_file = "path/to/custom_output.xlsx"
)
```

## Troubleshooting

### Issue: "Package 'openxlsx2' is required"

**Solution:**
```r
install.packages("openxlsx2")
```

### Issue: OLS queries failing

**Symptoms:** Warning messages about OLS API status codes

**Possible causes:**
1. No internet connection
2. OLS service temporarily unavailable
3. Invalid ontology IDs in data dictionary

**Solution:** The script will automatically fall back to using existing curated metadata values.

### Issue: Validation lists are empty

**Solution:** Check:
1. Data dictionary has correct ontology IDs in `dynamic.enum` column
2. Internet connection is working
3. Curated metadata file exists and is readable

### Issue: Excel file is too large

**Cause:** Too many validation options in dropdown lists

**Solution:**
1. Reduce `ols_size_threshold` to be more aggressive about using curated values
2. Manually edit data dictionary to limit ontology scope
3. Use more specific ontology terms instead of broad parent terms

## Performance

**Typical generation time:**
- Static enums only: < 1 minute
- With OLS queries: 5-15 minutes (depends on number of dynamic enums and network speed)
- Large metadata file: handled efficiently with streaming reads

**Optimization tips:**
- Run during off-peak hours if querying many ontologies
- Consider caching OLS results for repeated generations (future enhancement)
- Use curated metadata for very large ontologies

## Maintenance

### When to Regenerate

Regenerate the Excel template when:

1. **Data dictionary changes:** New columns added or validation rules modified
2. **Ontologies updated:** Want to include new terms from ontology releases
3. **Curated metadata grows:** New studies added with novel enum values
4. **Bugs fixed:** Issues found in the generation logic

### Version Control

The generated Excel file includes:
- README sheet with generation timestamp
- All source information (data dictionary version, ontology dates)

Consider:
- Committing both the generation script and the generated Excel file
- Tagging releases when significant template changes are made
- Documenting template version in filename or metadata

## Future Enhancements

Potential improvements:

- [ ] Cache OLS query results for faster regeneration
- [ ] Support for additional ontology services (BioPortal, OBO Foundry)
- [ ] Excel cell formatting and styling
- [ ] Conditional validation rules (dependent dropdowns)
- [ ] Automated testing of generated files
- [ ] Version tracking in Excel metadata
- [ ] Progress bar for long-running operations
- [ ] Parallel OLS queries for better performance

## Contributing

To improve the Excel generation system:

1. Modify [R/generate_data_entry_excel.R](R/generate_data_entry_excel.R)
2. Test with [inst/scripts/test_excel_generation.R](inst/scripts/test_excel_generation.R)
3. Update documentation as needed
4. Submit a pull request

## Support

For issues or questions:

- **GitHub Issues:** https://github.com/waldronlab/curatedMetagenomicDataCuration/issues
- **Documentation:** [inst/scripts/README_EXCEL_GENERATION.md](inst/scripts/README_EXCEL_GENERATION.md)

## References

- [Ontology Lookup Service (OLS) Documentation](https://www.ebi.ac.uk/ols4/help)
- [openxlsx2 Package Documentation](https://janmarvin.github.io/openxlsx2/)
- [curatedMetagenomicData Project](https://waldronlab.io/curatedMetagenomicData/)
