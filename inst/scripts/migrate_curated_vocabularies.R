#!/usr/bin/env Rscript
##
## Migrate controlled-vocabulary values in inst/curated/ to the terms declared
## in inst/extdata/cMD_data_dictionary.csv.
##
## The dictionary moved dna_extraction_kit from short codes to full product
## names and respaced sequencing_platform to vendor spelling. The curated TSVs
## still carry the old values, so validation fails for them. This script renames
## the cell values in place; it changes no other column and no other file.
##
## Usage
##   Rscript inst/scripts/migrate_curated_vocabularies.R            # dry run
##   Rscript inst/scripts/migrate_curated_vocabularies.R --apply    # write
##   Rscript inst/scripts/migrate_curated_vocabularies.R --apply --path inst/curated/StudyX
##
## Properties
##   * Exact whole-cell matches only -- "PowerSoil" never touches "PowerSoilPro".
##   * Idempotent: the new values are not keys, so a second run is a no-op.
##   * Byte-preserving: only the target column's field is rewritten. Every other
##     field, the header, line endings and the final-newline state are untouched.
##   * Values with no mapping are reported and LEFT ALONE, never guessed at.

## ---- mappings ---------------------------------------------------------------
## old value (as curated) -> new value (as declared in the data dictionary)

MAPS <- list(
    dna_extraction_kit = c(
        "Qiagen"                = "QIAGEN",
        "QIAamp"                = "QIAamp",                     # already current
        "MoBio"                 = "MO BIO Laboratories",
        "MPBio"                 = "MP Biomedicals",
        "NorgenBiotek"          = "Norgen Biotek",
        "Tiangen"               = "TIANGEN",
        "Chemagen"              = "chemagen",
        "thermo_fisher"         = "Thermo Fisher Scientific",
        "Maxwell_LEV"           = "Maxwell LEV",
        "PowerSoil"             = "PowerSoil DNA Isolation Kit",
        "PowerSoilPro"          = "DNeasy PowerSoil Pro Kit",
        "PSP_Spin_Stool"        = "PSP Spin Stool DNA Plus Kit",
        "ZR_Fecal_DNA_MiniPrep" = "ZR Fecal DNA MiniPrep Kit",
        "KAMA_Hyper_Prep"       = "KAPA HyperPrep Kit"          # KAMA was a typo
    ),
    sequencing_platform = c(
        "IlluminaHiSeq"   = "Illumina HiSeq",
        "IlluminaMiSeq"   = "Illumina MiSeq",
        "IlluminaNextSeq" = "Illumina NextSeq",
        "IlluminaNovaSeq" = "Illumina NovaSeq",
        "IonProton"       = "Ion Torrent Proton",
        "BGISeq"          = "BGISEQ"
    )
)

## Values deliberately NOT mapped, because the dictionary has no counterpart and
## picking one would be a curation decision, not a rename. The script reports
## these and leaves the data untouched.
##   dna_extraction_kit: Gnome, Illuminakit, other
## A literal "NA" string also appears in dna_extraction_kit; that is a data
## defect (the cell should be empty) and is reported, not rewritten.

## ---- args -------------------------------------------------------------------

args  <- commandArgs(trailingOnly = TRUE)
apply <- "--apply" %in% args
root  <- if ("--path" %in% args) args[match("--path", args) + 1L] else "inst/curated"
if (!dir.exists(root)) stop("No such directory: ", root, call. = FALSE)

## ---- helpers ----------------------------------------------------------------

`%||%` <- function(a, b) if (is.null(a)) b else a

## Split a TSV line without losing trailing empty fields (strsplit drops them).
split_tsv <- function(line) {
    p <- strsplit(paste0(line, "\t\001"), "\t", fixed = TRUE)[[1]]
    p[-length(p)]
}

## Read a file, remembering whether it ended with a newline.
read_lines_keep_eof <- function(path) {
    raw <- readChar(path, file.size(path), useBytes = TRUE)
    Encoding(raw) <- "UTF-8"
    eof <- grepl("\n$", raw)
    txt <- sub("\n$", "", raw)
    list(lines = if (nzchar(txt)) strsplit(txt, "\n", fixed = TRUE)[[1]] else character(0),
         eof = eof)
}

write_lines_keep_eof <- function(lines, eof, path) {
    out <- paste(lines, collapse = "\n")
    if (eof) out <- paste0(out, "\n")
    con <- file(path, open = "wb"); on.exit(close(con))
    writeChar(out, con, eos = NULL, useBytes = TRUE)
}

## ---- walk -------------------------------------------------------------------

files <- list.files(root, pattern = "_(sample|study)[.]tsv$",
                    recursive = TRUE, full.names = TRUE)
message("Scanning ", length(files), " curated TSV files under ", root,
        if (apply) "  [APPLY]" else "  [dry run]")

changed    <- list()   # field -> old value -> n rows
unmapped   <- list()   # field -> old value -> studies
files_hit  <- character(0)

for (fp in files) {
    fdat  <- read_lines_keep_eof(fp)
    lines <- fdat$lines
    if (length(lines) < 2L) next
    header <- split_tsv(lines[1])
    study  <- basename(dirname(fp))
    dirty  <- FALSE

    for (fld in names(MAPS)) {
        j <- match(fld, header)
        if (is.na(j)) next
        map <- MAPS[[fld]]

        for (i in seq.int(2L, length(lines))) {
            parts <- split_tsv(lines[i])
            if (length(parts) < j) next
            val <- parts[j]
            if (!nzchar(trimws(val))) next

            if (val %in% names(map)) {
                new <- map[[val]]
                changed[[fld]][[val]] <- (changed[[fld]][[val]] %||% 0L) + 1L
                if (!identical(new, val)) {
                    parts[j] <- new
                    lines[i] <- paste(parts, collapse = "\t")
                    dirty <- TRUE
                }
            } else {
                allowed <- unname(map)
                if (!val %in% allowed) {
                    unmapped[[fld]][[val]] <- union(unmapped[[fld]][[val]] %||%
                                                        character(0), study)
                }
            }
        }
    }

    if (dirty) {
        files_hit <- c(files_hit, fp)
        if (apply) write_lines_keep_eof(lines, fdat$eof, fp)
    }
}

## ---- report -----------------------------------------------------------------

cat("\n==== renames ", if (apply) "applied" else "that would be applied",
    " ====\n", sep = "")
for (fld in names(changed)) {
    cat("## ", fld, "\n", sep = "")
    m <- MAPS[[fld]]
    for (v in names(changed[[fld]])) {
        tag <- if (identical(m[[v]], v)) "  (already current)" else ""
        cat(sprintf("   %-24s -> %-28s %5d rows%s\n", v, m[[v]],
                    changed[[fld]][[v]], tag))
    }
}

if (length(unmapped)) {
    cat("\n==== NOT migrated: no mapping declared ====\n")
    for (fld in names(unmapped)) {
        cat("## ", fld, "\n", sep = "")
        for (v in names(unmapped[[fld]])) {
            s <- unmapped[[fld]][[v]]
            cat(sprintf("   %-24s %2d studies: %s\n", v, length(s),
                        paste(s, collapse = ", ")))
        }
    }
    cat("   These need a curation decision (add the term to the dictionary, or\n",
        "   map it to an existing one), not a rename. Left untouched.\n", sep = "")
}

cat("\nfiles ", if (apply) "written" else "that would change", ": ",
    length(files_hit), "\n", sep = "")
if (!apply) cat("\nDry run. Re-run with --apply to write.\n")
