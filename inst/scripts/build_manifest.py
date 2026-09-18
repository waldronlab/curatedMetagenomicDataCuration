#!/usr/bin/env python3
#
# build_manifest.py
# -----------------------------------------------------------------------------
# MAINTENANCE GENERATOR for the study reconciliation manifest (run by a
# maintainer from the repository root; commit the output).
#
# Answers the question raised in the 2026-09-18 cMD coordination meeting —
# "what samples in curatedMetagenomicDataCuration have already been processed
# through the Nextflow pipeline?" — by joining the curated corpus against the
# live pipeline telemetry, and writes:
#
#     inst/extdata/studies_status.csv
#         One row per curated study. Identity (uuid, study_name, study_id),
#         size, provenance, curation state, pipeline state, and placeholders
#         for profile versions/locations that only Sean and Francesco can fill.
#
#     inst/extdata/studies_runs.tsv
#         Long-form study_name -> run_accession lookup. Run accessions live
#         here and NOT in study_id, so the manifest stays small and study_id
#         holds an actual study identifier (BioProject).
#
# Telemetry source: https://nf-telemetry.cancerdatasci.org (public, no auth).
# Pass --offline to rebuild from a previously saved samples.json instead.
#
# Usage:
#     python3 inst/scripts/build_manifest.py
#
# -----------------------------------------------------------------------------

import argparse
import csv
import json
import os
import re
import sys
import urllib.request
import uuid
from collections import defaultdict

TELEMETRY = "https://nf-telemetry.cancerdatasci.org"
CURATED = os.path.join("inst", "curated")
OUTDIR = os.path.join("inst", "extdata")
CACHE = os.path.join(OUTDIR, ".telemetry_samples.json")

# Stable namespace so study UUIDs are reproducible across regenerations.
NS = uuid.uuid5(uuid.NAMESPACE_DNS, "curatedmetagenomicdata.waldronlab.org")

RUN_RE = re.compile(r"\b([SED]RR\d+)\b")

# Studies whose curation originated in ASAP-MAC/parkinsonsManualCuration.
# Kept explicit rather than inferred from `disease`, because the question is
# where the curation came from, not what the study is about.
PMD_SOURCE = {
    "AsnicarF_2021", "BedarfJR_2017", "BoktorJC_2023",
    "DeCastroFonsecaM_2026", "DumitrescuDG_2023", "DuruIC_2024", "JoS_2022",
    "LeeEJ_2024", "MaoL_2021", "MoiseyenkoA_2026", "NishiwakiH_2024",
    "QianY_2020", "SampsonTR_2025", "SchonhoffA_2023", "WallenZD_2022",
    "ZhangM_2023",
}

# Curated in both corpora (shared control cohorts).
SHARED = {"AsnicarF_2021"}

FIELDS = [
    "uuid", "study_name", "study_id", "n_samples", "n_runs",
    "source_project", "curation_available", "ci_validated",
    "primary_disease", "body_site", "country", "sequencing_platform", "pmid",
    "metaphlan_version", "metaphlan_location",
    "humann_version", "humann_location",
    "n_runs_processed", "telemetry_collections", "processing_status",
    "runs_per_sample", "notes",
]


def read_table(path):
    with open(path, newline="", errors="replace") as fh:
        head = fh.readline()
        fh.seek(0)
        delim = "\t" if "\t" in head else ","
        return list(csv.DictReader(fh, delimiter=delim))


def curated_file(study_dir):
    """The curated sample table, whichever naming convention it uses.

    Prefer `<study>_sample.tsv` (what the CI validator globs) when present.
    A few studies carry both that and a legacy `<study>.tsv`; picking
    alphabetically would wrongly select the legacy file and report the study
    as unvalidated. `<study>_study.tsv` is study-level, not sample-level.
    """
    candidates = [n for n in sorted(os.listdir(study_dir))
                  if n.endswith(".tsv")
                  and not n.endswith("_sra_meta.tsv")
                  and not n.endswith("_study.tsv")]
    if not candidates:
        return None
    preferred = [n for n in candidates if n.endswith("_sample.tsv")]
    return os.path.join(study_dir, (preferred or candidates)[0])


def distinct(rows, field, limit=5):
    seen = []
    for r in rows:
        for part in re.split(r"[;,|]", (r.get(field) or "").strip()):
            part = part.strip()
            if part and part not in ("NA", "na", "unknown") and part not in seen:
                seen.append(part)
    if len(seen) > limit:
        return ";".join(seen[:limit]) + f";(+{len(seen) - limit} more)"
    return ";".join(seen)


def fetch_telemetry_samples(offline):
    if offline:
        if not os.path.exists(CACHE):
            sys.exit(f"--offline given but {CACHE} does not exist")
        return json.load(open(CACHE))

    out, offset = [], 0
    while True:
        url = f"{TELEMETRY}/api/samples?limit=500&offset={offset}"
        page = json.load(urllib.request.urlopen(url, timeout=60))
        items = page.get("items", [])
        out += items
        if not items or len(out) >= page.get("total", 0):
            break
        offset += 500
    os.makedirs(OUTDIR, exist_ok=True)
    json.dump(out, open(CACHE, "w"))
    return out


def index_telemetry(samples):
    """run accession -> (bioproject, collections)"""
    run_map = {}
    for s in samples:
        meta = s.get("metadata") or {}
        bioproject = meta.get("bioproject") or ""
        collections = ";".join(sorted(
            c for c in (s.get("collections") or []) if isinstance(c, str)))
        for run in RUN_RE.findall(s.get("ncbi_accession") or ""):
            run_map[run] = (bioproject, collections)
    return run_map


def bioproject_for(study, study_dir, runs, run_map):
    """Prefer the checked-in SRA metadata; fall back to telemetry."""
    sra = os.path.join(study_dir, f"{study}_sra_meta.tsv")
    if os.path.exists(sra):
        found = sorted({(x.get("BioProject") or "").strip()
                        for x in read_table(sra)})
        found = [b for b in found if b.startswith("PRJ")]
        if found:
            return ";".join(found)
    found = sorted({run_map[r][0] for r in runs
                    if r in run_map and run_map[r][0]})
    return ";".join(found)


def build_row(study, study_dir, run_map):
    path = curated_file(study_dir)

    if study in SHARED:
        source = "CMD;PMD"
    elif study in PMD_SOURCE:
        source = "PMD"
    else:
        source = "CMD"

    blank = dict.fromkeys(FIELDS, "")
    blank.update(uuid=str(uuid.uuid5(NS, study)), study_name=study,
                 source_project=source)

    if path is None:
        blank.update(n_samples=0, n_runs=0, curation_available="FALSE",
                     ci_validated="FALSE", n_runs_processed=0,
                     processing_status="not_processed", runs_per_sample="",
                     notes="no curated sample table in repo")
        return blank, []

    records = read_table(path)
    runs = sorted({m for r in records
                   for m in RUN_RE.findall(r.get("ncbi_accession") or "")})
    processed = [r for r in runs if r in run_map]
    collections = sorted({run_map[r][1] for r in processed if run_map[r][1]})

    if not runs:
        status = "no_run_accessions"
    elif not processed:
        status = "not_processed"
    elif len(processed) == len(runs):
        status = "processed"
    else:
        status = "partial"

    # CI only validates *_sample.tsv; anything else is silently skipped.
    validated = os.path.basename(path).endswith("_sample.tsv")

    notes = []
    if not runs:
        notes.append("no run accessions in ncbi_accession")
    if not validated:
        notes.append("filename not *_sample.tsv - skipped by CI validator")

    blank.update(
        study_id=bioproject_for(study, study_dir, runs, run_map),
        n_samples=len(records), n_runs=len(runs),
        curation_available="TRUE",
        ci_validated="TRUE" if validated else "FALSE",
        primary_disease=distinct(records, "disease"),
        body_site=distinct(records, "body_site"),
        country=distinct(records, "country"),
        sequencing_platform=distinct(records, "sequencing_platform"),
        pmid=distinct(records, "pmid"),
        n_runs_processed=len(processed),
        telemetry_collections=";".join(collections),
        processing_status=status,
        runs_per_sample=(f"{len(runs) / len(records):.2f}"
                         if records and runs else ""),
        notes="; ".join(notes))

    if not blank["study_id"]:
        notes.append("BioProject unresolved")
        blank["notes"] = "; ".join(notes)

    return blank, [(study, r, run_map.get(r, ("", ""))[0],
                    "TRUE" if r in run_map else "FALSE") for r in runs]


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--offline", action="store_true",
                    help="rebuild from the cached telemetry snapshot")
    args = ap.parse_args()

    if not os.path.isdir(CURATED):
        sys.exit("run this from the repository root")

    samples = fetch_telemetry_samples(args.offline)
    run_map = index_telemetry(samples)
    print(f"telemetry: {len(samples)} samples, {len(run_map)} run accessions")

    rows, run_rows = [], []
    for study in sorted(os.listdir(CURATED)):
        study_dir = os.path.join(CURATED, study)
        if not os.path.isdir(study_dir):
            continue
        row, runs = build_row(study, study_dir, run_map)
        rows.append(row)
        run_rows += runs

    os.makedirs(OUTDIR, exist_ok=True)
    with open(os.path.join(OUTDIR, "studies_status.csv"), "w",
              newline="") as fh:
        w = csv.DictWriter(fh, fieldnames=FIELDS, lineterminator="\n")
        w.writeheader()
        w.writerows(rows)

    with open(os.path.join(OUTDIR, "studies_runs.tsv"), "w",
              newline="") as fh:
        w = csv.writer(fh, delimiter="\t", lineterminator="\n")
        w.writerow(["study_name", "run_accession", "bioproject",
                    "in_telemetry"])
        w.writerows(run_rows)

    def count(status):
        return sum(1 for r in rows if r["processing_status"] == status)

    print(f"studies:             {len(rows)}")
    print(f"samples:             {sum(int(r['n_samples']) for r in rows)}")
    print(f"run accessions:      {len(run_rows)}")
    print(f"BioProject resolved: {sum(1 for r in rows if r['study_id'])}")
    print(f"  processed:         {count('processed')}")
    print(f"  partial:           {count('partial')}")
    print(f"  not processed:     {count('not_processed')}")
    print(f"  no run accessions: {count('no_run_accessions')}")
    print(f"not CI-validated:    "
          f"{sum(1 for r in rows if r['ci_validated'] == 'FALSE')}")


if __name__ == "__main__":
    main()
