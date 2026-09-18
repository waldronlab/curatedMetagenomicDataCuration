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
#         One row per study on *either* side of the comparison:
#           - every curated study in inst/curated/, and
#           - every study the pipeline has processed that has no curated
#             metadata here (curation_available = FALSE,
#             processing_status = processed_not_curated).
#         Carries identity (uuid, study_name, study_id), size, provenance,
#         curation state, pipeline state, and placeholders for profile
#         versions/locations that only Sean and Francesco can fill.
#
#         Covering both sides is the point: the 2026-09-18 meeting asked to
#         "make a Venn diagram possible", which needs the processed-only set
#         as well as the curated one.
#
#     inst/extdata/studies_runs.tsv
#         Long-form study_name -> run_accession lookup, with the BioProject
#         each run resolves to and which source answered. Run accessions live
#         here and NOT in study_id, so the manifest stays small and study_id
#         holds an actual study identifier (BioProject).
#
# Telemetry source: https://nf-telemetry.cancerdatasci.org (public, no auth).
# Pass --offline to rebuild from a previously saved samples.json instead.
#
# Output is deterministic: sorted throughout, LF line endings, and study UUIDs
# are uuid5 over a fixed namespace. Re-running with unchanged inputs rewrites
# the files byte-for-byte, so a scheduled job commits only on real change.
#
# Usage:
#     python3 inst/scripts/build_manifest.py            # from the repo root
#
# Normally you do not need to: .github/workflows/update-manifest.yml runs this
# on pushes touching inst/curated/** and weekly for telemetry drift.
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
import collections

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
    "n_samples_processed", "n_runs_processed", "telemetry_collections",
    "processing_status", "runs_per_sample", "notes",
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
    """run accession -> (bioproject, collections)

    Telemetry registers collections by BioProject *or* by SRA study accession
    (its `/api/cohorts` reports both as `source`), so the `bioproject`
    metadata key is not guaranteed to hold a BioProject. Anything that is not
    a `PRJ*` accession is dropped here rather than downstream, so `study_id`
    can only ever contain real BioProjects.
    """
    run_map = {}
    for s in samples:
        meta = s.get("metadata") or {}
        bioproject = (meta.get("bioproject") or "").strip()
        if not bioproject.startswith("PRJ"):
            bioproject = ""
        collections = ";".join(sorted(
            c for c in (s.get("collections") or []) if isinstance(c, str)))
        for run in RUN_RE.findall(s.get("ncbi_accession") or ""):
            run_map[run] = (bioproject, collections)
    return run_map


def sra_meta_run_map(study, study_dir):
    """run accession -> BioProject, from the study's checked-in SRA metadata.

    Per-run rather than per-study, so a study spanning several BioProjects
    attributes each run correctly.
    """
    sra = os.path.join(study_dir, f"{study}_sra_meta.tsv")
    if not os.path.exists(sra):
        return {}
    out = {}
    for row in read_table(sra):
        run = (row.get("Run") or "").strip()
        bioproject = (row.get("BioProject") or "").strip()
        if run and bioproject.startswith("PRJ"):
            out[run] = bioproject
    return out


def bioproject_for(runs, run_map, sra_runs):
    """Study-level BioProject: SRA metadata first, then telemetry.

    Resolution is per-run so a study spanning several BioProjects reports all
    of them. The last fallback is every BioProject named anywhere in the
    study's SRA metadata, which is what keeps studies carrying no run
    accessions at all (`TettAJ_2016`, `YachidaS_2019`) from losing an
    identifier they demonstrably have.
    """
    found = sorted({sra_runs[r] for r in runs if r in sra_runs})
    if found:
        return ";".join(found)
    found = sorted({run_map[r][0] for r in runs
                    if r in run_map and run_map[r][0]})
    if found:
        return ";".join(found)
    return ";".join(sorted(set(sra_runs.values())))


def run_bioproject(run, run_map, sra_runs):
    """(bioproject, source) for one run.

    The checked-in SRA metadata covers runs telemetry has never seen, so it is
    tried first; `source` records which one answered, so a consumer can tell a
    curated mapping from a pipeline-observed one.
    """
    if run in sra_runs:
        return sra_runs[run], "sra_meta"
    if run in run_map and run_map[run][0]:
        return run_map[run][0], "telemetry"
    return "", ""


def attribute_samples(samples, curated_runs, curated_bioprojects,
                      curated_names):
    """Split telemetry samples into (per-study attribution, orphans).

    Three ways a processed sample is recognised as belonging to a curated
    study, tried in descending order of confidence:

      1. one of its run accessions is listed in that study's curated table;
      2. its BioProject is one the study resolves to;
      3. it was registered under a telemetry collection named after the study
         (the CMD-sourced collections use study names as labels).

    Arms 2 and 3 matter because the pipeline processes samples that curation
    has not listed run-by-run. Those are a gap *inside* a known study, which
    is a different problem from an unknown study, so they are counted
    separately as `unlisted` rather than treated as orphans.

    Whatever matches none of the three is genuinely processed-but-not-curated.
    """
    by_study = collections.defaultdict(
        lambda: {"samples": 0, "unlisted": 0})
    orphans = []

    for s in samples:
        runs = set(RUN_RE.findall(s.get("ncbi_accession") or ""))
        study = next((curated_runs[r] for r in sorted(runs)
                      if r in curated_runs), None)
        if study is not None:
            by_study[study]["samples"] += 1
            continue

        bioproject = ((s.get("metadata") or {}).get("bioproject") or "").strip()
        study = curated_bioprojects.get(bioproject)
        if study is None:
            study = next((c for c in sorted(s.get("collections") or [])
                          if c in curated_names), None)
        if study is not None:
            by_study[study]["samples"] += 1
            by_study[study]["unlisted"] += 1
            continue

        orphans.append(s)

    return by_study, orphans


def orphan_rows(orphans):
    """One row per processed-but-not-curated study.

    Grouped by BioProject where telemetry knows one, else by the collection
    it was registered under, because that is the only identifier such a study
    has until somebody curates it.
    """
    groups = collections.defaultdict(
        lambda: {"samples": 0, "runs": set(), "collections": set(),
                 "sra_studies": set()})

    for s in orphans:
        meta = s.get("metadata") or {}
        bioproject = (meta.get("bioproject") or "").strip()
        cols = sorted(c for c in (s.get("collections") or [])
                      if isinstance(c, str))
        key = bioproject if bioproject.startswith("PRJ") else (
            cols[0] if cols else "unidentified")
        g = groups[key]
        g["samples"] += 1
        g["runs"] |= set(RUN_RE.findall(s.get("ncbi_accession") or ""))
        g["collections"] |= set(cols)
        if meta.get("sra_study"):
            g["sra_studies"].add(meta["sra_study"])

    rows = []
    for key, g in sorted(groups.items()):
        row = dict.fromkeys(FIELDS, "")
        note = ["processed through the Nextflow pipeline; no curated metadata "
                "in this repository"]
        if g["sra_studies"]:
            note.append("SRA study " + ";".join(sorted(g["sra_studies"])))
        row.update(
            uuid=str(uuid.uuid5(NS, key)),
            study_name=key,
            study_id=key if key.startswith("PRJ") else "",
            n_samples=0,
            n_runs=0,
            source_project="",
            curation_available="FALSE",
            ci_validated="FALSE",
            n_samples_processed=g["samples"],
            n_runs_processed=len(g["runs"]),
            telemetry_collections=";".join(sorted(g["collections"])),
            processing_status="processed_not_curated",
            runs_per_sample="",
            notes="; ".join(note))
        rows.append(row)
    return rows


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
    sra_runs = sra_meta_run_map(study, study_dir)
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
        study_id=bioproject_for(runs, run_map, sra_runs),
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

    return blank, [(study, r) + run_bioproject(r, run_map, sra_runs) +
                   ("TRUE" if r in run_map else "FALSE",) for r in runs]


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

    # Second pass: work out which processed samples belong to which curated
    # study, so the ones belonging to none can be listed too. Without this the
    # manifest only ever shows the curated side of the comparison.
    curated_runs = {r[1]: r[0] for r in run_rows}
    curated_bioprojects = {}
    for row in rows:
        for bp in row["study_id"].split(";"):
            if bp:
                curated_bioprojects.setdefault(bp, row["study_name"])

    curated_names = {r["study_name"] for r in rows}
    attributed, orphans = attribute_samples(
        samples, curated_runs, curated_bioprojects, curated_names)

    for row in rows:
        hit = attributed.get(row["study_name"])
        row["n_samples_processed"] = hit["samples"] if hit else 0
        if hit and hit["unlisted"]:
            note = (f"{hit['unlisted']} processed sample(s) not listed in the "
                    "curated table")
            row["notes"] = f"{row['notes']}; {note}" if row["notes"] else note

    rows += orphan_rows(orphans)

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
                    "bioproject_source", "in_telemetry"])
        w.writerows(run_rows)

    def count(status):
        return sum(1 for r in rows if r["processing_status"] == status)

    curated = [r for r in rows if r["curation_available"] == "TRUE"]
    unlisted = sum(v["unlisted"] for v in attributed.values())

    print(f"rows:                  {len(rows)}")
    print(f"  curated studies:     {len(curated)}")
    print(f"  processed, uncurated:{count('processed_not_curated')}")
    print(f"curated samples:       {sum(int(r['n_samples']) for r in curated)}")
    print(f"run accessions:        {len(run_rows)}")
    print(f"BioProject resolved:   {sum(1 for r in rows if r['study_id'])}")
    print(f"  processed:           {count('processed')}")
    print(f"  partial:             {count('partial')}")
    print(f"  not processed:       {count('not_processed')}")
    print(f"  no run accessions:   {count('no_run_accessions')}")
    print(f"not CI-validated:      "
          f"{sum(1 for r in curated if r['ci_validated'] == 'FALSE')}")
    print(f"processed samples attributed to a curated study: "
          f"{sum(v['samples'] for v in attributed.values())}")
    print(f"  ...of those, not listed in its curated table: {unlisted}")
    print(f"processed samples with no curated study:         "
          f"{sum(int(r['n_samples_processed']) for r in rows if r['curation_available'] == 'FALSE')}")


if __name__ == "__main__":
    main()
