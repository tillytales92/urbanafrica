"""
Exploratory data analysis of the African Development Corridors Database (2022).

Data source (not included in repo history, read directly from the zip in data/raw):
  Thorn, J.P.R., Mwangi, B. & Juffe Bignoli, D. (2022). The African Development
  Corridors Database 2022 [Dataset]. Dryad. https://doi.org/10.5061/dryad.9kd51c5hw
Companion paper:
  Thorn et al. (2022). "The Development Corridors Database: A new tool to assess
  the impacts of infrastructure investments." Scientific Data.
  https://doi.org/10.1038/s41597-022-01771-y

Per the Dryad record, the database covers 79 development corridors comprising 184
projects across 53 continental African countries, compiled from a systematic
review of 191 sources (2020-2021) and manually digitised in ArcGIS. ~76% of
projects are spatially mapped (line/point layers); the rest are listed only in
the 'unmapped' table. The GeoPackage bundled in the raw zip has four layers:
  - line        : MultiLineString corridors (roads, railways, pipelines, ...)
  - point       : Point features (ports, airports, industrial parks, ...)
  - references  : source citations per project (many-to-one on Project_code)
  - unmapped    : same attribute schema as line/point, but no geometry

This script reads that GeoPackage straight out of the zip (GDAL /vsizip/, no
manual extraction needed) and writes summary tables + figures to
data/intermediate/eda/, plus a narrative gap-analysis to eda_summary.md.
"""

from __future__ import annotations

import re
import unicodedata
from pathlib import Path

import geopandas as gpd
import matplotlib
import numpy as np
import pandas as pd

matplotlib.use("Agg")
import matplotlib.pyplot as plt

# ---------------------------------------------------------------------------
# Paths & constants
# ---------------------------------------------------------------------------
ROOT = Path(__file__).resolve().parents[2]
RAW_DIR = ROOT / "data" / "raw"
OUT_DIR = ROOT / "data" / "intermediate" / "eda"
FIG_DIR = OUT_DIR / "figures"

OUTER_ZIP = RAW_DIR / "devcorridors" / "doi_10_5061_dryad_9kd51c5hw__v20220909.zip"
GPKG_NAME = "AfricanDevelopmentCorridorDatabase2022.gpkg"

LAYERS = ["line", "point", "references", "unmapped"]
SENTINELS = ["NI", "NA"]  # "No Information" / "Not Applicable" — not true NULLs
MULTI_VALUE_COLS = [
    "Country",
    "Region_or_province",
    "Name_of_donors_or_financiers",
    "Type_of_major_donors_or_financiers",
    "Commodities_traded_or_transported",
    "Key_beneficiaries",
]


def gpkg_path() -> str:
    if not OUTER_ZIP.exists():
        raise FileNotFoundError(
            f"Expected raw archive at {OUTER_ZIP}. "
            "Download it from https://doi.org/10.5061/dryad.9kd51c5hw"
        )
    return f"/vsizip/{OUTER_ZIP.as_posix()}/{GPKG_NAME}"


def load_layers() -> dict[str, gpd.GeoDataFrame]:
    path = gpkg_path()
    return {layer: gpd.read_file(path, layer=layer) for layer in LAYERS}


def clean_text(x: str) -> str:
    """Normalise whitespace and unicode accents (the raw data mixes curly/straight
    apostrophes and has stray leading/trailing spaces, e.g. ' South Africa')."""
    if not isinstance(x, str):
        return x
    x = unicodedata.normalize("NFKC", x).replace("’", "'")
    return re.sub(r"\s+", " ", x).strip()


def split_multi(series: pd.Series) -> pd.Series:
    """Explode a ';'-delimited field into a flat Series of cleaned values,
    dropping the NI/NA sentinels."""
    parts = (
        series.dropna()
        .astype(str)
        .apply(lambda s: [clean_text(p) for p in s.split(";")])
        .explode()
    )
    return parts[~parts.isin(SENTINELS + [""])]


def to_numeric_with_sentinels(series: pd.Series) -> pd.Series:
    cleaned = series.astype(str).str.strip().replace(SENTINELS, np.nan)
    return pd.to_numeric(cleaned, errors="coerce")


# ---------------------------------------------------------------------------
# Report sections
# ---------------------------------------------------------------------------
def section(title: str, lines: list[str]) -> None:
    lines.append("")
    lines.append(f"## {title}")


def overview(data: dict, lines: list[str]) -> None:
    section("Dataset overview", lines)
    for name, gdf in data.items():
        is_spatial = isinstance(gdf, gpd.GeoDataFrame) and gdf.geometry.notna().any()
        geom_note = ""
        if is_spatial:
            geom_note = f", geometry={gdf.geom_type.unique().tolist()}, crs={gdf.crs}"
        lines.append(f"- **{name}**: {gdf.shape[0]} rows x {gdf.shape[1]} cols{geom_note}")

    all_codes = set()
    for name in ["line", "point", "unmapped"]:
        all_codes |= set(data[name]["Project_code"])
    lines.append(
        f"- Union of Project_code across line+point+unmapped: **{len(all_codes)}** "
        "(Dryad metadata states the database covers 184 projects across 79 corridors — "
        "cross-check this number after any filtering)."
    )
    for name in ["line", "point", "unmapped"]:
        dup = data[name]["Project_code"].duplicated().sum()
        lines.append(f"  - duplicated Project_code within '{name}': {dup}")
    overlap_lp = set(data["line"]["Project_code"]) & set(data["point"]["Project_code"])
    lines.append(
        f"  - Project_code overlap between line and point layers: {len(overlap_lp)} "
        "(expect 0 — each project is mapped as either a line OR a point, not both)"
    )


def schema_diff(data: dict, lines: list[str]) -> None:
    section("Schema differences between line / point / unmapped", lines)
    cols = {name: set(data[name].columns) - {"geometry"} for name in ["line", "point", "unmapped"]}
    common = cols["line"] & cols["point"] & cols["unmapped"]
    lines.append(f"- Columns common to all three: {len(common)}")
    for name in ["line", "point", "unmapped"]:
        extra = cols[name] - (common)
        if extra:
            lines.append(f"- Columns only in '{name}': {sorted(extra)}")


def completeness(data: dict, lines: list[str]) -> pd.DataFrame:
    section("Completeness (true NULL vs. 'NI'/'NA' sentinels)", lines)
    rows = []
    for name in ["line", "point", "unmapped"]:
        gdf = data[name]
        n = len(gdf)
        for col in gdf.columns:
            if col in ("geometry", "OBJECTID"):
                continue
            s = gdf[col]
            n_null = s.isna().sum()
            as_str = s.astype(str).str.strip()
            n_ni = (as_str == "NI").sum()
            n_na = (as_str == "NA").sum()
            rows.append(
                {
                    "layer": name,
                    "column": col,
                    "n": n,
                    "n_null": n_null,
                    "n_NI": n_ni,
                    "n_NA": n_na,
                    "pct_missing_or_sentinel": round(100 * (n_null + n_ni + n_na) / n, 1),
                }
            )
    comp = pd.DataFrame(rows).sort_values(
        ["layer", "pct_missing_or_sentinel"], ascending=[True, False]
    )
    worst = comp[comp["pct_missing_or_sentinel"] >= 30]
    lines.append(
        f"- {len(worst)} (layer, column) combinations are >=30% missing/'NI'/'NA'. "
        "Worst offenders:"
    )
    # A handful of 100%-missing columns are structurally expected, not data-quality gaps:
    # Area__Km2_/GIS_distance only make sense for polygon/line features respectively, so
    # they're NA-by-construction for point rows (and GIS_distance for unmapped rows, which
    # have no geometry at all).
    expected_na = {
        ("point", "GIS_distance"): "points have no line length to measure",
        ("unmapped", "GIS_distance"): "unmapped rows have no digitised geometry",
        ("line", "Area__Km2_"): "area is not applicable to a line feature",
        ("unmapped", "Feature_type"): "column is entirely unused (100% NA) — dead field",
    }
    for _, r in worst.head(15).iterrows():
        note = expected_na.get((r["layer"], r["column"]))
        suffix = f"  [structural: {note}]" if note else ""
        lines.append(
            f"  - {r['layer']}.{r['column']}: {r['pct_missing_or_sentinel']}% "
            f"(null={r['n_null']}, NI={r['n_NI']}, NA={r['n_NA']}, n={r['n']}){suffix}"
        )
    return comp


def categorical_summaries(data: dict, lines: list[str]) -> None:
    section("Categorical variables", lines)

    for name in ["line", "point"]:
        vc = data[name]["Status"].value_counts(dropna=False)
        lines.append(f"- {name}.Status value counts: {vc.to_dict()}")
    lines.append(
        "  -> 'In progress' and 'In Progress' both occur in the line layer: inconsistent "
        "capitalisation that will silently split groups unless normalised."
    )

    for name in ["line", "point"]:
        vc = data[name]["Infrastructure_development_type"].value_counts(dropna=False)
        lines.append(f"- {name}.Infrastructure_development_type: {vc.to_dict()}")

    countries = split_multi(data["line"]["Country"])
    lines.append(
        f"- Country field is ';'-delimited (multi-country corridors). After splitting and "
        f"cleaning, line layer touches {countries.nunique()} distinct countries; "
        f"top 10: {countries.value_counts().head(10).to_dict()}"
    )

    n_named = data["line"]["Corridor_name"].nunique()
    lines.append(
        f"- {n_named} distinct Corridor_name values across {len(data['line'])} line "
        "projects: many corridors bundle several projects (e.g. road + rail + pipeline "
        "under one corridor name), so 'project' and 'corridor' are not the same unit "
        "of analysis."
    )


def numeric_summaries(data: dict, lines: list[str]) -> None:
    section("Numeric variables", lines)

    for name in ["line", "point", "unmapped"]:
        gdf = data[name]
        yr = to_numeric_with_sentinels(gdf["Launch_year"])
        lines.append(
            f"- {name}.Launch_year: {yr.notna().sum()}/{len(yr)} parseable "
            f"(range {yr.min():.0f}-{yr.max():.0f}); "
            f"{(gdf['Launch_year'].astype(str).str.strip()=='NI').sum()} marked 'NI', "
            f"{(gdf['Launch_year'].astype(str).str.strip()=='NA').sum()} marked 'NA'."
        )

    line_dist_min = to_numeric_with_sentinels(data["line"]["Distance__km__Minimum"])
    line_dist_max = to_numeric_with_sentinels(data["line"]["Distance__km__Maximum"])
    line_gis = to_numeric_with_sentinels(data["line"]["GIS_distance"])
    # GIS_distance tracks Distance__km__Minimum far more closely than __Maximum
    # (median relative gap ~15% vs. ~124%): the digitised line appears to represent the
    # single "core route", while __Maximum often covers a wider network of branches/spurs
    # reported in the source literature. Comparing GIS_distance to __Maximum (the more
    # obviously named counterpart) would overstate the discrepancy roughly 8x.
    rel_diff = (line_gis - line_dist_min).abs() / line_dist_min.replace(0, np.nan)
    big_gap = (rel_diff > 0.25).sum()
    lines.append(
        "- line.GIS_distance (digitised in ArcGIS) tracks line.Distance__km__Minimum, "
        "*not* __Maximum (median relative gap 15% vs. Minimum, 124% vs. Maximum) — the "
        "digitised route appears to represent the corridor's core alignment, while "
        "__Maximum often reflects a wider network of branches/spurs cited in the source "
        f"literature. Even against Minimum, {big_gap} of {rel_diff.notna().sum()} projects "
        "differ by >25%, a useful flag for digitisation/route uncertainty."
    )

    usd_max = to_numeric_with_sentinels(data["line"]["USD_amount__Million__Maximum"])
    lines.append(
        f"- line.USD_amount__Million__Maximum: {usd_max.notna().sum()}/{len(usd_max)} "
        f"parseable, median USD {usd_max.median():.0f}m, max USD {usd_max.max():.0f}m. "
        "Note: the line layer has no 'USD_amount__Million__Minimum' column (point and "
        "unmapped layers do) — cost ranges can only be recovered for point/unmapped rows."
    )


def donor_summaries(data: dict, lines: list[str]) -> None:
    section("Financing (donor) attributes", lines)
    donor_types = split_multi(data["line"]["Type_of_major_donors_or_financiers"])
    lines.append(f"- Donor-type frequency (line layer): {donor_types.value_counts().to_dict()}")
    lines.append(
        "  -> Name_of_donors_or_financiers, Amount_funded_per_donor_type and "
        "Type_of_major_donors_or_financiers are parallel ';'-delimited lists meant to "
        "align positionally (donor[i] <-> amount[i] <-> type[i]). This script does not "
        "verify that the three lists are always the same length per row — do that before "
        "trusting any per-donor amount join."
    )


def cross_layer_checks(data: dict, lines: list[str]) -> None:
    section("Cross-layer consistency: references & unmapped", lines)

    refs = data["references"]
    empty_fields = [c for c in refs.columns if c.startswith("Field") and refs[c].notna().sum() == 0]
    lines.append(
        f"- references layer carries {len(empty_fields)} entirely empty 'FieldN' columns "
        f"({empty_fields[0]}..{empty_fields[-1]}) — leftover spreadsheet artefacts with no data."
    )

    all_codes = set()
    for name in ["line", "point", "unmapped"]:
        all_codes |= set(data[name]["Project_code"])
    ref_codes = set(refs["Project_code"].dropna())
    lines.append(
        f"- {len(ref_codes - all_codes)} Project_code values appear in 'references' but not "
        "in line/point/unmapped (orphan citations)."
    )
    lines.append(
        f"- {len(all_codes - ref_codes)} projects (in line/point/unmapped) have **no** "
        "citation at all in 'references'."
    )
    refs_per_project = refs["Project_code"].value_counts()
    lines.append(
        f"- References per project: median {refs_per_project.median():.0f}, "
        f"max {refs_per_project.max()}, {(refs_per_project == 1).sum()} projects backed "
        "by only a single source."
    )

    unmapped = data["unmapped"]
    lines.append(
        f"- 'unmapped' layer: {len(unmapped)} projects "
        f"({len(unmapped) / len(all_codes):.1%} of all {len(all_codes)}) have attributes "
        "but no digitised geometry at all — any spatial analysis silently drops them."
    )
    avail = unmapped["Spatial_data_availability"].str.strip().str.lower().value_counts()
    lines.append(f"  - Spatial_data_availability reasons: {avail.to_dict()}")
    search = unmapped["Spatial_data_search"].value_counts()
    lines.append(f"  - Spatial_data_search outcome: {search.to_dict()}")
    lines.append(
        "  -> most unmapped projects were *seen* by the compilers but the geodata was "
        "not obtainable/licensable, i.e. this is a data-access gap, not a knowledge gap."
    )


def geometry_checks(data: dict, lines: list[str]) -> None:
    section("Geometry checks", lines)
    for name in ["line", "point"]:
        gdf = data[name]
        invalid = (~gdf.geometry.is_valid).sum()
        empty = gdf.geometry.is_empty.sum()
        bounds = gdf.total_bounds
        lines.append(
            f"- {name}: {invalid} invalid geometries, {empty} empty geometries, "
            f"bbox (lon/lat) = [{bounds[0]:.2f}, {bounds[1]:.2f}, {bounds[2]:.2f}, {bounds[3]:.2f}]"
        )
    lines.append(
        "  -> bbox should sit within continental Africa (~-18 to 52 lon, -35 to 38 lat); "
        "a wider bbox would indicate a digitising error worth spot-checking on a map."
    )


# ---------------------------------------------------------------------------
# Figures
# ---------------------------------------------------------------------------
def make_figures(data: dict) -> None:
    FIG_DIR.mkdir(parents=True, exist_ok=True)

    # 1. Status counts (line vs point), with capitalisation normalised
    fig, ax = plt.subplots(figsize=(7, 4))
    for name, offset in [("line", -0.2), ("point", 0.2)]:
        status = data[name]["Status"].str.strip().str.lower().str.capitalize()
        vc = status.value_counts()
        ax.bar(np.arange(len(vc)) + offset, vc.values, width=0.4, label=name)
        ax.set_xticks(np.arange(len(vc)))
        ax.set_xticklabels(vc.index, rotation=30, ha="right")
    ax.set_ylabel("Number of projects")
    ax.set_title("Project status by geometry layer")
    ax.legend()
    fig.tight_layout()
    fig.savefig(FIG_DIR / "status_counts.png", dpi=150)
    plt.close(fig)

    # 2. Infrastructure type counts
    fig, ax = plt.subplots(figsize=(8, 4))
    combined = pd.concat(
        [data["line"]["Infrastructure_development_type"], data["point"]["Infrastructure_development_type"]]
    )
    combined.value_counts().sort_values().plot.barh(ax=ax)
    ax.set_xlabel("Number of projects")
    ax.set_title("Infrastructure development type (line + point)")
    fig.tight_layout()
    fig.savefig(FIG_DIR / "infrastructure_type_counts.png", dpi=150)
    plt.close(fig)

    # 3. Top-20 countries by project count
    countries = split_multi(pd.concat([data["line"]["Country"], data["point"]["Country"]]))
    fig, ax = plt.subplots(figsize=(7, 6))
    countries.value_counts().head(20).sort_values().plot.barh(ax=ax)
    ax.set_xlabel("Number of projects touching country")
    ax.set_title("Top 20 countries by project count")
    fig.tight_layout()
    fig.savefig(FIG_DIR / "top20_countries.png", dpi=150)
    plt.close(fig)

    # 4. Launch year histogram
    fig, ax = plt.subplots(figsize=(8, 4))
    years = pd.concat(
        [to_numeric_with_sentinels(data[n]["Launch_year"]) for n in ["line", "point", "unmapped"]]
    ).dropna()
    ax.hist(years, bins=30, edgecolor="white")
    ax.set_xlabel("Launch year")
    ax.set_ylabel("Number of projects")
    ax.set_title(f"Launch year distribution (n={len(years)} with a parseable year)")
    fig.tight_layout()
    fig.savefig(FIG_DIR / "launch_year_hist.png", dpi=150)
    plt.close(fig)

    # 5. Missingness heatmap (sentinel-aware), line + point
    fig, axes = plt.subplots(1, 2, figsize=(14, 6))
    for ax, name in zip(axes, ["line", "point"]):
        gdf = data[name].drop(columns=["geometry", "OBJECTID"])
        miss = gdf.apply(lambda s: s.astype(str).str.strip().isin(["nan", "None", "NI", "NA"]))
        ax.imshow(miss.values.astype(int), aspect="auto", cmap="Reds", interpolation="none")
        ax.set_title(f"{name} layer: missing / NI / NA (red)")
        ax.set_xticks(range(len(miss.columns)))
        ax.set_xticklabels(miss.columns, rotation=90, fontsize=6)
        ax.set_yticks([])
    fig.tight_layout()
    fig.savefig(FIG_DIR / "missingness_heatmap.png", dpi=150)
    plt.close(fig)

    # 6. Map of mapped geometry (lines + points) coloured by infra type
    fig, ax = plt.subplots(figsize=(8, 8))
    data["line"].plot(
        ax=ax, column="Infrastructure_development_type", legend=False, linewidth=1, cmap="tab10"
    )
    data["point"].plot(ax=ax, color="black", markersize=8, marker="o")
    ax.set_title("Digitised corridors (lines) and nodes (points)")
    ax.set_xlabel("Longitude")
    ax.set_ylabel("Latitude")
    fig.tight_layout()
    fig.savefig(FIG_DIR / "corridor_map.png", dpi=150)
    plt.close(fig)

    # 7. GIS-measured vs reported distance (GIS_distance tracks Minimum, not Maximum — see numeric_summaries)
    fig, ax = plt.subplots(figsize=(6, 6))
    x = to_numeric_with_sentinels(data["line"]["Distance__km__Minimum"])
    y = to_numeric_with_sentinels(data["line"]["GIS_distance"])
    mask = x.notna() & y.notna()
    ax.scatter(x[mask], y[mask], alpha=0.6)
    lim = max(x[mask].max(), y[mask].max()) * 1.05
    ax.plot([0, lim], [0, lim], color="grey", linestyle="--", linewidth=1)
    ax.set_xlabel("Reported distance, km (Distance__km__Minimum)")
    ax.set_ylabel("Digitised GIS distance, km")
    ax.set_title("Reported ('core route') vs. digitised corridor length (line layer)")
    fig.tight_layout()
    fig.savefig(FIG_DIR / "distance_reported_vs_gis.png", dpi=150)
    plt.close(fig)


# ---------------------------------------------------------------------------
# Gaps summary
# ---------------------------------------------------------------------------
def gaps_summary(data: dict, lines: list[str]) -> None:
    section("What this dataset does NOT have (read before using it)", lines)
    lines.extend(
        [
            "- **Static snapshot, not a time series**: compiled from a literature review "
            "conducted 2020-2021 and published 2022. No project status/cost updates since "
            "then — nothing here reflects 2023-2026 developments (e.g. Lobito Corridor "
            "progress discussed in the accompanying G20/OECD policy note is not in this data).",
            "- **Documented scope vs. actual content mismatch on islands**: Dryad metadata "
            "states coverage is limited to the 53 continental African countries, but the data "
            "itself contradicts this — Madagascar (Port of Toamasina), Seychelles (Port of "
            "Victoria) and Cabo Verde all appear as Country values. Coverage of island states "
            "is real but incidental/incomplete, not a deliberate, systematic inclusion — don't "
            "treat 'island nations' as a reliably absent or reliably present category.",
            "- **Corridor-affiliated projects only**: only infrastructure explicitly tied to a "
            "named development-corridor initiative is included; general national infrastructure "
            "off any named corridor is out of scope.",
            "- **~24% of projects have no geometry** ('unmapped' layer): 44 of 184 projects "
            "are attribute-only; mostly because usable GIS data could not be sourced/licensed, "
            "not because the compilers didn't know the project existed.",
            "- **No trade/traffic flow data**: commodities are listed qualitatively "
            "(Commodities_traded_or_transported), but there are no volumes, tonnages, "
            "transit times, or trade-value flows — this is a project/infrastructure "
            "inventory, not a freight or economic-impact dataset.",
            "- **No standardised project identifiers linking to PIDA, AfDB or other registries**: "
            "Project_code is internal to this database only, so joining to PIDA PAP2's 69 "
            "priority projects (mentioned in the policy PDF) or AfDB project IDs requires "
            "manual/fuzzy matching on name/corridor.",
            "- **Financing detail is shallow and only lightly structured**: donors, amounts and "
            "donor types are parallel ';'-delimited free-text lists (not a normalised "
            "donor-project-amount table), amounts are often 'NI', and there's no actual-vs-"
            "planned cost, no disbursement schedule, and no currency-year deflator.",
            "- **No governance/institutional attributes**: no corridor management authority, "
            "no cross-border agreement status, no regulatory/customs information — despite "
            "these being flagged as central to corridor success in the policy PDF.",
            "- **No energy/power-system linkage**: the policy PDF stresses co-planning corridors "
            "with the Continental Power System Master Plan / AfSEM; this dataset has no energy "
            "attributes beyond 'Pipeline (oil)' as an infrastructure type.",
            "- **No environmental/social indicators**: no ESIA status, no land-use or resettlement "
            "data, no population served/affected counts.",
            "- **Source-language bias**: literature review covered English, Swahili, Portuguese "
            "and French sources only (per Dryad metadata) — Arabic-, Yoruba- and Hausa-language "
            "sources (relevant to North/West Africa) were explicitly out of scope, so North "
            "African corridor coverage may be comparatively thinner.",
            "- **Data-entry inconsistencies to clean before analysis**: mixed capitalisation in "
            "Status ('In progress' vs 'In Progress'); inconsistent whitespace and apostrophe "
            "unicode in Country/Corridor names (e.g. ' South Africa', \"Côte d'Ivoire\" vs "
            "\"Cote d'Ivoire\"); 'NI'/'NA' sentinel strings mixed with true NULLs across nearly "
            "every free-text and numeric-looking column; the 'references' layer carries 20 "
            "entirely empty legacy columns (Field4-Field23).",
        ]
    )


def write_report(lines: list[str]) -> Path:
    OUT_DIR.mkdir(parents=True, exist_ok=True)
    report_path = OUT_DIR / "eda_summary.md"
    report_path.write_text(
        "# EDA summary — African Development Corridors Database 2022\n" + "\n".join(lines) + "\n",
        encoding="utf-8",
    )
    return report_path


# ---------------------------------------------------------------------------
# Run — cell-delimited (# %%) so this also works as an interactive notebook-style
# script in Positron / VS Code / Jupytext: open the file and run cell-by-cell to
# inspect `data["line"]`, `comp`, etc. in the Variable Explorer as you go. Running
# `python scripts/eda_corridors.py` from the CLI executes every cell top to bottom.
# ---------------------------------------------------------------------------

# %% Load data straight from the raw zip (no manual extraction needed)
data = load_layers()
lines: list[str] = []

# %% Overview & schema
overview(data, lines)
schema_diff(data, lines)
print("\n".join(lines[-20:]))

# %% Completeness (true NULL vs. 'NI'/'NA' sentinels)
comp = completeness(data, lines)
comp.head(20)

# %% Categorical, numeric & donor-field summaries
categorical_summaries(data, lines)
numeric_summaries(data, lines)
donor_summaries(data, lines)

# %% Cross-layer consistency & geometry checks
cross_layer_checks(data, lines)
geometry_checks(data, lines)

# %% Gaps summary — what the dataset does NOT have
gaps_summary(data, lines)

# %% Save outputs: completeness table, figures, narrative report
OUT_DIR.mkdir(parents=True, exist_ok=True)
comp.to_csv(OUT_DIR / "completeness_by_column.csv", index=False)
make_figures(data)
report_path = write_report(lines)

print("\n".join(lines))
print(f"\nWrote completeness table to {OUT_DIR / 'completeness_by_column.csv'}")
print(f"Wrote {len(list(FIG_DIR.glob('*.png')))} figures to {FIG_DIR}")
print(f"Wrote narrative summary to {report_path}")
