# Urban growth in Africa — story draft

**Status:** working draft. Numbers pulled from `app/data/` (`city_index.Rds`,
`sprawl_stats.Rds`, `africapolis_ntl.Rds`, `agglom_attrs.Rds`) on 2026-09-02.
Companion to the interactive dashboard; meant to be extended.

Scope: the **100 largest African agglomerations** by Africapolis 2020 population,
built-up surface 2000–2025 (GHSL BUILT-S R2023) plus harmonised nighttime lights
2000–2024.

---

## Read this first — data caveats

Anything built on this doc inherits these. Flag them wherever the relevant number appears.

1. **5 North African cities have broken absolute built-up totals.** Algiers,
   Constantine, Oran, Tangier and Tunis read `NaN` for `total_km2_2000/2025` in
   `city_index` (the zonal `terra::extract(fun = sum)` step failed for these
   polygons — the per-city rasters themselves are fine, so `sprawl_stats` still
   has valid numbers for them). Effect: "Northern Africa" absolute/growth stats
   are really 15 cities, and these 5 show blank in the dashboard rankings table.
   **To fix before publishing.**

2. **Small-town % / absolute outliers are polygon artifacts.** Kisumu (KEN)
   tops the absolute-growth table at **+946 km² / +409%** — ahead of Cairo. Not
   real: the Africapolis "Kisumu" polygon bundles a large multi-town Lake
   Victoria cluster. Same for Kikima (+656%), Sodo (+406%), Hawassa (+264%),
   Nakuru (+225%), Embu (+136%). Usable as a "measurement is hard" sidebar, not
   in a growth ranking.

3. **Absolute NTL-intensity change is a sensor artifact.** 2000 = DMSP-OLS,
   2024 = VIIRS. Growth ratios like +47,000 % or `Inf` (Lilongwe) are
   meaningless. Lit *share* is more robust but still has the DMSP→VIIRS
   discontinuity — treat unlit-share changes as **directional, not precise**.

4. **Africapolis boundaries are fixed (2015 vintage).** Cities that sprawled
   past their 2015 edge (Kinshasa, likely Luanda) are undercounted — their low
   measured growth is partly an artifact of a static boundary.

---

## The anchor

Across the 100 cities, built-up land grew from **~8,300 km² to ~13,300 km² —
+60 % in 25 years** (excludes the 5 broken cities). Median city: **+50 %**.

---

## Stories

Ranked by how solid the underlying data is and how compelling the framing.

### 1. It's the secondary cities, not the megacities

The biggest *absolute* built-up gains among genuinely large cities:

| City | 2000 → 2025 (km²) | Δ km² | Δ % |
|---|---|---|---|
| Cairo (Al-Qāhira) | 571 → 897 | +326 | +57 % |
| Lagos | 321 → 489 | +168 | +52 % |
| Tripoli | 265 → 388 | +124 | +47 % |
| Accra | 289 → 396 | +107 | +37 % |
| Nairobi | 178 → 262 | +83 | +47 % |
| Ibadan | 123 → 204 | +81 | +66 % |

But the fastest *relative* growth is all mid-tier: **Abuja +155 %**, Ikorodu
+228 %, **Kigali +122 %** (59 → 131 km²), **Benin City +121 %**, Osogbo +126 %.
Capital-relocation cities (Abuja) and secondary cities are where the multiplier
is — the megacities are adding the most land but off a base so large the
percentage looks modest.

- **Framing:** "Africa's urban story isn't only Lagos and Cairo."
- **Chart:** scatter, x = built-up 2000 (sqrt), y = Δ km² 2000–2025 (sqrt),
  size = population, colour = sub-region. Already exists in the dashboard's
  Scatterplots tab.
- **To verify:** strip the polygon-artifact cities before ranking.

### 2. Two urban Africas — one builds up, one spreads out

| Region | n | median growth | mean sprawl share | mean density change | mean unlit % 2025 |
|---|---|---|---|---|---|
| Sub-Saharan Africa | 80 | +52 % | 0.57 | −0.007 | 23 % |
| Northern Africa | 20 (15 valid) | +38 % | 0.34 | +0.020 | 2 % |

North African cities **densified** — more built-up surface packed into a
footprint that grew slowly. Sub-Saharan cities **sprawled** — footprint
outran surface, density flat to falling. The densification leaders are the
Maghreb plus Sahelian Nigeria (Constantine +0.060, Algiers, Oran, Agadir,
Ouagadougou, Kano, Kaduna). The sprawl leaders are East African / Ethiopian
(Sodo 0.92, Nakuru 0.92, Bujumbura 0.71, Nairobi 0.71).

- **Framing:** "Same continent, opposite growth model."
- **Chart:** sprawl share vs density change, coloured by region; or a
  region-grouped bar.
- **To verify:** re-run once the 5 NaN cities are fixed — they're all North
  African and would firm up that row.

### 3. Half of all new building is greenfield

Mean **sprawl share 0.53** — just over half of net new built-up surface landed
on previously-empty land rather than inside the existing city. **47 of 100
cities got *less* dense** over 25 years: same city logic, more land per
resident, higher cost per capita to service.

- **Framing:** the affordability angle — sprawl is expensive to connect to
  water, power, transport.
- **Chart:** histogram of sprawl share across the 100 cities; or the
  intensive-vs-extensive decomposition stacked bar for the top 20 by growth.

### 4. The compact exceptions

Tunis (sprawl share **0.17**) and the Egyptian Nile Delta cities (Mansura,
Damietta, Fayyum, Matariyya) — low sprawl, rising or flat density. The
counter-example to "African cities sprawl."

- **Framing:** callout box, not a section. What do they have in common —
  constrained land (Delta agriculture), older dense cores, planning capacity?

### 5. Quality of growth ≠ quantity

Density change is the "is this growth affordable to serve?" metric.

- **Juba** grew **+197 %** but *densified* (+0.049) — a young capital filling
  in after 2011.
- **Sodo, Nakuru, Hawassa** grew fast **and** thinned out — worst of both,
  though see caveat 2 on the Ethiopian/Kenyan polygons.
- Big cities that barely moved: **Kinshasa +16 %** (147 → 170 km²), Khartoum
  +17 %, **Cape Town +15 %**, Johannesburg +18 %, Luanda +24 %. Cape Town /
  Jo'burg are mature and already spread; Kinshasa's low number is almost
  certainly the static boundary (caveat 4) — which is itself a story about how
  African cities get measured.

### 6. The grid catches up — mostly

Many cities went from majority-unlit built-up in 2000 to near-fully-lit by
2024: **Kampala** 86 % → 5 %, **Dar es Salaam** 79 % → 0.2 %, **Kumasi**
80 % → 0 %, **Ibadan** 90 % → 0 %, **Mogadishu** 96 % → 2 %. Real
electrification progress.

- **Caveat:** magnitudes inflated by DMSP→VIIRS (caveat 3). Frame as
  direction and rank, not "X fell by 90 points."
- **Chart:** slope chart, unlit share 2000 vs 2025, one line per city.

### 7. Where the city outran the grid

**17 of 100 cities are still >50 % unlit built-up in 2025** — concentrated in
East Africa (the Kisumu and Uganda-border clusters), Ethiopian secondary
cities (Sodo 97 %, Hawassa 92 %), eastern DRC (Bukavu 84 %), and Bafoussam
(Cameroon, 86 %). Meanwhile **21 cities are essentially 100 % lit** — all of
North Africa, plus Abidjan and Accra.

- **Framing:** "The buildings arrived; the services didn't."
- **Chart:** map or ranked bar of unlit share 2025; two-colour split at 50 %.

### 8. Tree cover under pressure (weak — needs more data)

Only **Uyo** (Nigeria, 70 % tree cover, +78 % growth) clears a "high canopy +
fast growth" bar. The `p_tree_cov` field is sparse (many NA) and single-date
(2020), so this is a per-city detail at best, not a section. Revisit if a
time-varying canopy layer is added.

---

## Candidate charts (for the doc / dashboard companion)

1. Absolute vs relative growth scatter (exists — Scatterplots tab).
2. Sprawl share vs density change, coloured by region (exists — Scatterplots tab).
3. Unlit-share slope chart 2000 → 2025.
4. Region-grouped bars: growth %, sprawl share, density change.
5. Intensive vs extensive decomposition, stacked bar, top 20 by growth.
6. Small-multiples footprint maps for 6–8 exemplar cities (Cairo, Lagos,
   Kigali, Abuja, Tunis, Kinshasa) — the "expansion by period" style.

## To do / to verify

- [ ] Fix the 5 NaN North African cities (zonal extraction in
      `1_extract_ghsl.R` → rebuild `africapolis_builtup.Rds` → `city_index`).
- [ ] Build a "clean" city list that flags the Africapolis multi-town cluster
      polygons (Kisumu, Kikima, the cross-border `[uga]` / `[zar]` rows) so
      rankings can exclude or footnote them.
- [ ] Confirm the DMSP→VIIRS handling in the NTL series and decide how much
      weight the lit/unlit numbers can carry.
- [ ] Once GHS-POP lands: add per-capita built-up and population-growth
      columns — turns several of these stories from "land" to "land per person".
- [ ] Decide the doc's format: extend this markdown, or promote to a
      scrollytelling Artifact with the charts inline.

## Sources

- GHSL BUILT-S R2023 — European Commission JRC.
- Africapolis 2020 — OECD/SWAC (agglomeration boundaries, population, tree cover).
- Nighttime lights — harmonised DMSP-OLS + VIIRS (sat-io), 2000–2024.
