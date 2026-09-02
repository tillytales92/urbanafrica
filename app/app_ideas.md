# App improvement ideas

All ideas below are feasible without heavy runtime computation — they rely on data already pre-computed or trivially derived from `city_index` / per-city files.

Status legend: ✅ implemented · ⬜ not yet done

---

## Map tab

✅ **Period selector**
Replace the fixed 2000→2025 window with two `selectInput` dropdowns (start year / end year) using the six GHSL epochs. The per-city `res_wgs84.tif` and `nres_wgs84.tif` stacks already contain all six layers, so change rasters can be computed at load time from two small per-city files — no continental rasters needed. This dramatically increases analytical value for almost zero extra storage cost.

✅ **NTL intensity layer**
`ntl_wgs84.tif` is already pre-computed per city. Add it as a toggleable overlay in the layers control. Useful for contextualising where lit built-up ends and dark/informal settlements begin.

✅ **Lit / unlit built-up layer**
`lit_unlit_wgs84.tif` (tri-state: 0 = unbuilt, 1 = built-unlit, 2 = built-lit) is already on disk. Display as a categorical overlay with a three-colour legend. This directly shows the informal/unlit settlement footprint — one of the most distinctive outputs of the pipeline.

✅ **Click popup on metro boundary**
The collapsible info box is always visible. Additionally wiring `addPolygons(..., popup = ...)` means the stats appear on demand when users click the boundary — less visual clutter when the box is not needed.

---

## Time series tab

✅ **Population-normalised view**
`pop2020` is in `agglom_attrs.Rds` (one join away). Add a toggle to switch between absolute km² and built-up area per 1,000 residents. Essential for comparing cities of very different sizes.

✅ **Growth rate view**
Add a radio button: "Absolute (km²)" vs "Indexed to 2000 (2000 = 100)". The indexed view makes slow-growing large cities and fast-growing small cities directly comparable on the same scale.

✅ **Total built-up as a third facet**
Replaced faceted view with a variable selector (total / residential / non-residential / NTL intensity / unlit share). NTL and unlit share use annual data 2000–2024; GHSL variables use 6 epochs 2000–2025.

---

## New tabs

✅ **Rankings (league table)**
A `DT::datatable` showing all 100 cities with sortable/filterable columns: city, country, sub-region, built-up 2000, built-up 2025, absolute change, % change, tree cover, unlit share. Inline bar chart on the Growth column. CSV download button. Default sort: % growth descending.

✅ **All-cities scatter**
A `plotly::ggplotly` scatter: x = built-up 2000, y = built-up 2025 (both sqrt-scaled), dot size = `pop2020`, colour = UN sub-region. Dashed diagonal = no-change reference. Hovering shows city name, ISO3, area values, and % growth.

✅ **About / methodology**
A `nav_panel` with data sources (GHSL R2023, Africapolis, NASA Black Marble, Li et al. 2020), margin definitions (intensive vs extensive), lit/unlit classification note, and units explanation.

---

## UX / performance

✅ **URL state**
Use `shiny::updateQueryString` / `shiny::getQueryString` to encode the selected country and city in the URL. Users can bookmark or share a link to a specific city without copying a slug.

✅ **Unlit share in info box**
`unlit_share_2025` is already in `city_index`. Surface it in the collapsible info box alongside tree cover — it's the share of built-up area with no nighttime light, a direct informal-settlement proxy.

✅ **Loading spinner**
Wrap `leafletOutput` in `shinycssloaders::withSpinner()` so users see feedback while rasters load on city switch, rather than a blank map.

✅ **Sub-region presets on time series sidebar**
Added a sub-region dropdown + Load button below the existing quick-select buttons. Replaces the city selection with all cities in the chosen UN sub-region.

✅ **Mobile / narrow viewport layout**
CSS media query reduces map and plot height to `60vh` on screens narrower than 768 px, preventing the fixed `85vh` from overflowing on tablets/phones.

✅ **Caching loaded assets**
`load_assets()` reads three files from disk on every city switch. Wrapping the reactive in `bindCache(input$city)` serves repeat visits from memory, cutting IO on commonly selected cities.
