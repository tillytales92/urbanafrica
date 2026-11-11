---
layout: post
title: "The Shape of Growth: Land and People in Africa's 100 Largest Cities, 1990–2025"
date: 2026-11-11 09:00:00
description: Africa's largest cities more than doubled their population since 1990 but added far less land. An interactive look at how 100 cities grew, using GHSL built-up and population data.
tags: urbanisation africa remote-sensing ghsl interactive
categories: data-stories
related_posts: false
_styles: >
  #ug-scrolly { position: relative; margin: 2rem 0; }
  .ug-sticky { position: sticky; top: 56px; z-index: 2; background: var(--global-bg-color); padding: 0.5rem 0 0.75rem; border-bottom: 1px solid var(--global-divider-color); }
  .ug-head { display: flex; justify-content: space-between; align-items: baseline; font-size: 0.9rem; color: var(--global-text-color-light); }
  .ug-year { font-size: 1.6rem; font-weight: 600; color: var(--global-text-color); font-variant-numeric: tabular-nums; }
  .ug-svg { display: block; width: 100%; height: auto; max-height: min(52vh, 460px); }
  .ug-steps { padding: 4vh 0 35vh; }
  .ug-step { margin: 0 0 32vh; padding: 0.25rem 0 0.25rem 1rem; border-left: 3px solid var(--global-divider-color); color: var(--global-text-color-light); transition: color 0.2s, border-color 0.2s; }
  .ug-step.ug-active { border-left-color: var(--global-theme-color); color: var(--global-text-color); }
  .ug-step p { margin: 0.2rem 0; }
  .ug-step .ug-figs { font-size: 0.85rem; color: var(--global-text-color-light); }
  .ug-legend { display: flex; flex-wrap: wrap; gap: 0.25rem 1rem; font-size: 0.8rem; color: var(--global-text-color-light); }
  .ug-legend button { background: none; border: 0; padding: 0; color: inherit; font: inherit; cursor: pointer; }
  .ug-legend button:hover { color: var(--global-text-color); }
  .ug-legend .ug-dimmed { opacity: 0.4; }
  .ug-sw { display: inline-block; width: 0.65rem; height: 0.65rem; border-radius: 50%; margin-right: 0.35rem; vertical-align: baseline; }
  .ug-sw-muted { background: var(--global-text-color-light); opacity: 0.4; }
  .ug-grid line { stroke: var(--global-divider-color); }
  .ug-axis text { fill: var(--global-text-color-light); font-size: 11px; }
  .ug-axis line { stroke: var(--global-text-color-light); }
  .ug-axis-label { fill: var(--global-text-color-light); font-size: 11px; }
  .ug-dot-muted { fill: var(--global-text-color-light); fill-opacity: 0.3; }
  .ug-dot { fill-opacity: 0.85; stroke: var(--global-bg-color); stroke-width: 1; }
  .ug-dot.ug-spot { stroke: var(--global-text-color); stroke-width: 2; }
  .ug-label { fill: var(--global-text-color); font-size: 11px; }
  .ug-label.ug-spot { font-weight: 700; }
  .ug-trail { fill: none; stroke-width: 1.5; stroke-dasharray: 3 3; }
  .ug-controls { display: flex; align-items: center; gap: 1rem; margin: 0.5rem 0; }
  .ug-controls button { border: 1px solid var(--global-theme-color); color: var(--global-theme-color); background: none; border-radius: 4px; padding: 0.15rem 0.8rem; font-size: 0.9rem; }
  .ug-controls input { flex: 1; accent-color: var(--global-theme-color); }
  .ug-tooltip { position: fixed; pointer-events: none; opacity: 0; z-index: 1050; background: var(--global-card-bg-color); color: var(--global-text-color); border: 1px solid var(--global-divider-color); border-radius: 4px; padding: 0.4rem 0.6rem; font-size: 0.8rem; line-height: 1.4; box-shadow: 0 2px 6px rgba(0,0,0,0.15); }
---

Between 1990 and 2025, the built-up area of Africa's 100 largest cities grew from about 6,900 to 12,500 km², an increase of 81%. Over the same period their population went from 142 to 370 million, an increase of 161%. In the median city, each million residents now live on 35 km² of built-up land, down from 48 km² in 1990. On average, then, African cities have grown denser rather than more spread out. The average hides a lot of variation, though, and the chart below shows how differently individual cities grew.

Each bubble is a city. Its horizontal position is built-up area, its vertical position is population density, and its size is population. Both axes use a log scale and stay fixed throughout. As you scroll, time runs from 1990 to 2025, so a city that moves right and down is adding land faster than people, while one that moves up is filling in. Ten cities are highlighted and discussed along the way; the other 90 are shown in grey (hover over any bubble for its numbers).

<div id="ug-scrolly">
  <div class="ug-sticky">
    <div class="ug-head"><span>Built-up area vs. population density, 100 African cities</span><span class="ug-year" id="ug-year">1990</span></div>
    <svg class="ug-svg" id="ug-chart" aria-label="Bubble chart of built-up area against population density for 100 African cities"></svg>
    <div class="ug-legend" id="ug-legend"></div>
  </div>
  <div class="ug-steps">
    <div class="ug-step" data-slug="lagos"><p><strong>Lagos.</strong> A megacity where growth is dominated by sprawl, still expanding onto greenfield land at the edge.</p><p class="ug-figs"></p></div>
    <div class="ug-step" data-slug="johannesburg"><p><strong>Johannesburg.</strong> Growth mostly by infill: its 2025 edge is not far from where it was in 1990, while density nearly doubled.</p><p class="ug-figs"></p></div>
    <div class="ug-step" data-slug="kinshasa"><p><strong>Kinshasa.</strong> Population quadrupled, absorbed almost entirely within a footprint that barely grew. Part of the city had already spread beyond its mapped boundary by 1990, so the land growth here is a lower bound.</p><p class="ug-figs"></p></div>
    <div class="ug-step" data-slug="al_qahira"><p><strong>Cairo.</strong> The largest absolute gain in built-up land of any city in the sample, with land and population growing at roughly the same pace.</p><p class="ug-figs"></p></div>
    <div class="ug-step" data-slug="nairobi"><p><strong>Nairobi.</strong> The main city of East Africa, the fastest-growing region in the sample.</p><p class="ug-figs"></p></div>
    <div class="ug-step" data-slug="kigali"><p><strong>Kigali.</strong> A smaller capital growing quickly from a low 1990 base, with land outpacing population.</p><p class="ug-figs"></p></div>
    <div class="ug-step" data-slug="abuja"><p><strong>Abuja.</strong> Became Nigeria's capital in 1991. Very rapid early growth that has since slowed, with land and population rising in step.</p><p class="ug-figs"></p></div>
    <div class="ug-step" data-slug="hawassa"><p><strong>Hawassa.</strong> The clearest case of sprawl in the sample: from a very small, very dense 1990 core, land grew more than fivefold and density halved.</p><p class="ug-figs"></p></div>
    <div class="ug-step" data-slug="tunis"><p><strong>Tunis.</strong> The compact exception, where growth has come almost entirely from filling in rather than spreading out.</p><p class="ug-figs"></p></div>
    <div class="ug-step" data-slug="juba"><p><strong>Juba.</strong> Rapid expansion, especially after independence in 2011. Land almost quadrupled while population doubled, so density nearly halved.</p><p class="ug-figs"></p></div>
  </div>
</div>

## Explore all 100 cities

The same chart, without the narration. Press play or drag the slider to change the year, and click a region in the legend to show only its cities.

<div id="ug-explore">
  <div class="ug-controls"><button type="button" id="ug-play">Play</button><input type="range" id="ug-slider" min="1990" max="2025" step="0.5" value="1990" aria-label="Year"><span class="ug-year" id="ug-year2">1990</span></div>
  <svg class="ug-svg" id="ug-chart2" aria-label="Interactive bubble chart of built-up area against population density"></svg>
  <div class="ug-legend" id="ug-legend2"></div>
</div>

## Data and caveats

Built-up area and population come from the Global Human Settlement Layer (GHSL BUILT-S and GHS-POP, release R2023A, European Commission JRC), in five-year epochs from 1990 to 2025. Values between epochs are interpolated linearly for the animation. City boundaries are Africapolis 2020 agglomerations (OECD/SWAC), held fixed at their 2020 extent for all years.

Two caveats are worth keeping in mind. A few cities, notably Kinshasa and Luanda, had already spread beyond their mapped boundary by 1990, so their land growth is a floor rather than an estimate. And for several smaller cities, the large percentage changes partly reflect how the boundary polygon was drawn rather than actual growth.

<script src="https://cdn.jsdelivr.net/npm/d3@7.9.0/dist/d3.min.js"></script>
<script>window.UG_DATA_URL = "{{ '/assets/json/urban-growth.json' | relative_url }}";</script>
<script src="{{ '/assets/js/urban-growth.js' | relative_url }}"></script>
