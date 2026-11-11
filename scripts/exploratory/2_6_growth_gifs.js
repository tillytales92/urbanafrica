// 2_6_growth_gifs.js
// Paste into code.earthengine.google.com and click Run.
//
// Sentinel-2 true-colour growth GIFs, 2016-2025, for three growth-form-typology
// examples (sec 8, docs/insights-1990-2025.md).
//   ikorodu -- sprawl-dominant & accelerating (26.5 -> 95.7 km2, +261%,
//              1990-2025; a Lagos exurb, same "sprawling & accelerating"
//              cluster as Lagos itself but small enough to actually see)
//   tunis   -- infill-dominant & decelerating (cleanest infill case; kept --
//              its growth is genuinely subtle, which is itself the point of
//              contrast against ikorodu)
//   bamako  -- steady & mixed (+87%, same tag as Cairo but at 1/6th the area)
// For a blogpost -- one-off, not part of the pipeline. R/rgee sibling:
// scripts/exploratory/2_6_growth_gifs.R
//
// NOTE on an earlier version of this script: it used Sodo, not Ikorodu, as
// the sprawl example (the single biggest % grower in the 100-city sample,
// +580%). That looked almost empty on screen -- checked against the raw
// per-city raster and Sodo's Africapolis polygon is a *dispersed* rural-urban
// complex (built-up pixels fill only ~1.1% of its bounding box, same pattern
// as Nakuru/Hawassa/Embu), not a compact town, so there's no single visible
// "blob" to animate no matter the zoom. Ikorodu's built-up fills ~12% of its
// bbox (vs. Tunis 2.9%, Bamako 8.9%) -- a real, compact, growing town.
//
// Bboxes below are each city's *built-up pixel* bounding box (trimmed from
// data/intermediate/cities/<slug>/total_wgs84.tif, 2025 band), padded 40-50%
// for peri-urban context -- not the raw Africapolis polygon bbox (which is
// what produced the Sodo problem above).

var cities = {
  ikorodu: ee.Geometry.Rectangle([3.31033, 6.38870, 3.81311, 6.95301]),
  tunis:   ee.Geometry.Rectangle([9.57128, 36.05189, 11.28584, 37.23020]),
  bamako:  ee.Geometry.Rectangle([-8.29429, 12.28426, -7.58499, 12.97882])
};

var years = ee.List.sequence(2016, 2025);

// SCL-band mask: drop cloud shadow (3), medium/high-probability cloud (8, 9), thin cirrus (10).
function maskS2clouds(image) {
  var scl = image.select('SCL');
  var mask = scl.neq(3).and(scl.neq(8)).and(scl.neq(9)).and(scl.neq(10));
  return image.updateMask(mask);
}

function makeComposite(year, bounds) {
  year = ee.Number(year);
  var start = ee.Date.fromYMD(year, 1, 1);
  var end = start.advance(1, 'year');
  return ee.ImageCollection('COPERNICUS/S2_SR_HARMONIZED')
    .filterBounds(bounds)
    .filterDate(start, end)
    .filter(ee.Filter.lt('CLOUDY_PIXEL_PERCENTAGE', 60))
    .map(maskS2clouds)
    .select(['B4', 'B3', 'B2'])
    .median()
    .clip(bounds);
}

function buildGifForCity(name, bounds) {
  var composites = ee.ImageCollection(years.map(function (y) {
    return makeComposite(y, bounds);
  }));

  var visParams = {bands: ['B4', 'B3', 'B2'], min: 0, max: 3000, gamma: 1.3};

  // Quick visual sanity check in the map pane (first and last year).
  Map.addLayer(ee.Image(composites.toList(10).get(0)), visParams, name + ' 2016');
  Map.addLayer(ee.Image(composites.toList(10).get(9)), visParams, name + ' 2025');

  var gifParams = {
    region: bounds,
    dimensions: 768,
    crs: 'EPSG:3857',
    framesPerSecond: 0.5, // was 2, then 1 -- each frame now holds for 2s (20s total for 10 frames)
    min: 0,
    max: 3000,
    gamma: 1.3,
    bands: ['B4', 'B3', 'B2']
  };

  print(name + ' GIF URL:', composites.getVideoThumbURL(gifParams));
}

buildGifForCity('ikorodu', cities.ikorodu);
buildGifForCity('tunis', cities.tunis);
buildGifForCity('bamako', cities.bamako);

Map.centerObject(cities.ikorodu, 12);
