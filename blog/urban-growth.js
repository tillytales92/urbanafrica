// Scroll-driven bubble chart for the "Shape of Growth" blog post.
// Expects in the page: #ug-scrolly (with #ug-chart, #ug-year, #ug-legend and
// .ug-step[data-slug] elements), #ug-explore (with #ug-chart2, #ug-legend2,
// #ug-play, #ug-slider, #ug-year2) and window.UG_DATA_URL pointing at the JSON.
// Colours come from the site's CSS variables, so light/dark mode just works.
(function () {
  "use strict";

  var REGION_COLOR = {
    "Northern Africa": "#3b82b8",
    "West Africa": "#d1603d",
    "Central Africa": "#8a6bb8",
    "East Africa": "#2a9d78",
    "Southern Africa": "#d39a2b",
  };
  var REGIONS = Object.keys(REGION_COLOR);
  var HL_SLUGS = ["lagos", "johannesburg", "kinshasa", "al_qahira", "nairobi", "kigali", "abuja", "hawassa", "tunis", "juba"];
  var fmt = function (v) {
    return d3.format(",.0f")(v);
  };

  var tooltip = document.createElement("div");
  tooltip.className = "ug-tooltip";
  document.body.appendChild(tooltip);
  function showTip(evt, d, year) {
    tooltip.innerHTML =
      "<strong>" +
      d.name +
      "</strong> (" +
      d.iso3 +
      "), " +
      year +
      "<br>Built-up: " +
      fmt(d.area) +
      " km²<br>Population: " +
      fmt(d.pop) +
      "<br>Density: " +
      fmt(d.density) +
      " per km²";
    tooltip.style.left = evt.clientX + 12 + "px";
    tooltip.style.top = evt.clientY + 12 + "px";
    tooltip.style.opacity = 1;
  }
  function hideTip() {
    tooltip.style.opacity = 0;
  }

  function start() {
    fetch(window.UG_DATA_URL)
      .then(function (r) {
        return r.json();
      })
      .then(init)
      .catch(function (err) {
        var el = document.getElementById("ug-scrolly");
        if (el) el.innerHTML = "<p><em>The interactive chart could not be loaded (" + err + ").</em></p>";
      });
  }

  function init(RAW) {
    var byCity = {};
    RAW.cities.forEach(function (r) {
      (byCity[r.slug] = byCity[r.slug] || []).push(r);
    });
    Object.keys(byCity).forEach(function (s) {
      byCity[s].sort(function (a, b) {
        return a.year - b.year;
      });
    });
    var allSlugs = Object.keys(byCity);
    var areaExt = d3.extent(RAW.cities, function (d) {
      return d.area_km2;
    });
    var densExt = d3.extent(RAW.cities, function (d) {
      return d.density;
    });
    var popMax = d3.max(RAW.cities, function (d) {
      return d.pop;
    });

    // Linear interpolation between the five-year epochs.
    function interp(slug, year) {
      var rows = byCity[slug];
      var i = 0;
      while (i < rows.length - 2 && rows[i + 1].year < year) i++;
      var a = rows[i],
        b = rows[i + 1];
      var t = Math.max(0, Math.min(1, (year - a.year) / (b.year - a.year)));
      return {
        slug: slug,
        name: a.name,
        iso3: a.iso3,
        region: a.region,
        area: a.area_km2 + (b.area_km2 - a.area_km2) * t,
        pop: a.pop + (b.pop - a.pop) * t,
        density: a.density + (b.density - a.density) * t,
      };
    }

    function buildChart(svgSel, opts) {
      var W = 640,
        H = 440;
      var m = { top: 10, right: 84, bottom: 40, left: 64 };
      var iw = W - m.left - m.right,
        ih = H - m.top - m.bottom;
      var x = d3
        .scaleLog()
        .domain([areaExt[0] * 0.85, areaExt[1] * 1.15])
        .range([0, iw]);
      var y = d3
        .scaleLog()
        .domain([densExt[0] * 0.85, densExt[1] * 1.15])
        .range([ih, 0]);
      var r = d3.scaleSqrt().domain([0, popMax]).range([2, 26]);
      var xTicks = [10, 25, 50, 100, 250, 500, 900].filter(function (v) {
        return v >= x.domain()[0] && v <= x.domain()[1];
      });
      var yTicks = [2000, 5000, 10000, 25000, 50000, 100000].filter(function (v) {
        return v >= y.domain()[0] && v <= y.domain()[1];
      });

      var svg = d3
        .select(svgSel)
        .attr("viewBox", "0 0 " + W + " " + H)
        .attr("role", "img");
      svg.selectAll("*").remove();
      var g = svg.append("g").attr("transform", "translate(" + m.left + "," + m.top + ")");

      g.append("g").attr("class", "ug-grid").selectAll("line").data(yTicks).join("line").attr("x1", 0).attr("x2", iw).attr("y1", y).attr("y2", y);
      g.append("g")
        .attr("class", "ug-axis")
        .attr("transform", "translate(0," + ih + ")")
        .call(d3.axisBottom(x).tickValues(xTicks).tickFormat(d3.format(",")).tickSize(4))
        .call(function (a) {
          a.select(".domain").remove();
        });
      g.append("g")
        .attr("class", "ug-axis")
        .call(d3.axisLeft(y).tickValues(yTicks).tickFormat(d3.format(",")).tickSize(0).tickPadding(6))
        .call(function (a) {
          a.select(".domain").remove();
        });
      g.append("text")
        .attr("class", "ug-axis-label")
        .attr("x", iw)
        .attr("y", ih + 34)
        .attr("text-anchor", "end")
        .text("Built-up area, km² (log scale)");
      g.append("text")
        .attr("class", "ug-axis-label")
        .attr("transform", "rotate(-90)")
        .attr("x", 0)
        .attr("y", -54)
        .attr("text-anchor", "end")
        .text("People per km² (log scale)");

      var trailLayer = g.append("g");
      var mutedLayer = g.append("g");
      var hlLayer = g.append("g");
      var labelLayer = g.append("g");
      var line = d3
        .line()
        .x(function (d) {
          return x(d.area);
        })
        .y(function (d) {
          return y(d.density);
        })
        .curve(d3.curveCatmullRom.alpha(0.6));

      // Push labels apart vertically when they would overlap.
      function placeLabels(hl) {
        var labs = hl
          .map(function (d) {
            var lx = x(d.area) + r(d.pop) + 4;
            return { d: d, x: lx, y: y(d.density) + 4, w: d.name.length * 6.2 };
          })
          .sort(function (a, b) {
            return a.y - b.y;
          });
        for (var i = 1; i < labs.length; i++) {
          for (var j = 0; j < i; j++) {
            var a = labs[j],
              b = labs[i];
            var overlapX = a.x < b.x + b.w && b.x < a.x + a.w;
            if (overlapX && b.y - a.y < 12) b.y = a.y + 12;
          }
        }
        return labs;
      }

      function update(year, spotlight, region) {
        var pts = allSlugs.map(function (s) {
          return interp(s, year);
        });
        var muted = pts.filter(function (d) {
          return HL_SLUGS.indexOf(d.slug) < 0;
        });
        var hl = pts.filter(function (d) {
          return HL_SLUGS.indexOf(d.slug) >= 0;
        });
        var vis = function (d, off) {
          return !region || d.region === region ? 1 : off;
        };
        var shownYear = Math.round(year);

        mutedLayer
          .selectAll("circle")
          .data(muted, function (d) {
            return d.slug;
          })
          .join("circle")
          .attr("class", "ug-dot-muted")
          .attr("cx", function (d) {
            return x(d.area);
          })
          .attr("cy", function (d) {
            return y(d.density);
          })
          .attr("r", function (d) {
            return r(d.pop);
          })
          .style("fill", function (d) {
            return region && d.region === region ? REGION_COLOR[d.region] : null;
          })
          .style("opacity", function (d) {
            return vis(d, 0.1);
          })
          .on("mousemove", function (e, d) {
            showTip(e, d, shownYear);
          })
          .on("mouseleave", hideTip);

        hlLayer
          .selectAll("circle")
          .data(hl, function (d) {
            return d.slug;
          })
          .join("circle")
          .attr("class", function (d) {
            return "ug-dot" + (d.slug === spotlight ? " ug-spot" : "");
          })
          .attr("cx", function (d) {
            return x(d.area);
          })
          .attr("cy", function (d) {
            return y(d.density);
          })
          .attr("r", function (d) {
            return r(d.pop);
          })
          .attr("fill", function (d) {
            return REGION_COLOR[d.region];
          })
          .style("opacity", function (d) {
            return vis(d, 0.15);
          })
          .on("mousemove", function (e, d) {
            showTip(e, d, shownYear);
          })
          .on("mouseleave", hideTip);

        labelLayer
          .selectAll("text")
          .data(placeLabels(hl), function (l) {
            return l.d.slug;
          })
          .join("text")
          .attr("class", function (l) {
            return "ug-label" + (l.d.slug === spotlight ? " ug-spot" : "");
          })
          .attr("x", function (l) {
            return l.x;
          })
          .attr("y", function (l) {
            return l.y;
          })
          .style("opacity", function (l) {
            return vis(l.d, 0.15);
          })
          .text(function (l) {
            return l.d.name;
          });

        if (opts.trails && spotlight) {
          var cur = interp(spotlight, year);
          var trail = byCity[spotlight]
            .filter(function (row) {
              return row.year <= year;
            })
            .map(function (row) {
              return { area: row.area_km2, density: row.density };
            })
            .concat([{ area: cur.area, density: cur.density }]);
          trailLayer.selectAll("path").data([trail]).join("path").attr("class", "ug-trail").attr("stroke", REGION_COLOR[cur.region]).attr("d", line);
        } else {
          trailLayer.selectAll("path").remove();
        }
      }
      return { update: update };
    }

    function legend(container, interactive, onToggle) {
      container.innerHTML = "";
      var active = null;
      REGIONS.forEach(function (region) {
        var el = document.createElement(interactive ? "button" : "span");
        el.innerHTML = '<span class="ug-sw" style="background:' + REGION_COLOR[region] + '"></span>' + region;
        if (interactive) {
          el.type = "button";
          el.addEventListener("click", function () {
            active = active === region ? null : region;
            container.querySelectorAll("button").forEach(function (b, i) {
              b.classList.toggle("ug-dimmed", !!active && REGIONS[i] !== active);
            });
            onToggle(active);
          });
        }
        container.appendChild(el);
      });
      var other = document.createElement("span");
      other.innerHTML = '<span class="ug-sw ug-sw-muted"></span>Other 90 cities';
      container.appendChild(other);
    }

    // ---------- scroll-driven chart ----------
    var scrolly = document.getElementById("ug-scrolly");
    if (scrolly) {
      var chart1 = buildChart("#ug-chart", { trails: true });
      legend(document.getElementById("ug-legend"), false);
      var yearEl = document.getElementById("ug-year");
      var steps = Array.prototype.slice.call(scrolly.querySelectorAll(".ug-step"));
      var activeSlug = null;

      // Fill in each step's land/people change from the data.
      steps.forEach(function (el) {
        var rows = byCity[el.dataset.slug];
        if (!rows) return;
        var a = rows[0],
          b = rows[rows.length - 1];
        var pct = function (v0, v1) {
          var p = Math.round((v1 / v0 - 1) * 100);
          return (p >= 0 ? "+" : "") + p + "%";
        };
        var figs = el.querySelector(".ug-figs");
        if (figs) figs.textContent = "1990–2025: land " + pct(a.area_km2, b.area_km2) + ", people " + pct(a.pop, b.pop);
      });

      var ticking = false;
      function onScroll() {
        if (ticking) return;
        ticking = true;
        requestAnimationFrame(function () {
          var rect = scrolly.getBoundingClientRect();
          var total = rect.height - window.innerHeight;
          var p = total > 0 ? Math.max(0, Math.min(1, -rect.top / total)) : 0;
          var year = 1990 + p * 35;
          yearEl.textContent = Math.round(year);
          chart1.update(year, activeSlug, null);
          ticking = false;
        });
      }
      var io = new IntersectionObserver(
        function (entries) {
          entries.forEach(function (entry) {
            entry.target.classList.toggle("ug-active", entry.isIntersecting);
            if (entry.isIntersecting) activeSlug = entry.target.dataset.slug;
          });
          onScroll();
        },
        { rootMargin: "-72% 0px -12% 0px" }
      );
      steps.forEach(function (el) {
        io.observe(el);
      });
      window.addEventListener("scroll", onScroll, { passive: true });
      window.addEventListener("resize", onScroll);
      onScroll();
    }

    // ---------- free exploration ----------
    if (document.getElementById("ug-explore")) {
      var chart2 = buildChart("#ug-chart2", { trails: false });
      var slider = document.getElementById("ug-slider");
      var year2 = document.getElementById("ug-year2");
      var playBtn = document.getElementById("ug-play");
      var region = null,
        timer = null;
      function setYear(y) {
        slider.value = y;
        year2.textContent = Math.round(y);
        chart2.update(y, null, region);
      }
      function pause() {
        if (timer) {
          clearInterval(timer);
          timer = null;
          playBtn.textContent = "Play";
        }
      }
      legend(document.getElementById("ug-legend2"), true, function (reg) {
        region = reg;
        chart2.update(+slider.value, null, region);
      });
      slider.addEventListener("input", function () {
        pause();
        setYear(+slider.value);
      });
      playBtn.addEventListener("click", function () {
        if (timer) return pause();
        if (+slider.value >= 2025) setYear(1990);
        playBtn.textContent = "Pause";
        timer = setInterval(function () {
          var y = +slider.value + 0.5;
          if (y >= 2025) {
            setYear(2025);
            return pause();
          }
          setYear(y);
        }, 60);
      });
      setYear(1990);
    }
  }

  if (document.readyState === "loading") document.addEventListener("DOMContentLoaded", start);
  else start();
})();
