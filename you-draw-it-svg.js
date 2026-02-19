r2d3.onRender(function (data, svg, width, height, options) {
  svg.selectAll("*").remove();

  const mode = (options && options.mode) || "plain";
  const HIGHLIGHT_MODE = (mode === "highlight");
  const nPanels = data.n_panels || 20;

  // Make SVG responsive
  svg.attr("width", "100%")
    .attr("height", "100%")
    .style("display", "block");

  d3.xml(data.svg).then((xml) => {
    const imported = xml.documentElement;

    // Remove fixed sizing so it scales inside Shiny container
    imported.removeAttribute("width");
    imported.removeAttribute("height");

    const root = svg.append("g").attr("class", "lineup-root");
    root.node().appendChild(imported);

    // Preserve viewBox (for consistent coordinates)
    const vb = imported.getAttribute("viewBox");
    if (vb) {
      svg.attr("viewBox", vb).attr("preserveAspectRatio", "xMinYMin meet");
    }

    // Defs for clipPaths
    let defs = svg.select("defs");
    if (defs.empty()) defs = svg.append("defs");

    // UI layer (overlays, tools, drawings)
    const uiLayer = root.append("g")
      .attr("class", "ui-layer")
      .style("font-family", "sans-serif");


    // Helpers: geometry

    // Transform a local bbox into GLOBAL SVG user coordinates
    function localBBoxToGlobalBBox(el, svgNode) {
      const b = el.getBBox();
      const m = el.getCTM(); // local -> global (SVG user coords)
      if (!m) return { x: b.x, y: b.y, width: b.width, height: b.height };

      const pt = svgNode.createSVGPoint();
      function tx(x, y) {
        pt.x = x; pt.y = y;
        const q = pt.matrixTransform(m);
        return [q.x, q.y];
      }

      const p1 = tx(b.x, b.y);
      const p2 = tx(b.x + b.width, b.y);
      const p3 = tx(b.x, b.y + b.height);
      const p4 = tx(b.x + b.width, b.y + b.height);

      const xs = [p1[0], p2[0], p3[0], p4[0]];
      const ys = [p1[1], p2[1], p3[1], p4[1]];

      const minX = Math.min(...xs), maxX = Math.max(...xs);
      const minY = Math.min(...ys), maxY = Math.max(...ys);

      return { x: minX, y: minY, width: maxX - minX, height: maxY - minY };
    }

    // Find:
    //  - canvasNode: largest rect inside the panel group (best bbox reference)
    //  - outlineNode: ggplot border element (rect/path) matching the canvas bbox and has stroke
    function findPanelCanvasAndOutline(panelGroupSel, svgNode) {
      const rects = panelGroupSel.selectAll("rect").nodes();
      if (!rects.length) return null;

      // largest rect = panel canvas reference
      let canvas = null;
      let canvasBBoxLocal = null;
      let bestArea = -Infinity;

      rects.forEach(r => {
        try {
          const bb = r.getBBox();
          const area = bb.width * bb.height;
          if (area < 200) return; // skip tiny rects (ticks, etc.)
          if (area > bestArea) {
            bestArea = area;
            canvas = r;
            canvasBBoxLocal = bb;
          }
        } catch (e) {}
      });

      if (!canvas || !canvasBBoxLocal) return null;

      const bboxGlobal = localBBoxToGlobalBBox(canvas, svgNode);

      // look for an outline element whose bbox matches the canvas bbox,
      //    and that actually draws a stroke (ggplot panel.border)
      const candidates = panelGroupSel.selectAll("rect, path").nodes();
      const eps = 1.5;
      let outline = null;

      candidates.forEach(el => {
        try {
          const bb = el.getBBox();
          const close =
            Math.abs(bb.x - canvasBBoxLocal.x) < eps &&
            Math.abs(bb.y - canvasBBoxLocal.y) < eps &&
            Math.abs(bb.width  - canvasBBoxLocal.width) < eps &&
            Math.abs(bb.height - canvasBBoxLocal.height) < eps;

          if (!close) return;

          const sel = d3.select(el);
          const stroke = sel.attr("stroke");
          const sw = +sel.attr("stroke-width") || 0;

          if (stroke && stroke !== "none" && sw > 0) {
            outline = el; // take the first good one
          }
        } catch (e) {}
      });

      return { bboxGlobal, canvasNode: canvas, outlineNode: outline };
    }

    function makeClip(plotIndex, bbox) {
      const clipId = `clip_plot_${plotIndex}`;
      defs.select(`#${clipId}`).remove();

      const cp = defs.append("clipPath")
        .attr("id", clipId)
        .attr("clipPathUnits", "userSpaceOnUse");

      cp.append("rect")
        .attr("x", bbox.x)
        .attr("y", bbox.y)
        .attr("width", bbox.width)
        .attr("height", bbox.height);

      return clipId;
    }

    // Simplify points
    function simplify(points, tol = 2.0) {
      if (points.length <= 2) return points;
      const out = [points[0]];
      for (let i = 1; i < points.length - 1; i++) {
        const [x0, y0] = out[out.length - 1];
        const [x1, y1] = points[i];
        if (Math.hypot(x1 - x0, y1 - y0) >= tol) out.push(points[i]);
      }
      out.push(points[points.length - 1]);
      return out;
    }

    // Build SVG path from points
    function pointsToPath(pts, closed = false) {
      if (!pts.length) return "";
      const head = `M${pts[0][0]},${pts[0][1]}`;
      const tail = pts.slice(1).map(p => `L${p[0]},${p[1]}`).join("");
      return closed ? (head + tail + "Z") : (head + tail);
    }

  
    // Panel discovery (stable)

    const svgNode = svg.node();
    const panels = []; // { plotIndex, bbox, canvasSel, outlineSel }

    for (let k = 1; k <= nPanels; k++) {
      const panelId = `panel_${String(k).padStart(2, "0")}`;
      const panelGroup = root.select(`#${panelId}`);
      if (panelGroup.empty()) continue;

      const info = findPanelCanvasAndOutline(panelGroup, svgNode);
      if (!info) continue;

      panels.push({
        plotIndex: k,
        bbox: info.bboxGlobal,
        canvasSel: d3.select(info.canvasNode),
        outlineSel: info.outlineNode ? d3.select(info.outlineNode) : null
      });
    }

    if (!panels.length) {
      console.warn("No panels found. Expected #panel_01..#panel_20 in SVG.");
      return;
    }

    // Plain / record mode
    if (!HIGHLIGHT_MODE) {
      // No overlays, no pointer events.
      return;
    }

  
    // Highlight mode state
    const MAX_SEL = 2;
    let selected = new Set();
    let order = [];
    let activeDrawPlot = null;

    // plotIndex -> state
    const P = new Map();

    function showTools(plotIndex, show) {
      const o = P.get(plotIndex);
      if (!o) return;
      o.toolsG.style("display", show ? null : "none");
      o.toolsG.raise();
    }

    function setPanelBorder(plotIndex, on) {
      const o = P.get(plotIndex);
      if (!o) return;

      // Use ggplot outline if available; otherwise fall back to canvas rect
      o.borderSel
        .attr("stroke", on ? "#E53935" : o.origStroke)
        .attr("stroke-width", on ? 8 : o.origStrokeWidth)
        .attr("vector-effect", "non-scaling-stroke");

      showTools(plotIndex, on);
      if (!on && activeDrawPlot === plotIndex) activeDrawPlot = null;
    }

    function activateDrawing(plotIndex) {
      activeDrawPlot = plotIndex;
      P.forEach((o, idx) => {
        o.overlay.style("cursor", (idx === plotIndex) ? "crosshair" : "pointer");
      });
    }

  // Helper to clear drawings for a panel
    function clearDrawings(plotIndex, notify = true) {
      const o = P.get(plotIndex);
      if (!o) return;

  // remove all highlight polygons in that panel
      o.drawGroup.selectAll("path").remove();

  // if that panel was active for drawing, turn it off
      if (activeDrawPlot === plotIndex) activeDrawPlot = null;

  // notify Shiny so your buf$regions can be updated
      if (notify && HTMLWidgets.shinyMode) {
        Shiny.setInputValue("highlight_cleared", { plotIndex }, { priority: "event" });
      }
    }

    function hasDrawing(plotIndex) {
      const o = P.get(plotIndex);
      if (!o) return false;
      return o.drawGroup.selectAll("path").size() > 0;
    }

    function clearDrawings(plotIndex, notify = true) {
      const o = P.get(plotIndex);
      if (!o) return;

      o.drawGroup.selectAll("path").remove();

      if (activeDrawPlot === plotIndex) activeDrawPlot = null;

      if (notify && HTMLWidgets.shinyMode) {
        Shiny.setInputValue("highlight_cleared", { plotIndex }, { priority: "event" });
      }
    }

    // Build per-panel UI
    
    panels.forEach(({ plotIndex, bbox, canvasSel, outlineSel }) => {
      const clipId = makeClip(plotIndex, bbox);

      const g = uiLayer.append("g").attr("class", `panel-ui-${plotIndex}`);

      // Drawings (clipped)
      const drawGroup = g.append("g")
        .attr("class", `draw-group-${plotIndex}`)
        .attr("clip-path", `url(#${clipId})`);

      // Overlay for click/draw in global coords (correct alignment)
      const overlay = g.append("rect")
        .attr("x", bbox.x)
        .attr("y", bbox.y)
        .attr("width", bbox.width)
        .attr("height", bbox.height)
        .attr("fill", "transparent")
        .style("pointer-events", "all")
        .style("cursor", "pointer")
        .style("touch-action", "none");

      // Tools above the panel
      const toolsG = g.append("g")
        .attr("class", `tools-${plotIndex}`)
        .style("display", "none")
        .style("pointer-events", "all");

      const btnW = 62, btnH = 18, gap = 14, r = 4, fontSize = 10;
      const totalW = btnW * 2 + gap;

      const tx = bbox.x + (bbox.width - totalW) / 2;
      let ty = bbox.y - btnH + 1; // flush to border (slight overlap)
      if (ty < 0) ty = bbox.y + 2; // fallback for top row to avoid clipping

      toolsG.attr("transform", `translate(${tx},${ty})`);

      // Highlight button
      const hb = toolsG.append("g").style("cursor", "pointer");
      const hbRect = hb.append("rect")
        .attr("width", btnW).attr("height", btnH).attr("rx", r)
        .attr("fill", "#FFD54F")
        .attr("stroke", "#946200")
        .attr("stroke-width", 1.2);

      hb.append("text")
        .attr("x", btnW / 2).attr("y", btnH / 2)
        .attr("text-anchor", "middle")
        .attr("dominant-baseline", "middle")
        .attr("font-size", fontSize)
        .attr("font-weight", 700)
        .text("Highlight");

      hb.on("click", (event) => {
        event.stopPropagation();
        if (!selected.has(plotIndex)) return;
        activateDrawing(plotIndex);
      });

      // Undo button
      const ub = toolsG.append("g")
        .attr("transform", `translate(${btnW + gap},0)`)
        .style("cursor", "pointer");

      const ubRect = ub.append("rect")
        .attr("width", btnW).attr("height", btnH).attr("rx", r)
        .attr("fill", "#ECEFF1")
        .attr("stroke", "#607D8B")
        .attr("stroke-width", 1.2);

      ub.append("text")
        .attr("x", btnW / 2).attr("y", btnH / 2)
        .attr("text-anchor", "middle")
        .attr("dominant-baseline", "middle")
        .attr("font-size", fontSize)
        .attr("font-weight", 700)
        .text("Undo");

      ub.on("click", (event) => {
        event.stopPropagation();
        const paths = drawGroup.selectAll("path").nodes();
        if (paths.length) d3.select(paths[paths.length - 1]).remove();

        if (HTMLWidgets.shinyMode) {
          Shiny.setInputValue("highlight_cleared", { plotIndex }, { priority: "event" });
        }
      });

      // Choose which element is the "border" we recolor:
      // ggplot outline if found; else fallback to canvas rect
      const borderSel = outlineSel || canvasSel;

      // Capture original border style so we can restore
      const origStroke = borderSel.attr("stroke") || "grey40";
      const origStrokeWidth = +borderSel.attr("stroke-width") || 0.6;

      P.set(plotIndex, {
        plotIndex,
        bbox,
        borderSel,
        overlay,
        toolsG,
        drawGroup,
        origStroke,
        origStrokeWidth,
        justDrew: false
      });

      // ensure vector-effect on border
      borderSel.attr("vector-effect", "non-scaling-stroke");
    });

    // Selection logic (tap/click)
    P.forEach((o, plotIndex) => {
      o.overlay.on("click", (event) => {
        event.stopPropagation();
        if (o.justDrew) return;

        const was = selected.has(plotIndex);
        let nowSelected;

        if (was) {
          selected.delete(plotIndex);
          order = order.filter(x => x !== plotIndex);
          clearDrawings(plotIndex, true);
          nowSelected = false;
        } else {
          if (selected.size >= MAX_SEL) {
            let drop = null;

            for (const idx of order) {
              if (!hasDrawing(idx)) {
                drop = idx;
                break;
              }
            }
// If both selected plots already have drawings, drop oldest
          if (drop === null) drop = order[0];

            // Remove drop from selection + order
          selected.delete(drop);
          order = order.filter(x => x !== drop);

          if (hasDrawing(drop)) {
            clearDrawings(drop, true);
          }

          setPanelBorder(drop, false);
        }   

           
          selected.add(plotIndex);
          order.push(plotIndex);
          nowSelected = true;
        }

        setPanelBorder(plotIndex, nowSelected);

        if (HTMLWidgets.shinyMode) {
          Shiny.setInputValue("plot_clicked", {
            plotIndex,
            selected: nowSelected,
            action: nowSelected ? "select" : "deselect"
          }, { priority: "event" });
        }
      });
    });


    // Drawing (pointer events) - Works for mouse + touch + pen
    P.forEach((o, plotIndex) => {
      let drawing = false;
      let pts = [];
      let pathSel = null;
      let drawStartMs = null;
      let moved = 0;

      function startDraw(event) {
        if (activeDrawPlot !== plotIndex) return;
        if (!selected.has(plotIndex)) return;

        drawing = true;
        pts = [];
        moved = 0;
        drawStartMs = Date.now();

        pathSel = o.drawGroup.append("path")
          .attr("fill", "rgba(255,165,0,0.14)")
          .attr("stroke", "orange")
          .attr("stroke-width", 2.5)
          .attr("stroke-linejoin", "round")
          .attr("stroke-linecap", "round")
          .attr("vector-effect", "non-scaling-stroke");

        const target = event.currentTarget;
        if (target && target.setPointerCapture && event.pointerId != null) {
          try { target.setPointerCapture(event.pointerId); } catch (e) {}
        }

        const [x0, y0] = d3.pointer(event, svg.node());
        pts.push([x0, y0]);
        pathSel.attr("d", pointsToPath(pts, false));
      }

      function moveDraw(event) {
        if (!drawing) return;
        const [x, y] = d3.pointer(event, svg.node());
        const [px, py] = pts[pts.length - 1];
        const d = Math.hypot(x - px, y - py);
        moved += d;

        if (d >= 1.5) {
          pts.push([x, y]);
          pathSel.attr("d", pointsToPath(pts, false));
        }
      }

      function endDraw() {
        if (!drawing) return;
        drawing = false;

        const drawEndMs = Date.now();

        if (moved < 6 || pts.length < 3) {
          if (pathSel) pathSel.remove();
          pathSel = null;
          return;
        }

        pts = simplify(pts, 2.0);

        pathSel.attr("d", pointsToPath(pts.concat([pts[0]]), true));

        o.justDrew = true;
        setTimeout(() => { o.justDrew = false; }, 180);

        const b = o.bbox;
        const polyNorm = pts.map(([x, y]) => ({
          x: (x - b.x) / b.width,
          y: (y - b.y) / b.height
        }));

        if (HTMLWidgets.shinyMode) {
          Shiny.setInputValue("highlighted_region", {
            plotIndex,
            polygon: polyNorm,
            drawStartedAt: drawStartMs,
            drawEndedAt: drawEndMs,
            durationMs: drawEndMs - drawStartMs
          }, { priority: "event" });
        }
      }

      o.overlay
        .on("pointerdown", function (event) { startDraw(event); })
        .on("pointermove", function (event) { moveDraw(event); })
        .on("pointerup pointercancel pointerleave", function () { endDraw(); });
    });

    // Initialize borders off, tools hidden
    P.forEach((o, plotIndex) => setPanelBorder(plotIndex, false));
  });
});
