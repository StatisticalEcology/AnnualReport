// SEEC people network: Kumu-style map built on vis-network.
// Data comes from the <script id="network-data"> block written by network.qmd.
(function () {
  const raw = JSON.parse(document.getElementById("network-data").textContent);
  const container = document.getElementById("seec-network");
  if (!container || typeof vis === "undefined") return;

  // Fixed colour per category for each grouping (colour follows the entity).
  const PALETTE = ["#2a78d6", "#eb6834", "#1baf7a", "#eda100", "#e87ba4", "#008300", "#4a3aa7"];
  const GREY = "#9a9893";
  const LIGHT_GREY = "#c9c7c0";
  const GROUPINGS = {
    role: { label: "Role", order: raw.levels.role, quiet: ["Other collaborator"] },
    department: { label: "Department", order: raw.levels.department, quiet: ["Not recorded", "Outside SEEC"] },
    community: { label: "Cluster", order: raw.levels.community, quiet: ["Smaller clusters"] },
    degree_group: { label: "Degree", order: raw.levels.degree_group, quiet: ["Unknown", "Staff and collaborators"] },
  };
  const EDGE_STYLE = {
    "Supervision":   { color: "#52514e", dashes: false, arrows: { to: { enabled: true, scaleFactor: 0.45 } } },
    "Co-authorship": { color: "#8fb5e6", dashes: false, arrows: { to: { enabled: false } } },
    "Shared grant":  { color: "#e3a07f", dashes: [6, 4], arrows: { to: { enabled: false } } },
  };

  const state = {
    groupBy: "role",
    edgeTypes: new Set(Object.keys(EDGE_STYLE)),
    show: { "Current student": true, "Graduated student": true, "Other collaborator": true },
    collapsed: false,
    selected: null,
  };

  const byId = new Map(raw.nodes.map((n) => [n.id, n]));

  function colourFor(groupBy, value) {
    const g = GROUPINGS[groupBy];
    const qi = g.quiet.indexOf(value);
    if (qi >= 0) return qi === 0 ? GREY : LIGHT_GREY;
    const loud = g.order.filter((v) => !g.quiet.includes(v));
    const i = loud.indexOf(value);
    return i >= 0 && i < PALETTE.length ? PALETTE[i] : GREY;
  }

  function nodeVisible(n) {
    if (n.role in state.show && !state.show[n.role]) return false;
    return true;
  }

  function styledNode(n) {
    const c = colourFor(state.groupBy, n[state.groupBy]);
    return {
      id: n.id,
      label: n.name,
      value: Math.max(1, n.links),
      grp: n[state.groupBy],
      hidden: !nodeVisible(n),
      color: { background: c, border: "#ffffff", highlight: { background: c, border: "#0b0b0b" },
               hover: { background: c, border: "#0b0b0b" } },
      title: n.name + " · " + n.role,
    };
  }

  function styledEdge(e, i) {
    const s = EDGE_STYLE[e.type];
    return {
      id: i, from: e.from, to: e.to, type: e.type,
      value: e.weight,
      hidden: !state.edgeTypes.has(e.type),
      color: { color: s.color, highlight: "#0b0b0b", hover: "#0b0b0b", opacity: 0.75 },
      dashes: s.dashes, arrows: s.arrows,
      title: e.type + (e.detail ? " · " + e.detail : ""),
    };
  }

  const nodes = new vis.DataSet(raw.nodes.map(styledNode));
  const edges = new vis.DataSet(raw.edges.map(styledEdge));

  const network = new vis.Network(container, { nodes, edges }, {
    nodes: {
      shape: "dot",
      borderWidth: 2,
      scaling: { min: 10, max: 46, label: { enabled: true, min: 12, max: 30, drawThreshold: 8, maxVisible: 30 } },
      font: { face: "Inter, system-ui, sans-serif", color: "#0b0b0b", strokeWidth: 3, strokeColor: "#ffffff" },
    },
    edges: {
      smooth: { type: "continuous" },
      scaling: { min: 1, max: 6 },
      selectionWidth: 1.5,
    },
    physics: {
      solver: "forceAtlas2Based",
      forceAtlas2Based: { gravitationalConstant: -45, centralGravity: 0.02, springLength: 70,
                          springConstant: 0.08, avoidOverlap: 0.5 },
      stabilization: { iterations: 400, updateInterval: 50 },
    },
    interaction: { hover: true, tooltipDelay: 120, navigationButtons: false, keyboard: false },
  });

  const loading = document.getElementById("network-loading");
  network.once("stabilizationIterationsDone", () => {
    network.setOptions({ physics: { enabled: false } });
    network.fit();
    if (loading) loading.remove();
  });

  // ---- Restyle when controls change ----
  function refresh() {
    if (state.collapsed) expandAll();
    nodes.update(raw.nodes.map(styledNode));
    edges.update(raw.edges.map(styledEdge));
    renderLegend();
    if (state.collapsed) collapseAll();
    if (state.selected !== null) showDetails(state.selected);
  }

  // ---- Collapse each group into one bubble (click a bubble to open it) ----
  let savedPositions = null;
  function collapseAll() {
    savedPositions = network.getPositions();
    const g = GROUPINGS[state.groupBy];
    g.order.forEach((value) => {
      const members = raw.nodes.filter((n) => n[state.groupBy] === value && nodeVisible(n));
      if (members.length < 2) return;
      const c = colourFor(state.groupBy, value);
      network.cluster({
        joinCondition: (opts) => opts.grp === value && !opts.hidden,
        clusterNodeProperties: {
          id: "cluster:" + value, label: value + "\n" + members.length + " people",
          shape: "dot", size: 22 + 6 * Math.sqrt(members.length),
          scaling: { label: { enabled: false } },
          color: { background: c, border: "#ffffff" },
          font: { size: 22, face: "Inter, system-ui, sans-serif", strokeWidth: 5, strokeColor: "#ffffff" },
          title: "Click to open " + value,
        },
      });
    });
    // Lay the bubbles out on a circle so they never overlap
    const ids = Object.keys(network.body.nodes).filter((id) => network.isCluster(id));
    const radius = Math.max(200, ids.length * 38);
    ids.forEach((id, i) => {
      const angle = (2 * Math.PI * i) / ids.length - Math.PI / 2;
      network.moveNode(id, radius * Math.cos(angle), radius * Math.sin(angle));
    });
    setTimeout(() => network.fit({ animation: { duration: 400 } }), 60);
  }
  function expandAll() {
    Object.keys(network.body.nodes)
      .filter((id) => network.isCluster(id))
      .forEach((id) => network.openCluster(id));
    // Put everyone back where they were before collapsing
    if (savedPositions) {
      nodes.update(Object.entries(savedPositions).map(([id, p]) => ({ id: Number(id), x: p.x, y: p.y })));
      savedPositions = null;
      network.fit({ animation: true });
    }
  }

  // ---- Legend ----
  function renderLegend() {
    const el = document.getElementById("network-legend");
    const g = GROUPINGS[state.groupBy];
    const counts = {};
    raw.nodes.forEach((n) => { if (nodeVisible(n)) counts[n[state.groupBy]] = (counts[n[state.groupBy]] || 0) + 1; });
    el.innerHTML = "<div class='legend-title'>" + g.label + "</div>" +
      g.order.filter((v) => counts[v]).map((v) =>
        "<div class='legend-row'><span class='legend-dot' style='background:" + colourFor(state.groupBy, v) +
        "'></span><span>" + v + "</span><span class='legend-count'>" + counts[v] + "</span></div>").join("") +
      "<div class='legend-title legend-gap'>Connections</div>" +
      "<div class='legend-row'><span class='legend-line'></span>Supervision (arrow points to student)</div>" +
      "<div class='legend-row'><span class='legend-line coauthor'></span>Co-authored papers</div>" +
      "<div class='legend-row'><span class='legend-line grant'></span>Shared grants</div>" +
      "<div class='legend-note'>Dot size shows how many people someone is connected to.</div>";
  }

  // ---- Details panel ----
  function showDetails(id) {
    const n = byId.get(id);
    const el = document.getElementById("network-details");
    if (!n) { el.innerHTML = el.dataset.empty; return; }
    state.selected = id;
    const links = raw.edges.filter((e) => (e.from === id || e.to === id) && state.edgeTypes.has(e.type));
    const section = (title, list) => list.length ? "<div class='detail-section'><div class='detail-head'>" + title +
      " <span class='legend-count'>" + list.length + "</span></div>" + list.join("") + "</div>" : "";
    const person = (otherId, extra) => {
      const o = byId.get(otherId);
      return "<a href='#' class='detail-person' data-id='" + otherId + "'>" + o.name + "</a>" +
        (extra ? "<span class='detail-extra'>" + extra + "</span>" : "") + "<br>";
    };
    const students = links.filter((e) => e.type === "Supervision" && e.from === id).map((e) => person(e.to, e.detail));
    const supervisors = links.filter((e) => e.type === "Supervision" && e.to === id).map((e) => person(e.from, e.detail));
    const coauthors = links.filter((e) => e.type === "Co-authorship")
      .sort((a, b) => b.weight - a.weight).map((e) => person(e.from === id ? e.to : e.from, e.detail));
    const cogrants = links.filter((e) => e.type === "Shared grant").map((e) => person(e.from === id ? e.to : e.from, e.detail));
    const facts = [
      ["Role", n.role], ["Department", n.department_raw || n.department],
      n.degrees ? ["Degree", n.degrees] : null,
      ["Cluster", n.community],
    ].filter(Boolean).map(([k, v]) => "<div class='detail-fact'><span>" + k + "</span>" + v + "</div>").join("");
    const stats = "<div class='detail-stats'>" +
      [["Students", n.n_students], ["Papers", n.n_papers], ["Grants", n.n_grants]]
        .map(([k, v]) => "<div><b>" + v + "</b>" + k + "</div>").join("") + "</div>";
    el.innerHTML = "<div class='detail-name'>" + n.name + "</div>" + facts + stats +
      section("Supervised", students) + section("Supervisors", supervisors) +
      section("Co-authors", coauthors) + section("Shared grants with", cogrants);
    el.querySelectorAll(".detail-person").forEach((a) => a.addEventListener("click", (ev) => {
      ev.preventDefault(); focusPerson(Number(a.dataset.id));
    }));
  }

  function focusPerson(id) {
    if (state.collapsed) { document.getElementById("net-collapse").checked = false; state.collapsed = false; refresh(); }
    const n = byId.get(id);
    if (n && !nodeVisible(n)) { state.show[n.role] = true; syncShowBoxes(); refresh(); }
    network.selectNodes([id]);
    network.focus(id, { scale: 1.2, animation: { duration: 500, easingFunction: "easeInOutQuad" } });
    showDetails(id);
  }

  network.on("click", (params) => {
    if (params.nodes.length) {
      const id = params.nodes[0];
      if (network.isCluster(id)) { network.openCluster(id); return; }
      showDetails(id);
    } else {
      state.selected = null;
      showDetails(null);
    }
  });

  // ---- Controls ----
  document.getElementById("net-group").addEventListener("change", (e) => { state.groupBy = e.target.value; refresh(); });
  document.querySelectorAll("[data-edge-type]").forEach((box) => box.addEventListener("change", () => {
    box.checked ? state.edgeTypes.add(box.dataset.edgeType) : state.edgeTypes.delete(box.dataset.edgeType);
    refresh();
  }));
  function syncShowBoxes() {
    document.querySelectorAll("[data-show-role]").forEach((box) => { box.checked = state.show[box.dataset.showRole]; });
  }
  document.querySelectorAll("[data-show-role]").forEach((box) => box.addEventListener("change", () => {
    state.show[box.dataset.showRole] = box.checked; refresh();
  }));
  document.getElementById("net-collapse").addEventListener("change", (e) => {
    state.collapsed = e.target.checked;
    state.collapsed ? collapseAll() : expandAll();
  });
  document.getElementById("net-fit").addEventListener("click", () => network.fit({ animation: true }));

  const search = document.getElementById("net-search");
  const list = document.getElementById("net-people");
  list.innerHTML = raw.nodes.map((n) => n.name).sort().map((s) => "<option value=\"" + s + "\">").join("");
  search.addEventListener("change", () => {
    const hit = raw.nodes.find((n) => n.name.toLowerCase() === search.value.trim().toLowerCase());
    if (hit) focusPerson(hit.id);
  });

  const detailsEl = document.getElementById("network-details");
  detailsEl.dataset.empty = detailsEl.innerHTML;
  renderLegend();
})();
