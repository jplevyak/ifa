
const DATA = JSON.parse(document.getElementById('pyc-compiler-data').textContent);

const totalPasses = (DATA.phase3_ifa && DATA.phase3_ifa.passes) ? DATA.phase3_ifa.passes.length : 0;
let currentPassIndex = 0;

function escapeHtml(str) {
  if (str === null || str === undefined) return '';
  return String(str).replace(/&/g, '&amp;').replace(/</g, '&lt;').replace(/>/g, '&gt;').replace(/"/g, '&quot;');
}

function getPassMeta(passNum) {
  const passes = (DATA.phase3_ifa && DATA.phase3_ifa.passes) || [];
  let pMeta = passes.find(p => p.pass === passNum);
  if (!pMeta) pMeta = passes.find(p => p.pass === (passNum - 1));
  if (!pMeta && passNum >= 1 && passNum <= passes.length) pMeta = passes[passNum - 1];
  if (!pMeta && passNum === 0 && passes.length > 0) pMeta = passes[0];
  return pMeta || null;
}

function ensurePassIndexed(passData) {
  if (!passData || passData._indexed) return passData;
  passData._indexed = true;
  passData._avarById = new Map();
  passData._avarsByVarId = new Map();
  passData._esById = new Map();
  (passData.avars || []).forEach(av => {
    passData._avarById.set(av.id, av);
    const addKey = (k) => {
      if (k === undefined || k === null || k < 0) return;
      let arr = passData._avarsByVarId.get(k);
      if (!arr) { arr = []; passData._avarsByVarId.set(k, arr); }
      if (!arr.includes(av)) arr.push(av);
    };
    addKey(av.var_id);
    addKey(av.orig_var_id);
  });
  (passData.entry_sets || []).forEach(es => {
    passData._esById.set(es.id, es);
  });
  return passData;
}

function ensureFunIndexed(f) {
  if (!f || f._indexed) return f;
  f._indexed = true;
  f._varById = new Map();
  (f.vars || []).forEach(v => f._varById.set(v.id, v));
  return f;
}

// Pass data is delta-encoded (phase3_ifa.encoding "delta-v1"; the writer
// is dump_html in ifa/common/html.cc). Each detailed pass lists the
// EntrySets, CreationSets and AVars that changed since the previous pass,
// plus the ids that went away; a "keyframe" pass replaces the state. AVars
// travel as [id, [type CS ids], [forward ids], [backward ids]], with their
// var/contour/function in avar_info and the CS ids in cs_info.
// materializePass rebuilds the object shape the views read.
let CS_INFO = null;
let AVAR_INFO = null;
function csInfo() {
  if (!CS_INFO) {
    CS_INFO = new Map();
    ((DATA.phase3_ifa && DATA.phase3_ifa.cs_info) || []).forEach(r => CS_INFO.set(r[0], r));
  }
  return CS_INFO;
}
function avarInfo() {
  if (!AVAR_INFO) {
    AVAR_INFO = new Map();
    ((DATA.phase3_ifa && DATA.phase3_ifa.avar_info) || []).forEach(r => AVAR_INFO.set(r[0], r));
  }
  return AVAR_INFO;
}
function typeObjects(ids) {
  const m = csInfo();
  return (ids || []).map(id => {
    const r = m.get(id);
    if (!r) return { cs: id, sym: '?' };
    const t = { cs: r[0], sym: r[1] };
    if (r[2]) { t.is_const = true; if (r[3] !== '') t.val = r[3]; }
    if (r[4] >= 0) t.arity = r[4];
    return t;
  });
}
function summaryOnlyPass(meta) {
  const sumObj = meta.summary || {};
  return { pass: meta.pass, summary: sumObj, summary_only: true, splits: sumObj.splits || [],
           entry_sets: [], creation_sets: [], avars: [], violations: [] };
}
function materializePass(passNum) {
  const passes = (DATA.phase3_ifa && DATA.phase3_ifa.passes) || [];
  const idx = passes.findIndex(p => p.pass === passNum);
  if (idx < 0) return null;
  const target = passes[idx];
  if (target.summary_only || !target.data) return summaryOnlyPass(target);
  let start = 0;
  for (let i = idx; i >= 0; i--) {
    if (passes[i].data && passes[i].data.keyframe) { start = i; break; }
  }
  const es = new Map(), cs = new Map(), av = new Map();
  for (let i = start; i <= idx; i++) {
    const d = passes[i].data;
    if (!d) continue;
    if (d.keyframe) { es.clear(); cs.clear(); av.clear(); }
    (d.es_gone || []).forEach(id => es.delete(id));
    (d.cs_gone || []).forEach(id => cs.delete(id));
    (d.av_gone || []).forEach(id => av.delete(id));
    (d.entry_sets || []).forEach(e => es.set(e.id, e));
    (d.creation_sets || []).forEach(c => cs.set(c.id, c));
    (d.avars || []).forEach(a => av.set(a[0], a));
  }
  const info = avarInfo();
  const avars = [];
  av.forEach(a => {
    const r = info.get(a[0]);
    avars.push({
      id: a[0],
      var_id: r ? r[1] : -1, orig_var_id: r ? r[2] : -1, contour_id: r ? r[3] : -1,
      fun_id: r ? r[4] : -1, orig_fun_id: r ? r[5] : -1,
      var_name: r ? r[6] : '', fun_name: r ? r[7] : '',
      type: typeObjects(a[1]), forward: a[2] || [], backward: a[3] || []
    });
  });
  const byId = (x, y) => x.id - y.id;
  avars.sort(byId);
  const d = target.data;
  return {
    pass: target.pass, summary: d.summary, splits: d.splits || [], violations: d.violations || [],
    entry_sets: Array.from(es.values()).sort(byId),
    creation_sets: Array.from(cs.values()).sort(byId),
    avars
  };
}
// A few materialized passes stay cached: the views ask for the current pass
// and its diff base over and over.
const PASS_CACHE = new Map();
function getPassData(passNum) {
  const pMeta = getPassMeta(passNum);
  if (!pMeta) return null;
  const key = pMeta.pass;
  if (PASS_CACHE.has(key)) {
    const v = PASS_CACHE.get(key);
    PASS_CACHE.delete(key);
    PASS_CACHE.set(key, v);
    return v;
  }
  const v = ensurePassIndexed(materializePass(key));
  PASS_CACHE.set(key, v);
  if (PASS_CACHE.size > 4) PASS_CACHE.delete(PASS_CACHE.keys().next().value);
  return v;
}

// Init Header
document.getElementById('hdr-program').textContent = DATA.meta.program;
document.getElementById('pill-backend').textContent = 'Backend: ' + DATA.meta.backend;
document.getElementById('pill-passes').textContent = DATA.meta.total_passes + ' Passes';
const lastPass = DATA.phase3_ifa.passes.length > 0 ? DATA.phase3_ifa.passes[DATA.phase3_ifa.passes.length - 1] : null;
const lastPassSummary = lastPass ? (lastPass.data ? lastPass.data.summary : lastPass.summary) : null;

if (lastPassSummary) {
  document.getElementById('pill-contours').textContent = lastPassSummary.ess_count + ' Contours';
  const vPill = document.getElementById('pill-viol');
  vPill.textContent = lastPassSummary.violations_count + ' Violations';
  vPill.className = 'stat-pill ' + (lastPassSummary.violations_count === 0 ? 'success' : 'warning');
}

// Init Dashboard
document.getElementById('card-lines').textContent = DATA.phase1_source.lines.length;
document.getElementById('card-funs').textContent = DATA.phase2_cfg.functions.length;
document.getElementById('card-passes').textContent = DATA.meta.total_passes;
if (lastPassSummary) {
  document.getElementById('card-ess').textContent = lastPassSummary.ess_count;
  document.getElementById('card-css').textContent = lastPassSummary.css_count;
}
document.getElementById('card-inlines').textContent = DATA.phase5_optimizations.inlining_events.length;

function initDashboardInlining() {
  const events = (DATA.phase5_optimizations && DATA.phase5_optimizations.inlining_events) || [];
  const card = document.getElementById('dashboard-inlining-card');
  const tbody = document.querySelector('#dashboard-inlines-table tbody');
  if (!card || !tbody || events.length === 0) return;
  card.style.display = 'block';
  tbody.innerHTML = '';
  events.forEach(ev => {
    const tr = document.createElement('tr');
    const callerLink = ev.caller_id >= 0
      ? `<a href="#" onclick="jumpToCFGAndNode(${ev.caller_id}, ${ev.pnode_id}); return false;" style="color:var(--primary); font-weight:600; text-decoration:underline;">${escapeHtml(ev.caller_name)} (#${ev.caller_id})</a>`
      : escapeHtml(ev.caller_name);
    const calleeExists = (DATA.phase2_cfg.functions || []).some(f => f.id === ev.callee_id);
    const calleeLink = calleeExists
      ? `<a href="#" onclick="jumpToCFG(${ev.callee_id}); return false;" style="color:var(--text-main); text-decoration:underline;">${escapeHtml(ev.callee_name)} (#${ev.callee_id})</a>`
      : `${escapeHtml(ev.callee_name)} <span style="color:var(--text-dim); font-size:0.75rem;">(#${ev.callee_id} elided)</span>`;
    tr.innerHTML = `
      <td>${callerLink}</td>
      <td>${calleeLink}</td>
      <td><code>#${ev.pnode_id}</code></td>
      <td><span class="badge badge-prim">${escapeHtml(ev.reason)}</span></td>
    `;
    tbody.appendChild(tr);
  });
}
initDashboardInlining();

// Render Trajectory SVG
function renderTrajectoryChart() {
  const container = document.getElementById('chart-trajectory');
  const passes = DATA.phase3_ifa.passes;
  if (!container || !passes || passes.length === 0) return;
  const w = Math.max(360, container.clientWidth || 800);
  const h = 240;
  const padL = 48, padR = 32, padT = 36, padB = 34;

  let maxVal = 4;
  passes.forEach(p => {
    const s = p.data ? p.data.summary : p.summary;
    if (s) {
      maxVal = Math.max(maxVal, s.ess_count || 0, s.css_count || 0, s.violations_count || 0);
    }
  });

  const getX = (idx) => passes.length === 1
    ? (padL + (w - padL - padR) / 2)
    : (padL + (idx / (passes.length - 1)) * (w - padL - padR));
  const getY = (val) => h - padB - (val / maxVal) * (h - padT - padB);

  let essPath = '', cssPath = '', violPath = '';
  let dotsAndLabels = '';
  const labelStride = Math.max(1, Math.ceil(passes.length / 16));

  passes.forEach((p, idx) => {
    const s = p.data ? p.data.summary : p.summary;
    if (!s) return;
    const passNum = idx + 1;
    const x = getX(idx);
    const yEss = getY(s.ess_count || 0);
    const yCss = getY(s.css_count || 0);
    const yViol = getY(s.violations_count || 0);

    essPath += (idx === 0 ? 'M' : 'L') + `${x},${yEss}`;
    cssPath += (idx === 0 ? 'M' : 'L') + `${x},${yCss}`;
    violPath += (idx === 0 ? 'M' : 'L') + `${x},${yViol}`;

    const tip = `Pass ${passNum}: ${s.ess_count || 0} EntrySets, ${s.css_count || 0} CreationSets, ${s.violations_count || 0} Violations`;
    dotsAndLabels += `
      <circle cx="${x}" cy="${yEss}" r="4.5" fill="#38bdf8" style="cursor:pointer;" onclick="jumpToPassContour(${passNum})"><title>${tip}</title></circle>
      <circle cx="${x}" cy="${yCss}" r="4.5" fill="#a855f7" style="cursor:pointer;" onclick="jumpToPassContour(${passNum})"><title>${tip}</title></circle>
      <circle cx="${x}" cy="${yViol}" r="4" fill="#ef4444" style="cursor:pointer;" onclick="jumpToPassContour(${passNum})"><title>${tip}</title></circle>
    `;
    if (idx % labelStride === 0 || idx === passes.length - 1) {
      dotsAndLabels += `<text x="${x}" y="${h - 10}" fill="#64748b" font-size="11" text-anchor="middle">P${passNum}</text>`;
    }
  });

  container.innerHTML = `
    <svg width="${w}" height="${h}" style="background:var(--code-bg); border-radius:6px; display:block;">
      <!-- Grid lines & Y-axis labels -->
      <line x1="${padL}" y1="${getY(0)}" x2="${w-padR}" y2="${getY(0)}" stroke="#334155" stroke-width="1"/>
      <text x="${padL - 8}" y="${getY(0) + 4}" fill="#64748b" font-size="11" text-anchor="end">0</text>
      <line x1="${padL}" y1="${getY(Math.round(maxVal/2))}" x2="${w-padR}" y2="${getY(Math.round(maxVal/2))}" stroke="#1e293b" stroke-dasharray="4"/>
      <text x="${padL - 8}" y="${getY(Math.round(maxVal/2)) + 4}" fill="#64748b" font-size="11" text-anchor="end">${Math.round(maxVal/2)}</text>
      <line x1="${padL}" y1="${getY(maxVal)}" x2="${w-padR}" y2="${getY(maxVal)}" stroke="#1e293b" stroke-dasharray="4"/>
      <text x="${padL - 8}" y="${getY(maxVal) + 4}" fill="#64748b" font-size="11" text-anchor="end">${maxVal}</text>

      <!-- Paths -->
      <path d="${essPath}" fill="none" stroke="#38bdf8" stroke-width="2"/>
      <path d="${cssPath}" fill="none" stroke="#a855f7" stroke-width="2"/>
      <path d="${violPath}" fill="none" stroke="#ef4444" stroke-width="2"/>

      <!-- Interactive Data Points & X-axis labels -->
      ${dotsAndLabels}

      <!-- Legend -->
      <circle cx="${padL + 10}" cy="18" r="4" fill="#38bdf8"/><text x="${padL + 20}" y="22" fill="#94a3b8" font-size="12">EntrySets (ESS)</text>
      <circle cx="${padL + 150}" cy="18" r="4" fill="#a855f7"/><text x="${padL + 160}" y="22" fill="#94a3b8" font-size="12">CreationSets (CSS)</text>
      <circle cx="${padL + 300}" cy="18" r="4" fill="#ef4444"/><text x="${padL + 310}" y="22" fill="#94a3b8" font-size="12">Violations</text>
    </svg>
  `;
}
renderTrajectoryChart();
window.addEventListener('resize', () => {
  const dash = document.getElementById('view-dashboard');
  if (dash && dash.classList.contains('active')) renderTrajectoryChart();
});

function switchTab(tabName) {
  document.querySelectorAll('.tab-btn').forEach(b => {
    b.classList.toggle('active', b.dataset.tab === tabName);
  });
  document.querySelectorAll('.view-panel').forEach(p => {
    p.classList.toggle('active', p.id === 'view-' + tabName);
  });
}

// Tabs Navigation
document.querySelectorAll('.tab-btn').forEach(btn => {
  btn.addEventListener('click', () => {
    const tab = btn.dataset.tab;
    switchTab(tab);
    if (tab === 'dashboard') renderTrajectoryChart();
    if (tab === 'callgraph') renderCallGraph();
    if (tab === 'cfg') renderCFG();
    if (tab === 'dataflow') {
      if (!currentDfData) {
        loadPassData(currentDfPass, (d) => {
          currentDfData = d;
          renderDataflowTable();
        });
      } else {
        renderDataflowTable();
      }
    }
  });
});

// Tab: Source & Codegen Split Viewer
const cgData = DATA.phase6_codegen || { language: DATA.meta.backend || 'C', filename: '', lines: [] };
let currentSourceTabMode = (cgData.lines && cgData.lines.length > 0) ? 'both' : 'py';

const srcPyHeaderTitle = document.getElementById('source-py-header-title');
if (srcPyHeaderTitle) srcPyHeaderTitle.textContent = `📜 Python Source (${DATA.meta.program || 'program'})`;
const srcCgHeaderTitle = document.getElementById('source-cg-header-title');
if (srcCgHeaderTitle) srcCgHeaderTitle.textContent = `⚙️ Generated ${cgData.language || 'Target'} (${cgData.filename || 'not emitted'})`;
const srcCgHeaderSub = document.getElementById('source-cg-header-sub');
if (srcCgHeaderSub) srcCgHeaderSub.textContent = `${(cgData.lines || []).length} lines`;
const srcCodegenMeta = document.getElementById('src-codegen-meta');
if (srcCodegenMeta) {
  srcCodegenMeta.textContent = cgData.filename
    ? `Target: ${cgData.filename} (${(cgData.lines || []).length} lines of ${cgData.language})`
    : `Python Source: ${(DATA.phase1_source.lines || []).length} lines`;
}

// Build line -> {fun, pnode} lookup for Python source lines
const pyLineToFunPNode = new Map();
(DATA.phase2_cfg.functions || []).forEach(f => {
  (f.pnodes || []).forEach(pn => {
    if (pn.line > 0 && !pyLineToFunPNode.has(pn.line)) {
      pyLineToFunPNode.set(pn.line, { fun: f, pn: pn });
    }
  });
});

function getFunctionPythonSpan(f) {
  if (!f) return { start: 0, end: 0 };
  let minL = f.line > 0 ? f.line : 0;
  let maxL = minL;
  (f.pnodes || []).forEach(pn => {
    if (pn.line > 0) {
      if (minL === 0 || pn.line < minL) minL = pn.line;
      if (pn.line > maxL) maxL = pn.line;
    }
  });
  return { start: minL, end: maxL };
}

const srcViewer = document.getElementById('source-code-viewer');
(DATA.phase1_source.lines || []).forEach((line, idx) => {
  const lineNum = idx + 1;
  const d = document.createElement('div');
  const hit = pyLineToFunPNode.get(lineNum);
  d.className = 'code-line' + (hit ? ' clickable-line' : '');
  d.id = 'src-line-' + lineNum;
  if (hit) {
    d.title = `Function ${hit.fun.name} — PNode #${hit.pn.id} (${hit.pn.kind}). Click to sync target code; double-click to inspect in CFG.`;
    d.addEventListener('click', () => {
      highlightSourceAndCodegenForFunction(hit.fun, lineNum);
    });
    d.addEventListener('dblclick', () => {
      jumpToCFGAndNode(hit.fun.id, hit.pn.id);
    });
  }
  d.innerHTML = `<span class="line-num">${lineNum}</span><span class="line-text">${escapeHtml(line)}</span>`;
  srcViewer.appendChild(d);
});

const cgViewer = document.getElementById('codegen-code-viewer');
if (cgViewer) {
  if (!cgData.lines || cgData.lines.length === 0) {
    cgViewer.innerHTML = `<div style="padding:1.5rem; color:var(--text-muted); text-align:center;">No generated ${escapeHtml(cgData.language || 'C/LLVM')} file was available when HTML was emitted.</div>`;
  } else {
    const cgFrag = document.createDocumentFragment();
    cgData.lines.forEach((line, idx) => {
      const lineNum = idx + 1;
      const d = document.createElement('div');
      d.className = 'code-line';
      d.id = 'cg-line-' + lineNum;
      d.innerHTML = `<span class="line-num">${lineNum}</span><span class="line-text">${escapeHtml(line)}</span>`;
      cgFrag.appendChild(d);
    });
    cgViewer.appendChild(cgFrag);
  }
}

const srcFunJumpSelect = document.getElementById('src-fun-jump-select');
if (srcFunJumpSelect) {
  (DATA.phase2_cfg.functions || []).forEach(f => {
    if (f.line > 0 || (f.cg_start_line && f.cg_start_line > 0)) {
      const opt = document.createElement('option');
      opt.value = f.id;
      const cgTag = (f.cg_start_line > 0) ? ` → ${f.cg_name} (L${f.cg_start_line})` : '';
      opt.textContent = `${f.name} (#${f.id})${cgTag}`;
      srcFunJumpSelect.appendChild(opt);
    }
  });
  srcFunJumpSelect.addEventListener('change', () => {
    const fId = parseInt(srcFunJumpSelect.value, 10);
    const f = (DATA.phase2_cfg.functions || []).find(fn => fn.id === fId);
    if (f) highlightSourceAndCodegenForFunction(f, f.line);
  });
}

function setSourceTabMode(mode) {
  currentSourceTabMode = mode;
  ['py', 'cg', 'both'].forEach(m => {
    const btn = document.getElementById('btn-src-mode-' + m);
    if (btn) btn.classList.toggle('active', m === mode);
  });
  const pyCol = document.getElementById('source-py-col');
  const cgCol = document.getElementById('source-cg-col');
  if (pyCol) pyCol.style.display = (mode === 'py' || mode === 'both') ? 'flex' : 'none';
  if (cgCol) cgCol.style.display = (mode === 'cg' || mode === 'both') ? 'flex' : 'none';
}
setSourceTabMode(currentSourceTabMode);

function highlightSourceAndCodegenForFunction(f, focusPyLine) {
  if (!f) return;
  document.querySelectorAll('#source-code-viewer .code-line.highlight-line, #source-code-viewer .code-line.fun-span-line, #source-code-viewer .code-line.pnode-match-line').forEach(el => {
    el.classList.remove('highlight-line', 'fun-span-line', 'pnode-match-line');
  });
  document.querySelectorAll('#codegen-code-viewer .code-line.highlight-line, #codegen-code-viewer .code-line.fun-span-line, #codegen-code-viewer .code-line.pnode-match-line').forEach(el => {
    el.classList.remove('highlight-line', 'fun-span-line', 'pnode-match-line');
  });

  const pySpan = getFunctionPythonSpan(f);
  if (pySpan.start > 0) {
    for (let l = pySpan.start; l <= pySpan.end; l++) {
      const el = document.getElementById('src-line-' + l);
      if (el) el.classList.add('fun-span-line');
    }
  }
  const targetPyLine = focusPyLine || pySpan.start;
  if (targetPyLine > 0) {
    const lineEl = document.getElementById('src-line-' + targetPyLine);
    if (lineEl) {
      lineEl.classList.add('highlight-line');
      lineEl.scrollIntoView({ behavior: 'smooth', block: 'center' });
    }
  }

  if (f.cg_start_line > 0 && f.cg_end_line >= f.cg_start_line) {
    for (let l = f.cg_start_line; l <= f.cg_end_line; l++) {
      const el = document.getElementById('cg-line-' + l);
      if (el) el.classList.add('fun-span-line');
    }
    const cgStartEl = document.getElementById('cg-line-' + f.cg_start_line);
    if (cgStartEl) {
      cgStartEl.classList.add('highlight-line');
      cgStartEl.scrollIntoView({ behavior: 'smooth', block: 'center' });
    }
  }
}

function jumpToSource(lineNum, funId) {
  if (!lineNum || lineNum <= 0) return;
  switchTab('source');
  if (currentSourceTabMode === 'cg') setSourceTabMode((cgData.lines && cgData.lines.length > 0) ? 'both' : 'py');

  let f = funId !== undefined ? (DATA.phase2_cfg.functions || []).find(fn => fn.id === funId) : null;
  if (!f) {
    const hit = pyLineToFunPNode.get(lineNum);
    if (hit) f = hit.fun;
  }
  if (f) {
    if (srcFunJumpSelect) srcFunJumpSelect.value = f.id;
    highlightSourceAndCodegenForFunction(f, lineNum);
  } else {
    document.querySelectorAll('#source-code-viewer .code-line.highlight-line').forEach(el => el.classList.remove('highlight-line'));
    const lineEl = document.getElementById('src-line-' + lineNum);
    if (lineEl) {
      lineEl.classList.add('highlight-line');
      lineEl.scrollIntoView({ behavior: 'smooth', block: 'center' });
    }
  }
}

function jumpToCodegen(startLine, endLine, funId) {
  if (!startLine || startLine <= 0) return;
  switchTab('source');
  if (currentSourceTabMode === 'py') setSourceTabMode('both');

  const f = funId !== undefined
    ? (DATA.phase2_cfg.functions || []).find(fn => fn.id === funId)
    : (DATA.phase2_cfg.functions || []).find(fn => fn.cg_start_line === startLine);

  if (f) {
    if (srcFunJumpSelect) srcFunJumpSelect.value = f.id;
    highlightSourceAndCodegenForFunction(f, f.line);
  } else {
    document.querySelectorAll('#codegen-code-viewer .code-line.highlight-line, #codegen-code-viewer .code-line.fun-span-line').forEach(el => {
      el.classList.remove('highlight-line', 'fun-span-line');
    });
    const lastL = (endLine && endLine >= startLine) ? endLine : startLine;
    for (let l = startLine; l <= lastL; l++) {
      const el = document.getElementById('cg-line-' + l);
      if (el) el.classList.add('fun-span-line');
    }
    const startEl = document.getElementById('cg-line-' + startLine);
    if (startEl) {
      startEl.classList.add('highlight-line');
      startEl.scrollIntoView({ behavior: 'smooth', block: 'center' });
    }
  }
}

// Tab: Call Graph (Incremental Cytoscape + ELK Hierarchical Layout)
let cyInstance = null;
let currentCgScope = 'all';
let currentCgDepth = 'all';
let currentCgDirection = 'DOWN';
let expandedNodeIds = new Set();
let collapsedNodeIds = new Set();
let globalFunMap = new Map();
let currentScopedIdSet = new Set();
let currentInspectedFunId = null;

function getSubtreeDescendants(rootId) {
  const descendants = new Set();
  const queue = [rootId];
  while (queue.length > 0) {
    const currId = queue.shift();
    const f = globalFunMap.get(currId);
    if (!f || !f.calls) continue;
    f.calls.forEach(c => {
      const calleeId = c.callee_id;
      if (calleeId !== rootId && !descendants.has(calleeId)) {
        if (!currentScopedIdSet || currentScopedIdSet.has(calleeId)) {
          descendants.add(calleeId);
          queue.push(calleeId);
        }
      }
    });
  }
  return descendants;
}

function showCgNodeInspector(fId) {
  const f = globalFunMap.get(fId);
  if (!f || !cyInstance) return;
  const node = cyInstance.getElementById('fn_' + fId);
  if (!node || node.length === 0) return;

  currentInspectedFunId = fId;

  cyInstance.elements().removeClass('selected-focus caller-focus callee-focus caller-edge callee-edge dimmed');
  cyInstance.elements().addClass('dimmed');

  node.removeClass('dimmed').addClass('selected-focus');

  const inEdges = node.incomers('edge');
  const inNodes = node.incomers('node');
  inEdges.removeClass('dimmed').addClass('caller-edge');
  inNodes.removeClass('dimmed').addClass('caller-focus');

  const outEdges = node.outgoers('edge');
  const outNodes = node.outgoers('node');
  outEdges.removeClass('dimmed').addClass('callee-edge');
  outNodes.removeClass('dimmed').addClass('callee-focus');

  const desc = getSubtreeDescendants(f.id);
  const hiddenDescCount = node.data('hiddenDescCount') || 0;
  const hiddenCount = node.data('hiddenCount') || 0;
  const isSubtreeCollapsed = collapsedNodeIds.has(f.id);
  const visibleDescCount = Math.max(0, desc.size - hiddenDescCount);

  const srcLocHtml = f.line > 0
    ? `<a href="#" onclick="jumpToSource(${f.line}); return false;">${escapeHtml(f.filename || 'unknown')}:${f.line}</a>`
    : `${escapeHtml(f.filename || 'unknown')}`;

  const inspector = document.getElementById('cg-node-inspector');
  inspector.style.display = 'block';
  inspector.innerHTML = `
    <h4>${escapeHtml(f.name)}</h4>
    <div class="prop-row"><span>ID:</span><span class="prop-val">${f.id}${f.is_clone ? ` (clone of #${f.wraps_id})` : ''}</span></div>
    <div class="prop-row"><span>Source:</span><span class="prop-val">${srcLocHtml}</span></div>
    <div class="prop-row"><span>Depth Level:</span><span class="prop-val">${node.data('depth')}</span></div>
    <div class="prop-row"><span>Callers:</span><span class="prop-val">${inNodes.length}</span></div>
    <div class="prop-row"><span>Direct Callees:</span><span class="prop-val">${outNodes.length} visible ${hiddenCount > 0 ? '(' + hiddenCount + ' hidden)' : ''}</span></div>
    ${desc.size > 0 ? `<div class="prop-row"><span>Subtree Total:</span><span class="prop-val">${visibleDescCount} visible, ${hiddenDescCount} hidden</span></div>` : ''}
    <div style="margin-top:0.75rem; display:flex; flex-direction:column; gap:0.4rem;">
      ${hiddenDescCount > 0 || isSubtreeCollapsed ? `<button class="btn-sm" style="background:#059669; color:#fff; font-weight:600;" id="btn-uncollapse-subtree">⊞⊞ Uncollapse Subtree (+${hiddenDescCount})</button>` : ''}
      ${hiddenCount > 0 ? `<button class="btn-sm" style="background:#d97706; color:#fff; font-weight:600;" id="btn-expand-node">⊞ Expand Direct Callees (+${hiddenCount})</button>` : ''}
      ${desc.size > 0 && (visibleDescCount > 0 || outNodes.length > 0) ? `<button class="btn-sm" style="background:var(--bg-tertiary); color:var(--text-main); border:1px solid var(--border);" id="btn-collapse-subtree">⊟ Collapse Subtree</button>` : ''}
      <div style="display:flex; gap:0.4rem;">
        <button class="btn-sm" style="flex:1; background:var(--primary-hover); color:#fff; font-weight:600;" id="btn-jump-cfg">Inspect CFG &rarr;</button>
        <button class="btn-sm" style="background:var(--bg-tertiary); color:var(--text-main);" id="btn-close-inspector">✕</button>
      </div>
    </div>
  `;

  const uncollapseBtn = document.getElementById('btn-uncollapse-subtree');
  if (uncollapseBtn) {
    uncollapseBtn.addEventListener('click', () => {
      uncollapseSubtree(f.id);
    });
  }

  const expandBtn = document.getElementById('btn-expand-node');
  if (expandBtn) {
    expandBtn.addEventListener('click', () => {
      expandNodeCallees(f.id);
    });
  }

  const collapseBtn = document.getElementById('btn-collapse-subtree');
  if (collapseBtn) {
    collapseBtn.addEventListener('click', () => {
      collapseSubtree(f.id);
    });
  }

  document.getElementById('btn-jump-cfg').addEventListener('click', () => {
    jumpToCFG(f.id);
  });
  document.getElementById('btn-close-inspector').addEventListener('click', () => {
    inspector.style.display = 'none';
    currentInspectedFunId = null;
    cyInstance.elements().removeClass('selected-focus caller-focus callee-focus caller-edge callee-edge dimmed');
  });
}

function collapseSubtree(fId) {
  collapsedNodeIds.add(fId);
  const desc = getSubtreeDescendants(fId);
  expandedNodeIds.delete(fId);
  desc.forEach(dId => {
    expandedNodeIds.delete(dId);
    collapsedNodeIds.delete(dId);
  });
  rebuildCallGraph();
  showCgNodeInspector(fId);
}

function uncollapseSubtree(fId) {
  collapsedNodeIds.delete(fId);
  const desc = getSubtreeDescendants(fId);
  expandedNodeIds.add(fId);
  desc.forEach(dId => {
    collapsedNodeIds.delete(dId);
    expandedNodeIds.add(dId);
  });
  rebuildCallGraph();
  showCgNodeInspector(fId);
}

function expandNodeCallees(fId) {
  collapsedNodeIds.delete(fId);
  expandedNodeIds.add(fId);
  rebuildCallGraph();
  showCgNodeInspector(fId);
}

let cgScriptsLoading = false;
let cgScriptsLoaded = false;
let cgScriptCallbacks = [];

function loadScript(src, onSuccess, onError) {
  const s = document.createElement('script');
  s.src = src;
  s.onload = onSuccess;
  s.onerror = onError;
  document.head.appendChild(s);
}

function loadCallGraphScripts(callback) {
  if (typeof cytoscape !== 'undefined') {
    cgScriptsLoaded = true;
    if (callback) callback(true);
    return;
  }
  if (callback) cgScriptCallbacks.push(callback);
  if (cgScriptsLoading) return;
  cgScriptsLoading = true;

  const flushCallbacks = (ok) => {
    cgScriptsLoading = false;
    cgScriptsLoaded = ok;
    const cbs = cgScriptCallbacks.slice();
    cgScriptCallbacks = [];
    cbs.forEach(cb => { try { cb(ok); } catch (e) { console.error(e); } });
  };

  const overlay = document.getElementById('cg-loading-overlay');
  const statusEl = document.getElementById('cg-loading-status');
  if (overlay) overlay.style.display = 'flex';
  if (statusEl) statusEl.textContent = 'Loading Cytoscape engine...';

  loadScript('https://cdn.jsdelivr.net/npm/cytoscape@3.30.2/dist/cytoscape.min.js', () => {
    if (statusEl) statusEl.textContent = 'Loading ELK layout engine...';
    loadScript('https://cdn.jsdelivr.net/npm/elkjs@0.9.3/lib/elk.bundled.js', () => {
      if (statusEl) statusEl.textContent = 'Loading Cytoscape-ELK adapter...';
      loadScript('https://cdn.jsdelivr.net/npm/cytoscape-elk@2.2.0/dist/cytoscape-elk.min.js', () => {
        if (overlay) overlay.style.display = 'none';
        flushCallbacks(true);
      }, () => {
        if (overlay) overlay.style.display = 'none';
        flushCallbacks(true);
      });
    }, () => {
      if (overlay) overlay.style.display = 'none';
      flushCallbacks(true);
    });
  }, () => {
    if (overlay) overlay.style.display = 'none';
    const fallback = document.getElementById('cg-offline-fallback');
    if (fallback) fallback.style.display = 'block';
    flushCallbacks(false);
  });
}

function getCallGraphData() {
  const allFuns = DATA.phase2_cfg.functions || [];
  globalFunMap.clear();
  allFuns.forEach(f => globalFunMap.set(f.id, f));

  // 1. Filter by Scope
  let scopedFuns = allFuns;
  if (currentCgScope === 'active') {
    scopedFuns = allFuns.filter(f => (f.calls && f.calls.length > 0) || (f.called_by && f.called_by.length > 0));
    if (scopedFuns.length === 0) scopedFuns = allFuns;
  } else if (currentCgScope === 'user') {
    scopedFuns = allFuns.filter(f => {
      const fn = f.filename || '';
      return fn !== '' && !fn.startsWith('__pyc__') && !fn.startsWith('<builtin');
    });
    if (scopedFuns.length === 0) scopedFuns = allFuns;
  }
  const scopedIdSet = new Set(scopedFuns.map(f => f.id));
  currentScopedIdSet = scopedIdSet;

  // 2. Discover Root Functions (Entry Points)
  let roots = [];
  scopedFuns.forEach(f => {
    const inScopedCallers = (f.called_by || []).filter(c => scopedIdSet.has(c.caller_id));
    if (inScopedCallers.length === 0) {
      roots.push(f.id);
    }
  });

  if (roots.length === 0 && scopedFuns.length > 0) {
    const mainFn = scopedFuns.find(f => f.name === '__main__' || f.name === 'main');
    if (mainFn) roots.push(mainFn.id);
    else roots.push(scopedFuns[0].id);
  }

  // 3. BFS Depth Calculation from Roots
  const depths = new Map();
  const queue = [];
  roots.forEach(rId => {
    depths.set(rId, 0);
    queue.push(rId);
  });

  while (queue.length > 0) {
    const currId = queue.shift();
    const currD = depths.get(currId);
    const f = globalFunMap.get(currId);
    if (!f || !f.calls) continue;
    f.calls.forEach(c => {
      if (scopedIdSet.has(c.callee_id) && !depths.has(c.callee_id)) {
        depths.set(c.callee_id, currD + 1);
        queue.push(c.callee_id);
      }
    });
  }

  scopedFuns.forEach(f => {
    if (!depths.has(f.id)) {
      depths.set(f.id, 0);
      roots.push(f.id);
    }
  });

  // 4. Determine Visible Functions by Graph Traversal from Roots
  const maxDepth = currentCgDepth === 'all' ? 9999 : parseInt(currentCgDepth, 10);
  const visibleIds = new Set();
  const visQueue = [];

  roots.forEach(rId => {
    if (scopedIdSet.has(rId)) {
      visibleIds.add(rId);
      visQueue.push(rId);
    }
  });

  if (visibleIds.size === 0 && scopedFuns.length > 0) {
    visibleIds.add(scopedFuns[0].id);
    visQueue.push(scopedFuns[0].id);
  }

  // Also include explicitly expanded nodes
  expandedNodeIds.forEach(id => {
    if (scopedIdSet.has(id) && !visibleIds.has(id)) {
      visibleIds.add(id);
      visQueue.push(id);
    }
  });

  while (visQueue.length > 0) {
    const currId = visQueue.shift();
    // Collapsed nodes stay visible themselves, but do NOT propagate visibility to callees
    if (collapsedNodeIds.has(currId)) {
      continue;
    }

    const f = globalFunMap.get(currId);
    if (!f || !f.calls) continue;

    const currDepth = depths.get(currId) ?? 0;
    const isExplicitlyExpanded = expandedNodeIds.has(currId);

    f.calls.forEach(c => {
      const calleeId = c.callee_id;
      if (!scopedIdSet.has(calleeId)) return;

      const calleeDepth = depths.get(calleeId) ?? (currDepth + 1);
      const shouldBeVisible = isExplicitlyExpanded || (calleeDepth <= maxDepth);

      if (shouldBeVisible) {
        if (!visibleIds.has(calleeId)) {
          visibleIds.add(calleeId);
          visQueue.push(calleeId);
        }
      }
    });
  }

  // 5. Build Cytoscape Nodes and Edges
  const elements = [];
  visibleIds.forEach(fId => {
    const f = globalFunMap.get(fId);
    if (!f) return;

    const isUser = f.filename && !f.filename.startsWith('__pyc__') && !f.filename.startsWith('<builtin');
    const isEntry = roots.includes(f.id);

    const totalCallees = (f.calls || []).filter(c => scopedIdSet.has(c.callee_id));
    const hiddenCallees = totalCallees.filter(c => !visibleIds.has(c.callee_id)).length;
    const isSubtreeCollapsed = collapsedNodeIds.has(f.id);
    const desc = getSubtreeDescendants(f.id);
    const hiddenDescCount = Array.from(desc).filter(id => !visibleIds.has(id)).length;

    const classes = [];
    if (isEntry) classes.push('entry-node');
    else if (isUser) classes.push('user-node');
    else classes.push('runtime-node');

    if (f.can_raise) classes.push('can-raise');
    if (!f.live) classes.push('dead-node');
    if (hiddenCallees > 0 || isSubtreeCollapsed) classes.push('has-hidden');
    if (isSubtreeCollapsed) classes.push('collapsed-subtree');

    let baseLabel = f.is_clone ? `${f.name || '*anon*'} #${f.id}` : (f.name || '*anon*');
    let displayName = baseLabel;
    if (isSubtreeCollapsed && hiddenDescCount > 0) {
      displayName = `${baseLabel} [⊟ ${hiddenDescCount}]`;
    } else if (hiddenDescCount > 0) {
      displayName = `${baseLabel} (+${hiddenDescCount})`;
    }

    elements.push({
      group: 'nodes',
      data: {
        id: 'fn_' + f.id,
        funId: f.id,
        name: f.name || '*anon*',
        displayName: displayName,
        filename: f.filename || 'unknown',
        line: f.line || 0,
        can_raise: f.can_raise,
        live: f.live,
        depth: depths.get(f.id) ?? 0,
        callCount: totalCallees.length,
        hiddenCount: hiddenCallees,
        hiddenDescCount: hiddenDescCount,
        isSubtreeCollapsed: isSubtreeCollapsed,
        calledByCount: f.called_by ? f.called_by.length : 0
      },
      classes: classes.join(' ')
    });
  });

  const edgeSet = new Set();
  visibleIds.forEach(fId => {
    const f = globalFunMap.get(fId);
    if (!f || !f.calls) return;
    f.calls.forEach(c => {
      if (visibleIds.has(c.callee_id)) {
        const edgeId = `e_${f.id}_${c.callee_id}`;
        if (!edgeSet.has(edgeId)) {
          edgeSet.add(edgeId);
          elements.push({
            group: 'edges',
            data: {
              id: edgeId,
              source: 'fn_' + f.id,
              target: 'fn_' + c.callee_id,
              pnode_id: c.pnode_id
            }
          });
        }
      }
    });
  });

  return { elements, count: visibleIds.size, total: scopedFuns.length };
}

function initCallGraph() {
  const container = document.getElementById('cy-callgraph');
  if (!container) return;

  if (typeof cytoscape === 'undefined') {
    loadCallGraphScripts(ok => { if (ok) initCallGraph(); });
    return;
  }

  if (typeof cytoscapeElk !== 'undefined') {
    try { cytoscape.use(cytoscapeElk); } catch (e) {}
  }

  const { elements, count, total } = getCallGraphData();
  document.getElementById('cg-node-count').textContent = `${count} of ${total} functions`;

  const cyStyles = [
    {
      selector: 'node',
      style: {
        'label': 'data(displayName)',
        'font-family': 'ui-monospace, SFMono-Regular, Menlo, Monaco, Consolas, monospace',
        'font-size': '11px',
        'color': '#f8fafc',
        'text-valign': 'center',
        'text-halign': 'center',
        'background-color': '#1e293b',
        'border-width': 2,
        'border-color': '#475569',
        'shape': 'round-rectangle',
        'padding': '8px',
        'width': 'label',
        'height': 'label',
        'text-max-width': '220px',
        'text-wrap': 'ellipsis',
        'transition-property': 'background-color, border-color, opacity, border-width',
        'transition-duration': '0.15s'
      }
    },
    {
      selector: 'node.user-node',
      style: {
        'background-color': '#0369a1',
        'border-color': '#38bdf8',
        'color': '#ffffff'
      }
    },
    {
      selector: 'node.entry-node',
      style: {
        'background-color': '#065f46',
        'border-color': '#34d399',
        'border-width': 3,
        'color': '#ecfdf5'
      }
    },
    {
      selector: 'node.runtime-node',
      style: {
        'background-color': '#1e293b',
        'border-color': '#64748b',
        'color': '#cbd5e1'
      }
    },
    {
      selector: 'node.can-raise',
      style: {
        'border-color': '#f97316'
      }
    },
    {
      selector: 'node.dead-node',
      style: {
        'opacity': 0.4,
        'border-style': 'dashed'
      }
    },
    {
      selector: 'node.has-hidden',
      style: {
        'border-color': '#f59e0b',
        'border-width': 2.5
      }
    },
    {
      selector: 'node.collapsed-subtree',
      style: {
        'border-color': '#f59e0b',
        'border-width': 3,
        'border-style': 'dashed'
      }
    },
    {
      selector: 'edge',
      style: {
        'width': 1.5,
        'line-color': '#475569',
        'target-arrow-color': '#475569',
        'target-arrow-shape': 'triangle',
        'arrow-scale': 1.0,
        'curve-style': 'bezier',
        'opacity': 0.7,
        'transition-property': 'line-color, target-arrow-color, width, opacity',
        'transition-duration': '0.15s'
      }
    },
    {
      selector: 'node.selected-focus',
      style: {
        'border-color': '#facc15',
        'border-width': 4,
        'background-color': '#ca8a04',
        'color': '#ffffff',
        'opacity': 1.0,
        'z-index': 999
      }
    },
    {
      selector: 'node.caller-focus',
      style: {
        'border-color': '#4ade80',
        'border-width': 3,
        'color': '#ffffff',
        'opacity': 1.0,
        'z-index': 990
      }
    },
    {
      selector: 'node.callee-focus',
      style: {
        'border-color': '#38bdf8',
        'border-width': 3,
        'color': '#ffffff',
        'opacity': 1.0,
        'z-index': 990
      }
    },
    {
      selector: 'edge.caller-edge',
      style: {
        'line-color': '#4ade80',
        'target-arrow-color': '#4ade80',
        'width': 3,
        'opacity': 1.0,
        'z-index': 995
      }
    },
    {
      selector: 'edge.callee-edge',
      style: {
        'line-color': '#38bdf8',
        'target-arrow-color': '#38bdf8',
        'width': 3,
        'opacity': 1.0,
        'z-index': 995
      }
    },
    {
      selector: '.dimmed',
      style: {
        'opacity': 0.12
      }
    }
  ];

  if (cyInstance) {
    cyInstance.destroy();
  }

  cyInstance = cytoscape({
    container: container,
    elements: elements,
    style: cyStyles,
    boxSelectionEnabled: false,
    wheelSensitivity: 0.25,
    minZoom: 0.1,
    maxZoom: 4.0
  });

  applyCallGraphLayout(currentCgDirection);

  // Interaction handlers
  cyInstance.on('tap', 'node', function(evt) {
    const fId = evt.target.data('funId');
    showCgNodeInspector(fId);
  });

  cyInstance.on('dbltap', 'node', function(evt) {
    const fId = evt.target.data('funId');
    const desc = getSubtreeDescendants(fId);
    const node = evt.target;
    const hiddenDescCount = node.data('hiddenDescCount') || 0;
    if (hiddenDescCount > 0 || collapsedNodeIds.has(fId)) {
      uncollapseSubtree(fId);
    } else if (desc.size > 0) {
      collapseSubtree(fId);
    } else {
      jumpToCFG(fId);
    }
  });

  cyInstance.on('cxttap', 'node', function(evt) {
    evt.preventDefault();
    const fId = evt.target.data('funId');
    const desc = getSubtreeDescendants(fId);
    const node = evt.target;
    const hiddenDescCount = node.data('hiddenDescCount') || 0;
    if (hiddenDescCount > 0 || collapsedNodeIds.has(fId)) {
      uncollapseSubtree(fId);
    } else if (desc.size > 0) {
      collapseSubtree(fId);
    }
  });

  cyInstance.on('tap', function(evt) {
    if (evt.target === cyInstance) {
      cyInstance.elements().removeClass('selected-focus caller-focus callee-focus caller-edge callee-edge dimmed');
      document.getElementById('cg-node-inspector').style.display = 'none';
      currentInspectedFunId = null;
    }
  });
}

function rebuildCallGraph() {
  if (!cyInstance) {
    initCallGraph();
    return;
  }
  const { elements, count, total } = getCallGraphData();
  document.getElementById('cg-node-count').textContent = `${count} of ${total} functions`;

  cyInstance.elements().remove();
  cyInstance.add(elements);
  applyCallGraphLayout(currentCgDirection);
}

function applyCallGraphLayout(direction) {
  if (!cyInstance) return;
  const dir = direction || 'DOWN';

  let layoutOptions = {
    name: 'elk',
    fit: true,
    padding: 35,
    animate: false,
    elk: {
      'algorithm': 'layered',
      'org.eclipse.elk.direction': dir,
      'org.eclipse.elk.layered.spacing.nodeNodeBetweenLayers': '50',
      'org.eclipse.elk.spacing.nodeNode': '30',
      'org.eclipse.elk.edgeRouting': 'POLYLINE'
    }
  };

  try {
    const l = cyInstance.layout(layoutOptions);
    l.run();
  } catch (err) {
    console.warn('ELK layout fallback to breadthfirst:', err);
    cyInstance.layout({
      name: 'breadthfirst',
      directed: true,
      spacingFactor: 1.5,
      padding: 35
    }).run();
  }

  if (currentInspectedFunId !== null) {
    setTimeout(() => {
      showCgNodeInspector(currentInspectedFunId);
    }, 50);
  }
}

function renderCallGraph() {
  if (typeof cytoscape === 'undefined') {
    loadCallGraphScripts((ok) => {
      if (ok) initCallGraph();
    });
  } else if (!cyInstance) {
    initCallGraph();
  } else {
    setTimeout(() => {
      cyInstance.resize();
      cyInstance.fit(undefined, 35);
    }, 20);
  }
}

// Scope dropdown handler
document.getElementById('cg-scope-select').addEventListener('change', (e) => {
  currentCgScope = e.target.value;
  expandedNodeIds.clear();
  collapsedNodeIds.clear();
  rebuildCallGraph();
});

// Depth dropdown handler
document.getElementById('cg-depth-select').addEventListener('change', (e) => {
  currentCgDepth = e.target.value;
  expandedNodeIds.clear();
  collapsedNodeIds.clear();
  rebuildCallGraph();
});

// Direction dropdown handler
document.getElementById('cg-direction-select').addEventListener('change', (e) => {
  currentCgDirection = e.target.value;
  applyCallGraphLayout(currentCgDirection);
});

// Expand All button
document.getElementById('cg-btn-expand-all').addEventListener('click', () => {
  currentCgDepth = 'all';
  document.getElementById('cg-depth-select').value = 'all';
  collapsedNodeIds.clear();
  rebuildCallGraph();
});

// Collapse to Root button
document.getElementById('cg-btn-collapse-root').addEventListener('click', () => {
  currentCgDepth = '1';
  document.getElementById('cg-depth-select').value = '1';
  expandedNodeIds.clear();
  collapsedNodeIds.clear();
  rebuildCallGraph();
});

// Search input handler
const searchInput = document.getElementById('cg-search');
searchInput.addEventListener('input', () => {
  if (!cyInstance) return;
  const q = searchInput.value.trim().toLowerCase();
  if (!q) {
    cyInstance.elements().removeClass('selected-focus caller-focus callee-focus caller-edge callee-edge dimmed');
    return;
  }
  cyInstance.elements().addClass('dimmed');
  const matches = cyInstance.nodes().filter(n => n.data('name').toLowerCase().includes(q));
  matches.removeClass('dimmed').addClass('selected-focus');
  if (matches.length === 1) {
    cyInstance.animate({
      center: { eles: matches },
      zoom: Math.max(cyInstance.zoom(), 1.0),
      duration: 300
    });
  }
});

// Zoom and reset buttons
document.getElementById('cg-btn-zoom-in').addEventListener('click', () => {
  if (cyInstance) cyInstance.zoom({ level: cyInstance.zoom() * 1.3, renderedPosition: { x: cyInstance.width() / 2, y: cyInstance.height() / 2 } });
});
document.getElementById('cg-btn-zoom-out').addEventListener('click', () => {
  if (cyInstance) cyInstance.zoom({ level: cyInstance.zoom() * 0.75, renderedPosition: { x: cyInstance.width() / 2, y: cyInstance.height() / 2 } });
});
document.getElementById('cg-btn-fit').addEventListener('click', () => {
  if (cyInstance) cyInstance.fit(undefined, 35);
});
document.getElementById('cg-btn-reset').addEventListener('click', () => {
  if (cyInstance) {
    searchInput.value = '';
    cyInstance.elements().removeClass('selected-focus caller-focus callee-focus caller-edge callee-edge dimmed');
    document.getElementById('cg-node-inspector').style.display = 'none';
    cyInstance.fit(undefined, 35);
  }
});

// Tab: CFG & SSU (Hierarchical Cytoscape + ELK Visual Graph & Instruction Table)
let cyCfgInstance = null;
let currentCfgFunId = DATA.phase2_cfg.functions.length > 0 ? DATA.phase2_cfg.functions[0].id : null;
let currentCfgPass = 'final';
let currentCfgContour = 'all';
let currentCfgMode = 'graph';

const cfgSelect = document.getElementById('cfg-fun-select');
DATA.phase2_cfg.functions.forEach(f => {
  const opt = document.createElement('option');
  opt.value = f.id;
  opt.textContent = f.is_clone ? `${f.name} (id: ${f.id}, clone of #${f.wraps_id})` : `${f.name} (id: ${f.id})`;
  cfgSelect.appendChild(opt);
});

const cfgPassSelect = document.getElementById('cfg-pass-select');
if (cfgPassSelect) {
  const finalOpt = document.createElement('option');
  finalOpt.value = 'final';
  finalOpt.textContent = `Final (Pass ${DATA.meta.total_passes} Quiescence)`;
  cfgPassSelect.appendChild(finalOpt);
  for (let pi = 1; pi <= DATA.meta.total_passes; pi++) {
    const opt = document.createElement('option');
    opt.value = pi;
    opt.textContent = `Pass ${pi}`;
    cfgPassSelect.appendChild(opt);
  }
}

const cfgContourSelect = document.getElementById('cfg-contour-select');

cfgSelect.addEventListener('change', () => {
  currentCfgFunId = parseInt(cfgSelect.value);
  updateCfgContours();
  renderCFGCurrent();
});

if (cfgPassSelect) {
  cfgPassSelect.addEventListener('change', () => {
    currentCfgPass = cfgPassSelect.value;
    updateCfgContours();
    renderCFGCurrent();
  });
}

if (cfgContourSelect) {
  cfgContourSelect.addEventListener('change', () => {
    currentCfgContour = cfgContourSelect.value;
    renderCFGCurrent();
  });
}

document.getElementById('btn-cfg-mode-graph').addEventListener('click', () => {
  currentCfgMode = 'graph';
  document.getElementById('btn-cfg-mode-graph').classList.add('active');
  document.getElementById('btn-cfg-mode-table').classList.remove('active');
  document.getElementById('cfg-graph-view').style.display = 'block';
  document.getElementById('cfg-table-view').style.display = 'none';
  document.getElementById('cfg-graph-controls').style.display = 'flex';
  renderCFGCurrent();
});

document.getElementById('btn-cfg-mode-table').addEventListener('click', () => {
  currentCfgMode = 'table';
  document.getElementById('btn-cfg-mode-table').classList.add('active');
  document.getElementById('btn-cfg-mode-graph').classList.remove('active');
  document.getElementById('cfg-graph-view').style.display = 'none';
  document.getElementById('cfg-table-view').style.display = 'block';
  document.getElementById('cfg-graph-controls').style.display = 'none';
  renderCFGCurrent();
});

document.getElementById('cfg-btn-zoom-in').addEventListener('click', () => {
  if (cyCfgInstance) cyCfgInstance.zoom({ level: cyCfgInstance.zoom() * 1.3, renderedPosition: { x: cyCfgInstance.width() / 2, y: cyCfgInstance.height() / 2 } });
});
document.getElementById('cfg-btn-zoom-out').addEventListener('click', () => {
  if (cyCfgInstance) cyCfgInstance.zoom({ level: cyCfgInstance.zoom() * 0.75, renderedPosition: { x: cyCfgInstance.width() / 2, y: cyCfgInstance.height() / 2 } });
});
document.getElementById('cfg-btn-fit').addEventListener('click', () => {
  if (cyCfgInstance) cyCfgInstance.fit(undefined, 35);
});
document.getElementById('cfg-btn-reset').addEventListener('click', () => {
  if (cyCfgInstance) {
    cyCfgInstance.elements().removeClass('pnode-selected pnode-neighbor edge-highlight');
    document.getElementById('cfg-inspector').style.display = 'none';
    cyCfgInstance.fit(undefined, 35);
  }
});

function updateCfgContours() {
  const sel = document.getElementById('cfg-contour-select');
  if (!sel) return;
  const oldVal = sel.value;
  sel.innerHTML = '<option value="all">All Contours (Unified)</option>';

  const f = DATA.phase2_cfg.functions.find(fn => fn.id === currentCfgFunId);
  const pNum = currentCfgPass === 'final' ? totalPasses : parseInt(currentCfgPass);
  const passData = getPassData(pNum);
  const seenEs = new Set();
  if (passData && passData.entry_sets) {
    passData.entry_sets.filter(es =>
      es.fun_id === currentCfgFunId ||
      (f && f.is_clone && es.orig_fun_id === f.wraps_id && (!f.entry_sets || f.entry_sets.length === 0 || f.entry_sets.includes(es.id)))
    ).forEach(es => {
      seenEs.add(es.id);
      const opt = document.createElement('option');
      opt.value = es.id;
      opt.textContent = `EntrySet #${es.id}`;
      sel.appendChild(opt);
    });
  }
  if (seenEs.size === 0 && f && f.entry_sets) {
    f.entry_sets.forEach(esId => {
      const opt = document.createElement('option');
      opt.value = esId;
      opt.textContent = `EntrySet #${esId}`;
      sel.appendChild(opt);
    });
  }
  if (oldVal && sel.querySelector(`option[value="${oldVal}"]`)) {
    sel.value = oldVal;
  } else {
    sel.value = 'all';
  }
  currentCfgContour = sel.value;
}

function getTypeForVar(f, vId, passChoice, contourChoice) {
  if (!f) return 'void';
  ensureFunIndexed(f);
  const v = f._varById.get(vId);
  const pNum = passChoice === 'final' ? totalPasses : parseInt(passChoice);
  const passData = getPassData(pNum);

  if (passData && passData._avarById) {
    let matchingAvars = [];
    if (v && v.avars && v.avars.length > 0) {
      v.avars.forEach(a => {
        const av = passData._avarById.get(a.av_id);
        if (av) matchingAvars.push(av);
      });
    }
    if (matchingAvars.length === 0 && passData._avarsByVarId) {
      const byVar = passData._avarsByVarId.get(vId) || [];
      matchingAvars = byVar.filter(av =>
        av.fun_id === f.id || av.fun_id === -1 || (f.is_clone && av.orig_fun_id === f.wraps_id)
      );
    }
    if (matchingAvars.length > 0) {
      if (contourChoice !== 'all') {
        const cId = parseInt(contourChoice);
        const specificAv = matchingAvars.find(av => av.contour_id === cId);
        if (specificAv) {
          const typeNames = (specificAv.type || []).map(t => t.sym + (t.is_const ? `=${t.val}` : ''));
          return typeNames.length > 0 ? Array.from(new Set(typeNames)).join(' | ') : (specificAv.type_str || 'void');
        }
      } else {
        const allTypes = new Set();
        matchingAvars.forEach(av => {
          (av.type || []).forEach(t => allTypes.add(t.sym + (t.is_const ? `=${t.val}` : '')));
        });
        if (allTypes.size > 0) return Array.from(allTypes).join(' | ');
      }
    }
  }

  if (v) {
    if (contourChoice !== 'all') {
      const cId = parseInt(contourChoice);
      const av = (v.avars || []).find(a => a.contour_id === cId);
      if (av && av.type_str) return av.type_str;
    }
    return v.type_str || 'void';
  }
  return 'void';
}

let currentCfgSplitMode = 'off';
let currentSelectedPNodeId = null;

function setCfgSplitMode(mode) {
  currentCfgSplitMode = mode;
  ['off', 'py', 'cg', 'both'].forEach(m => {
    const btn = document.getElementById('btn-cfg-split-' + m);
    if (btn) btn.classList.toggle('active', m === mode);
  });
  const dock = document.getElementById('cfg-split-dock');
  const pyPane = document.getElementById('cfg-dock-py-pane');
  const cgPane = document.getElementById('cfg-dock-cg-pane');
  if (dock) dock.classList.toggle('active', mode !== 'off');
  if (pyPane) pyPane.style.display = (mode === 'py' || mode === 'both') ? 'flex' : 'none';
  if (cgPane) cgPane.style.display = (mode === 'cg' || mode === 'both') ? 'flex' : 'none';

  if (mode !== 'off' && currentCfgFunId !== null) {
    syncCfgSplitDock(currentCfgFunId, currentSelectedPNodeId);
  }
  if (cyCfgInstance) {
    setTimeout(() => {
      cyCfgInstance.resize();
      cyCfgInstance.fit(undefined, 35);
    }, 30);
  }
}

function openCurrentFunInSourceTab(which) {
  const f = (DATA.phase2_cfg.functions || []).find(fn => fn.id === currentCfgFunId);
  if (!f) {
    switchTab('source');
    return;
  }
  if (which === 'cg' && f.cg_start_line > 0) {
    jumpToCodegen(f.cg_start_line, f.cg_end_line, f.id);
  } else if (f.line > 0) {
    jumpToSource(f.line, f.id);
  } else {
    switchTab('source');
  }
}

function syncCfgSplitDock(funId, selectedPNodeId) {
  if (currentCfgSplitMode === 'off') return;
  const f = (DATA.phase2_cfg.functions || []).find(fn => fn.id === funId);
  if (!f) return;
  const selectedPn = selectedPNodeId !== null && selectedPNodeId !== undefined
    ? (f.pnodes || []).find(p => p.id === selectedPNodeId)
    : null;

  // 1. Sync Python Source Dock Pane
  const pyBody = document.getElementById('cfg-dock-py-body');
  const pyTitle = document.getElementById('cfg-dock-py-title');
  if (pyBody && (currentCfgSplitMode === 'py' || currentCfgSplitMode === 'both')) {
    const pyLines = (DATA.phase1_source && DATA.phase1_source.lines) || [];
    const span = getFunctionPythonSpan(f);
    if (pyTitle) {
      pyTitle.textContent = span.start > 0
        ? `📜 Python: ${f.name} (L${span.start}–${span.end})`
        : `📜 Python: ${f.name}`;
    }
    pyBody.innerHTML = '';
    if (pyLines.length === 0 || span.start <= 0) {
      pyBody.innerHTML = `<div style="padding:1rem; color:var(--text-muted);">No user source lines mapped to ${escapeHtml(f.name)} (builtin / runtime helper).</div>`;
    } else {
      const winStart = Math.max(1, span.start - 5);
      const winEnd = Math.min(pyLines.length, Math.max(span.end + 8, span.start + 15));
      const lineToPNode = new Map();
      (f.pnodes || []).forEach(pn => {
        if (pn.line > 0 && !lineToPNode.has(pn.line)) lineToPNode.set(pn.line, pn);
      });
      const frag = document.createDocumentFragment();
      let scrollTarget = null;
      for (let l = winStart; l <= winEnd; l++) {
        const row = document.createElement('div');
        const inSpan = l >= span.start && l <= span.end;
        const isSelectedLine = selectedPn && selectedPn.line === l;
        const pnAtLine = lineToPNode.get(l);
        let cls = 'code-line';
        if (inSpan) cls += ' fun-span-line';
        if (isSelectedLine) cls += ' pnode-match-line';
        if (pnAtLine) cls += ' clickable-line';
        row.className = cls;
        if (pnAtLine) {
          row.title = `Click to select PNode #${pnAtLine.id} (${pnAtLine.kind})`;
          row.addEventListener('click', () => {
            showPNodeInspector(f.id, pnAtLine.id);
          });
        }
        row.innerHTML = `<span class="line-num">${l}</span><span class="line-text">${escapeHtml(pyLines[l - 1] || '')}</span>`;
        frag.appendChild(row);
        if (isSelectedLine || (!scrollTarget && l === span.start)) scrollTarget = row;
      }
      pyBody.appendChild(frag);
      if (scrollTarget) {
        setTimeout(() => scrollTarget.scrollIntoView({ behavior: 'smooth', block: 'center' }), 10);
      }
    }
  }

  // 2. Sync Generated C / LLVM Dock Pane
  const cgBody = document.getElementById('cfg-dock-cg-body');
  const cgTitle = document.getElementById('cfg-dock-cg-title');
  if (cgBody && (currentCfgSplitMode === 'cg' || currentCfgSplitMode === 'both')) {
    const cgLines = (cgData && cgData.lines) || [];
    const lang = (cgData && cgData.language) || DATA.meta.backend || 'C';
    if (cgTitle) {
      cgTitle.textContent = (f.cg_start_line > 0)
        ? `⚙️ ${lang}: ${f.cg_name} (L${f.cg_start_line}–${f.cg_end_line})`
        : `⚙️ ${lang}: ${f.cg_name || f.name}`;
    }
    cgBody.innerHTML = '';
    if (cgLines.length === 0 || !f.cg_start_line || f.cg_start_line <= 0) {
      cgBody.innerHTML = `<div style="padding:1rem; color:var(--text-muted);">No standalone ${escapeHtml(lang)} function emitted for <code>${escapeHtml(f.cg_name || f.name)}</code> (inlined or elided).</div>`;
    } else {
      const winStart = Math.max(1, f.cg_start_line - 1);
      const winEnd = Math.min(cgLines.length, f.cg_end_line + 1);

      // Compute tokens to highlight for selectedPn inside the emitted C/LLVM body
      const matchPatterns = [];
      if (selectedPn) {
        if (selectedPn.label_id > 0) {
          matchPatterns.push(`L${selectedPn.label_id}:`);
          matchPatterns.push(`goto L${selectedPn.label_id};`);
          matchPatterns.push(`label %L${selectedPn.label_id}`);
        }
        if (selectedPn.label_else_id > 0) {
          matchPatterns.push(`L${selectedPn.label_else_id}:`);
          matchPatterns.push(`goto L${selectedPn.label_else_id};`);
          matchPatterns.push(`label %L${selectedPn.label_else_id}`);
        }
        (selectedPn.contour_calls || []).forEach(cc => {
          const calleeFun = (DATA.phase2_cfg.functions || []).find(fn => fn.id === cc.callee_id || fn.wraps_id === cc.callee_id);
          if (calleeFun && calleeFun.cg_name) {
            matchPatterns.push(calleeFun.cg_name + '(');
          }
        });
        if (selectedPn.prim === 'prim_reply') {
          matchPatterns.push('return ');
          matchPatterns.push('return;');
          matchPatterns.push('co_return ');
          matchPatterns.push('ret ');
        }
      }

      const frag = document.createDocumentFragment();
      let scrollTarget = null;
      for (let l = winStart; l <= winEnd; l++) {
        const lineStr = cgLines[l - 1] || '';
        const inSpan = l >= f.cg_start_line && l <= f.cg_end_line;
        const isMatch = inSpan && matchPatterns.length > 0 && matchPatterns.some(pat => lineStr.includes(pat));
        let cls = 'code-line';
        if (inSpan) cls += ' fun-span-line';
        if (isMatch) cls += ' pnode-match-line';
        const row = document.createElement('div');
        row.className = cls;
        row.innerHTML = `<span class="line-num">${l}</span><span class="line-text">${escapeHtml(lineStr)}</span>`;
        frag.appendChild(row);
        if (isMatch && !scrollTarget) scrollTarget = row;
      }
      cgBody.appendChild(frag);
      if (scrollTarget) {
        setTimeout(() => scrollTarget.scrollIntoView({ behavior: 'smooth', block: 'center' }), 10);
      }
    }
  }
}

function renderCFGCurrent() {
  const pNum = currentCfgPass === 'final' ? totalPasses : parseInt(currentCfgPass);
  loadPassData(pNum, () => {
    updateCfgContours();
    renderCFGForFunction(currentCfgFunId);
  });
}

function renderCFGForFunction(funId) {
  const f = DATA.phase2_cfg.functions.find(fn => fn.id === funId);
  if (!f) return;
  ensureFunIndexed(f);
  if (currentCfgFunId !== funId) currentSelectedPNodeId = null;
  currentCfgFunId = funId;
  const cfgSelect = document.getElementById('cfg-fun-select');
  if (cfgSelect && cfgSelect.value != funId) cfgSelect.value = funId;

  const locHtml = f.line > 0
    ? `<a href="#" onclick="jumpToSource(${f.line}, ${f.id}); return false;">${escapeHtml(f.filename || 'unknown')}:${f.line}</a>`
    : escapeHtml(f.filename || 'unknown');
  const cgHtml = (f.cg_start_line && f.cg_start_line > 0)
    ? ` | Emit: <a href="#" onclick="jumpToCodegen(${f.cg_start_line}, ${f.cg_end_line}, ${f.id}); return false;" style="color:var(--primary);">${escapeHtml(f.cg_name)} (L${f.cg_start_line}–${f.cg_end_line})</a>`
    : '';
  document.getElementById('cfg-fun-meta').innerHTML = `File: ${locHtml}${cgHtml} | Can Raise: ${f.can_raise} | Live: ${f.live}`;

  syncCfgSplitDock(f.id, currentSelectedPNodeId);

  // 1. Render Table View
  const tbody = document.querySelector('#cfg-pnodes-table tbody');
  if (tbody) {
    tbody.innerHTML = '';
    f.pnodes.forEach(pn => {
      const tr = document.createElement('tr');
      const lvalsStr = (pn.lvals || []).map(v => {
        const t = getTypeForVar(f, v.id, currentCfgPass, currentCfgContour);
        return `<span class="badge badge-var" onclick="showVarInspector(${f.id}, ${v.id})">${escapeHtml(v.name || 'v'+v.id)}</span> <span class="badge badge-type">${escapeHtml(t)}</span>`;
      }).join(' ');

      const rvalsStr = (pn.rvals || []).map(v => {
        const t = getTypeForVar(f, v.id, currentCfgPass, currentCfgContour);
        return `<span class="badge badge-var" onclick="showVarInspector(${f.id}, ${v.id})">${escapeHtml(v.name || 'v'+v.id)}</span> <span class="badge badge-type">${escapeHtml(t)}</span>`;
      }).join(' ');

      const callsStr = (pn.contour_calls || []).map(c => {
        return `<button class="btn-sm" style="font-size:0.75rem; padding:0.15rem 0.35rem; margin:0.1rem;" onclick="jumpToCFG(${c.callee_id}, ${c.to_es}, currentCfgPass === 'final' ? totalPasses : parseInt(currentCfgPass))">${escapeHtml(c.callee_name)} (ES#${c.to_es})</button>`;
      }).join(' ');

      let statusBadge = '';
      if (!pn.live && !pn.fa_live) statusBadge = ' <span class="badge" style="background:#475569; color:#cbd5e1; font-size:0.65rem;">DEAD</span>';
      else if (!pn.live && pn.fa_live) statusBadge = ' <span class="badge" style="background:#334155; color:#94a3b8; font-size:0.65rem;">FOLDED</span>';

      const lineCell = pn.line > 0 ? `<a href="#" onclick="jumpToSource(${pn.line}); return false;">${pn.line}</a>` : '-';

      tr.innerHTML = `
        <td><a href="#" onclick="showPNodeInspector(${f.id}, ${pn.id}); return false;" style="color:var(--primary); font-weight:700;">#${pn.id}</a></td>
        <td><span class="badge badge-kind">${pn.kind}</span>${statusBadge}</td>
        <td>${pn.prim ? `<span class="badge badge-prim">${escapeHtml(pn.prim)}</span>` : '-'}</td>
        <td>${lineCell}</td>
        <td>${lvalsStr || '-'}</td>
        <td>${rvalsStr || '-'}</td>
        <td>${callsStr || '-'}</td>
        <td>${pn.cfg_succ.map(id => `<a href="#" onclick="showPNodeInspector(${f.id}, ${id}); return false;">#${id}</a>`).join(', ') || '-'}</td>
        <td>${pn.cfg_pred.map(id => `<a href="#" onclick="showPNodeInspector(${f.id}, ${id}); return false;">#${id}</a>`).join(', ') || '-'}</td>
      `;
      tbody.appendChild(tr);
    });
  }

  // 2. Render Visual Graph View only when CFG view is visible
  const cfgPanel = document.getElementById('view-cfg');
  if (currentCfgMode === 'graph' && cfgPanel && cfgPanel.classList.contains('active')) {
    renderCFGGraph(f);
  }
}

function renderCFGGraph(f) {
  if (typeof cytoscape === 'undefined') {
    loadCallGraphScripts((ok) => {
      if (ok && typeof cytoscape !== 'undefined') {
        renderCFGGraph(f);
      } else {
        // Offline fallback: switch CFG view to Instruction Table
        currentCfgMode = 'table';
        document.getElementById('btn-cfg-mode-table').classList.add('active');
        document.getElementById('btn-cfg-mode-graph').classList.remove('active');
        document.getElementById('cfg-graph-view').style.display = 'none';
        document.getElementById('cfg-table-view').style.display = 'block';
        document.getElementById('cfg-graph-controls').style.display = 'none';
      }
    });
    return;
  }

  const container = document.getElementById('cy-cfg');
  if (!container) return;

  if (typeof cytoscapeElk !== 'undefined') {
    try { cytoscape.use(cytoscapeElk); } catch (e) {}
  }

  const elements = [];
  const pnodeIdSet = new Set(f.pnodes.map(p => p.id));

  f.pnodes.forEach((pn, idx) => {
    let lines = [];
    let header = `#${pn.id} ${pn.kind}`;
    if (pn.prim) header += ` [${pn.prim}]`;
    if (pn.line) header += ` (L${pn.line})`;
    if (!pn.live && !pn.fa_live) header += ` [DEAD]`;
    else if (!pn.live && pn.fa_live) header += ` [FOLDED]`;
    lines.push(header);

    if (pn.lvals && pn.lvals.length > 0) {
      const defs = pn.lvals.map(v => `${v.name || 'v'+v.id}: ${getTypeForVar(f, v.id, currentCfgPass, currentCfgContour)}`).join(', ');
      lines.push(`▶ def: ${defs}`);
    }
    if (pn.rvals && pn.rvals.length > 0) {
      const uses = pn.rvals.map(v => `${v.name || 'v'+v.id}: ${getTypeForVar(f, v.id, currentCfgPass, currentCfgContour)}`).join(', ');
      lines.push(`◀ use: ${uses}`);
    }
    if (pn.contour_calls && pn.contour_calls.length > 0) {
      const calls = pn.contour_calls.map(c => `${c.callee_name} (ES#${c.to_es})`).join(', ');
      lines.push(`☎ to: ${calls}`);
    }

    const classes = ['pnode-box'];
    if (pn.kind === 'SEND') classes.push('pnode-send');
    else if (pn.kind === 'IF') classes.push('pnode-if');
    else if (pn.kind === 'GOTO') classes.push('pnode-goto');
    else if (pn.kind === 'LABEL') classes.push('pnode-label');
    else classes.push('pnode-normal');

    if (pn.id === f.entry_id || (f.entry_id < 0 && idx === 0)) classes.push('pnode-entry');
    if (pn.id === f.exit_id || pn.fa_noreturn || pn.fa_noreturn_raises || pn.prim === 'prim_reply') classes.push('pnode-exit');
    if (!pn.live && !pn.fa_live) classes.push('pnode-dead');

    elements.push({
      group: 'nodes',
      data: {
        id: 'pn_' + pn.id,
        pnodeId: pn.id,
        kind: pn.kind,
        prim: pn.prim,
        line: pn.line,
        label: lines.join('\n')
      },
      classes: classes.join(' ')
    });
  });

  f.pnodes.forEach(pn => {
    (pn.cfg_succ || []).forEach(succ_id => {
      if (pnodeIdSet.has(succ_id)) {
        let edgeLabel = '';
        let edgeClass = '';
        if (pn.kind === 'IF' && pn.cfg_succ.length >= 2) {
          if (succ_id === pn.cfg_succ[0]) {
            edgeLabel = 'then';
            edgeClass = 'edge-then';
          } else {
            edgeLabel = 'else';
            edgeClass = 'edge-else';
          }
        }
        elements.push({
          group: 'edges',
          data: {
            id: `cfg_e_${pn.id}_${succ_id}`,
            source: 'pn_' + pn.id,
            target: 'pn_' + succ_id,
            label: edgeLabel
          },
          classes: edgeClass
        });
      }
    });
  });

  if (!cyCfgInstance) {
    cyCfgInstance = cytoscape({
      container: container,
      elements: elements,
      style: [
        {
          selector: 'node',
          style: {
            'label': 'data(label)',
            'font-family': 'ui-monospace, SFMono-Regular, Menlo, Monaco, Consolas, monospace',
            'font-size': '11px',
            'color': '#f8fafc',
            'text-valign': 'center',
            'text-halign': 'center',
            'background-color': '#1e293b',
            'border-width': 2,
            'border-color': '#475569',
            'shape': 'round-rectangle',
            'padding': '12px',
            'width': 'label',
            'height': 'label',
            'text-max-width': '320px',
            'text-wrap': 'wrap',
            'text-justification': 'left',
            'line-height': 1.4,
            'transition-property': 'background-color, border-color, border-width',
            'transition-duration': '0.15s'
          }
        },
        {
          selector: 'node.pnode-send',
          style: { 'background-color': '#2e1065', 'border-color': '#a855f7', 'color': '#f3e8ff' }
        },
        {
          selector: 'node.pnode-if',
          style: { 'background-color': '#431407', 'border-color': '#f97316', 'color': '#ffedd5' }
        },
        {
          selector: 'node.pnode-goto',
          style: { 'background-color': '#1e1b4b', 'border-color': '#6366f1', 'color': '#e0e7ff' }
        },
        {
          selector: 'node.pnode-label',
          style: { 'background-color': '#0f172a', 'border-color': '#334155', 'color': '#94a3b8' }
        },
        {
          selector: 'node.pnode-entry',
          style: { 'background-color': '#064e3b', 'border-color': '#10b981', 'color': '#d1fae5' }
        },
        {
          selector: 'node.pnode-exit',
          style: { 'background-color': '#450a0a', 'border-color': '#ef4444', 'color': '#fee2e2' }
        },
        {
          selector: 'node.pnode-dead',
          style: { 'opacity': 0.45, 'border-style': 'dashed' }
        },
        {
          selector: 'node.pnode-selected',
          style: {
            'border-color': '#38bdf8',
            'border-width': 3,
            'color': '#ffffff',
            'shadow-blur': 12,
            'shadow-color': '#38bdf8',
            'shadow-opacity': 0.8
          }
        },
        {
          selector: 'node.pnode-neighbor',
          style: {
            'border-color': '#22c55e',
            'border-width': 2,
            'color': '#ffffff'
          }
        },
        {
          selector: 'edge',
          style: {
            'width': 2,
            'line-color': '#475569',
            'target-arrow-color': '#38bdf8',
            'target-arrow-shape': 'triangle',
            'curve-style': 'bezier',
            'arrow-scale': 1.1,
            'label': 'data(label)',
            'font-size': '10px',
            'color': '#cbd5e1',
            'font-family': 'ui-monospace, monospace',
            'text-background-opacity': 0.85,
            'text-background-color': '#0f172a',
            'text-background-padding': '2px',
            'text-background-shape': 'roundrectangle'
          }
        },
        {
          selector: 'edge.edge-then',
          style: { 'line-color': '#22c55e', 'target-arrow-color': '#22c55e', 'color': '#4ade80' }
        },
        {
          selector: 'edge.edge-else',
          style: { 'line-color': '#f97316', 'target-arrow-color': '#f97316', 'color': '#fb923c' }
        },
        {
          selector: 'edge.edge-highlight',
          style: { 'line-color': '#38bdf8', 'target-arrow-color': '#38bdf8', 'width': 3 }
        }
      ]
    });

    cyCfgInstance.on('tap', 'node', function(evt) {
      const node = evt.target;
      const pnId = node.data('pnodeId');
      showPNodeInspector(currentCfgFunId, pnId);
    });

    cyCfgInstance.on('tap', function(evt) {
      if (evt.target === cyCfgInstance) {
        currentSelectedPNodeId = null;
        document.getElementById('cfg-inspector').style.display = 'none';
        cyCfgInstance.elements().removeClass('pnode-selected pnode-neighbor edge-highlight');
        syncCfgSplitDock(currentCfgFunId, null);
      }
    });
  } else {
    cyCfgInstance.elements().remove();
    cyCfgInstance.add(elements);
  }

  let layoutOptions = {
    name: 'elk',
    fit: true,
    padding: 35,
    animate: false,
    elk: {
      'algorithm': 'layered',
      'org.eclipse.elk.direction': 'DOWN',
      'org.eclipse.elk.layered.spacing.nodeNodeBetweenLayers': '50',
      'org.eclipse.elk.spacing.nodeNode': '30',
      'org.eclipse.elk.edgeRouting': 'POLYLINE'
    }
  };

  try {
    const l = cyCfgInstance.layout(layoutOptions);
    l.run();
  } catch (e) {
    cyCfgInstance.layout({
      name: 'breadthfirst',
      directed: true,
      spacingFactor: 1.5,
      padding: 35
    }).run();
  }
}

function showPNodeInspector(funId, pnodeId) {
  const f = DATA.phase2_cfg.functions.find(fn => fn.id === funId);
  if (!f) return;
  ensureFunIndexed(f);
  const pn = f.pnodes.find(p => p.id === pnodeId);
  if (!pn) return;

  currentSelectedPNodeId = pnodeId;
  syncCfgSplitDock(funId, pnodeId);

  const inspector = document.getElementById('cfg-inspector');
  if (!inspector) return;
  inspector.style.display = 'block';

  if (cyCfgInstance) {
    cyCfgInstance.elements().removeClass('pnode-selected pnode-neighbor edge-highlight');
    const node = cyCfgInstance.getElementById('pn_' + pnodeId);
    if (node && node.length > 0) {
      node.addClass('pnode-selected');
      node.incomers('edge').addClass('edge-highlight');
      node.outgoers('edge').addClass('edge-highlight');
      node.incomers('node').addClass('pnode-neighbor');
      node.outgoers('node').addClass('pnode-neighbor');
    }
  }

  const pNum = currentCfgPass === 'final' ? totalPasses : parseInt(currentCfgPass);
  const srcLineLink = pn.line > 0 ? `<a href="#" onclick="jumpToSource(${pn.line}); return false;">line ${pn.line}</a>` : '-';

  let html = `
    <div style="display:flex; justify-content:space-between; align-items:center;">
      <h4 style="margin:0;">PNode #${pn.id} (${pn.kind})</h4>
      <button class="btn-sm" onclick="document.getElementById('cfg-inspector').style.display='none'" style="padding:0.15rem 0.4rem;">✕</button>
    </div>
    <div class="prop-row" style="margin-top:0.4rem;"><span>Primitive:</span><span class="prop-val">${pn.prim ? `<span class="badge badge-prim">${escapeHtml(pn.prim)}</span>` : 'none'}</span></div>
    <div class="prop-row"><span>Source Line:</span><span class="prop-val">${srcLineLink}</span></div>
    <div class="prop-row"><span>Predecessors:</span><span class="prop-val">${pn.cfg_pred.map(id => `<a href="#" onclick="showPNodeInspector(${f.id}, ${id}); return false;" style="color:var(--primary); margin-left:0.25rem;">#${id}</a>`).join('') || 'entry'}</span></div>
    <div class="prop-row"><span>Successors:</span><span class="prop-val">${pn.cfg_succ.map(id => `<a href="#" onclick="showPNodeInspector(${f.id}, ${id}); return false;" style="color:var(--primary); margin-left:0.25rem;">#${id}</a>`).join('') || 'exit'}</span></div>
  `;

  if (pn.lvals && pn.lvals.length > 0) {
    html += `
      <div class="inspector-section">
        <div class="inspector-section-title">Defined Variables (Lvals)</div>
    `;
    pn.lvals.forEach(lv => {
      const v = f._varById.get(lv.id);
      const curType = getTypeForVar(f, lv.id, currentCfgPass, currentCfgContour);
      html += `
        <div style="margin-bottom:0.6rem; background:rgba(0,0,0,0.25); padding:0.4rem; border-radius:6px;">
          <div style="display:flex; justify-content:space-between; align-items:center;">
            <span><strong style="color:var(--accent-green);">${escapeHtml(lv.name || 'v'+lv.id)}</strong> <span style="font-size:0.75rem; color:var(--text-dim);">(id: ${lv.id})</span></span>
            <span class="badge badge-type">${escapeHtml(curType)}</span>
          </div>
      `;
      if (v && v.avars && v.avars.length > 0) {
        html += `
          <div style="font-size:0.75rem; color:var(--text-muted); margin-top:0.25rem;">Mapped AVars across contours:</div>
          <table class="inspector-table">
            <thead><tr><th>Contour</th><th>AVar</th><th>Contour Type</th><th>Actions</th></tr></thead>
            <tbody>
        `;
        v.avars.forEach(av => {
          html += `
            <tr>
              <td>ES#${av.contour_id}</td>
              <td>#${av.av_id}</td>
              <td><code>${escapeHtml(av.type_str)}</code></td>
              <td>
                <button class="btn-sm" style="padding:0.1rem 0.3rem; font-size:0.7rem;" onclick="jumpToDataflow(${av.av_id})">Trace</button>
                <button class="btn-sm" style="padding:0.1rem 0.3rem; font-size:0.7rem;" onclick="jumpToPassContour(${pNum}, ${av.contour_id})">Passes</button>
              </td>
            </tr>
          `;
        });
        html += `</tbody></table>`;
      }
      html += `</div>`;
    });
    html += `</div>`;
  }

  if (pn.rvals && pn.rvals.length > 0) {
    html += `
      <div class="inspector-section">
        <div class="inspector-section-title">Used Variables (Rvals)</div>
    `;
    pn.rvals.forEach(rv => {
      const v = f._varById.get(rv.id);
      const curType = getTypeForVar(f, rv.id, currentCfgPass, currentCfgContour);
      html += `
        <div style="margin-bottom:0.6rem; background:rgba(0,0,0,0.25); padding:0.4rem; border-radius:6px;">
          <div style="display:flex; justify-content:space-between; align-items:center;">
            <span><strong style="color:var(--primary);">${escapeHtml(rv.name || 'v'+rv.id)}</strong> <span style="font-size:0.75rem; color:var(--text-dim);">(id: ${rv.id})</span></span>
            <span class="badge badge-type">${escapeHtml(curType)}</span>
          </div>
      `;
      if (v && v.avars && v.avars.length > 0) {
        html += `
          <div style="font-size:0.75rem; color:var(--text-muted); margin-top:0.25rem;">Mapped AVars across contours:</div>
          <table class="inspector-table">
            <thead><tr><th>Contour</th><th>AVar</th><th>Contour Type</th><th>Actions</th></tr></thead>
            <tbody>
        `;
        v.avars.forEach(av => {
          html += `
            <tr>
              <td>ES#${av.contour_id}</td>
              <td>#${av.av_id}</td>
              <td><code>${escapeHtml(av.type_str)}</code></td>
              <td>
                <button class="btn-sm" style="padding:0.1rem 0.3rem; font-size:0.7rem;" onclick="jumpToDataflow(${av.av_id})">Trace</button>
                <button class="btn-sm" style="padding:0.1rem 0.3rem; font-size:0.7rem;" onclick="jumpToPassContour(${pNum}, ${av.contour_id})">Passes</button>
              </td>
            </tr>
          `;
        });
        html += `</tbody></table>`;
      }
      html += `</div>`;
    });
    html += `</div>`;
  }

  if (pn.contour_calls && pn.contour_calls.length > 0) {
    html += `
      <div class="inspector-section">
        <div class="inspector-section-title">Contour Calls & Dispatch Targets</div>
    `;
    pn.contour_calls.forEach(cc => {
      html += `
        <div style="margin-bottom:0.4rem; background:rgba(168,85,247,0.1); border:1px solid rgba(168,85,247,0.2); padding:0.4rem; border-radius:6px;">
          <div style="font-weight:600; color:var(--text-main);">${escapeHtml(cc.callee_name)} <span style="font-size:0.75rem; color:var(--text-muted);">(fun #${cc.callee_id})</span></div>
          <div style="font-size:0.75rem; color:var(--text-muted); margin:0.2rem 0;">Caller: ES#${cc.from_es} &rarr; Callee: ES#${cc.to_es}</div>
          <div style="display:flex; gap:0.3rem; margin-top:0.3rem;">
            <button class="btn-sm" style="flex:1; font-size:0.75rem; padding:0.2rem;" onclick="jumpToCFG(${cc.callee_id}, ${cc.to_es}, ${pNum})">Inspect Callee CFG &rarr;</button>
            <button class="btn-sm" style="font-size:0.75rem; padding:0.2rem;" onclick="jumpToPassContour(${pNum}, ${cc.to_es})">View in Passes</button>
          </div>
        </div>
      `;
    });
    html += `</div>`;
  }

  inspector.innerHTML = html;
}

function showVarInspector(funId, varId) {
  const f = DATA.phase2_cfg.functions.find(fn => fn.id === funId);
  if (!f) return;
  ensureFunIndexed(f);
  const v = f._varById.get(varId);
  if (!v) return;

  const inspector = document.getElementById('cfg-inspector');
  if (!inspector) return;
  inspector.style.display = 'block';

  const pNum = currentCfgPass === 'final' ? totalPasses : parseInt(currentCfgPass);
  const curType = getTypeForVar(f, v.id, currentCfgPass, currentCfgContour);

  let html = `
    <div style="display:flex; justify-content:space-between; align-items:center;">
      <h4 style="margin:0;">Variable: ${escapeHtml(v.name || 'v'+v.id)}</h4>
      <button class="btn-sm" onclick="document.getElementById('cfg-inspector').style.display='none'" style="padding:0.15rem 0.4rem;">✕</button>
    </div>
    <div class="prop-row" style="margin-top:0.4rem;"><span>Variable ID:</span><span class="prop-val">${v.id}</span></div>
    <div class="prop-row"><span>Scope:</span><span class="prop-val">${v.is_formal ? 'Formal Argument' : (v.is_internal ? 'Internal Temporary' : 'Local Variable')}</span></div>
    <div class="prop-row"><span>Unified Type:</span><span class="prop-val"><code>${escapeHtml(v.type_str)}</code></span></div>
    <div class="prop-row"><span>Current Type (Pass ${pNum}):</span><span class="prop-val"><span class="badge badge-type">${escapeHtml(curType)}</span></span></div>
  `;

  if (v.avars && v.avars.length > 0) {
    html += `
      <div class="inspector-section">
        <div class="inspector-section-title">Contour Mappings (AVars)</div>
        <table class="inspector-table">
          <thead><tr><th>Contour</th><th>AVar ID</th><th>Type</th><th>Actions</th></tr></thead>
          <tbody>
    `;
    v.avars.forEach(av => {
      html += `
        <tr>
          <td>ES#${av.contour_id}</td>
          <td>#${av.av_id}</td>
          <td><code>${escapeHtml(av.type_str)}</code></td>
          <td>
            <button class="btn-sm" style="padding:0.1rem 0.3rem; font-size:0.7rem;" onclick="jumpToDataflow(${av.av_id})">Trace</button>
            <button class="btn-sm" style="padding:0.1rem 0.3rem; font-size:0.7rem;" onclick="jumpToPassContour(${pNum}, ${av.contour_id})">Passes</button>
          </td>
        </tr>
      `;
    });
    html += `</tbody></table></div>`;
  }

  html += `
    <div class="inspector-section">
      <div class="inspector-section-title">Type Evolution Across Passes</div>
      <table class="inspector-table">
        <thead><tr><th>Pass</th><th>Inferred Type</th><th>Action</th></tr></thead>
        <tbody>
  `;
  for (let pi = 1; pi <= totalPasses; pi++) {
    const t = getTypeForVar(f, v.id, pi, 'all');
    html += `
      <tr>
        <td>Pass ${pi}</td>
        <td><code>${escapeHtml(t)}</code></td>
        <td><button class="btn-sm" style="padding:0.1rem 0.3rem; font-size:0.7rem;" onclick="cfgPassSelect.value='${pi}'; cfgPassSelect.dispatchEvent(new Event('change'));">View</button></td>
      </tr>
    `;
  }
  html += `</tbody></table></div>`;

  inspector.innerHTML = html;
}

function jumpToCFG(funId, contourId, passNum) {
  let targetFun = DATA.phase2_cfg.functions.find(fn => fn.id === funId);
  if (!targetFun && contourId !== undefined && contourId !== null && contourId >= 0) {
    targetFun = DATA.phase2_cfg.functions.find(fn => (fn.entry_sets || []).includes(contourId));
  }
  if (!targetFun) {
    targetFun = DATA.phase2_cfg.functions.find(fn => fn.wraps_id === funId);
  }
  const resolvedFunId = targetFun ? targetFun.id : funId;

  switchTab('cfg');

  const fSel = document.getElementById('cfg-fun-select');
  if (fSel) fSel.value = resolvedFunId;
  currentCfgFunId = resolvedFunId;

  if (passNum) {
    const pSel = document.getElementById('cfg-pass-select');
    if (pSel) pSel.value = passNum;
    currentCfgPass = String(passNum);
  }

  const pN = currentCfgPass === 'final' ? totalPasses : parseInt(currentCfgPass);
  loadPassData(pN, () => {
    updateCfgContours();
    if (contourId !== undefined && contourId !== null && contourId >= 0) {
      const cSel = document.getElementById('cfg-contour-select');
      if (cSel && cSel.querySelector(`option[value="${contourId}"]`)) {
        cSel.value = contourId;
        currentCfgContour = String(contourId);
      }
    }
    renderCFGForFunction(resolvedFunId);
  });
}

function jumpToCFGAndNode(funId, pnodeId, contourId, passNum) {
  const cArg = (contourId !== undefined && contourId !== null && contourId >= 0) ? contourId : 'all';
  const pArg = (passNum !== undefined && passNum !== null && passNum > 0) ? passNum : 'final';
  jumpToCFG(funId, cArg, pArg);
  setTimeout(() => {
    showPNodeInspector(currentCfgFunId, pnodeId);
    if (cyCfgInstance) {
      const node = cyCfgInstance.getElementById('pn_' + pnodeId);
      if (node && node.length > 0) {
        cyCfgInstance.animate({
          center: { eles: node },
          zoom: Math.max(cyCfgInstance.zoom(), 1.0),
          duration: 300
        });
      }
    }
  }, 100);
}

function jumpToPassContour(passNum, esId, csId) {
  switchTab('passes');

  const pNum = passNum || totalPasses;
  currentPassIndex = pNum - 1;
  displayPass(pNum);

  if (esId !== undefined && esId !== null && esId >= 0) {
    setTimeout(() => {
      const rows = document.querySelectorAll('#pass-ess-table tbody tr');
      rows.forEach(r => {
        if (r.cells[0] && r.cells[0].textContent.trim() === String(esId)) {
          r.scrollIntoView({ behavior: 'smooth', block: 'center' });
          r.style.outline = '2px solid var(--primary)';
          setTimeout(() => { r.style.outline = 'none'; }, 2200);
        }
      });
    }, 250);
  } else if (csId !== undefined && csId !== null && csId >= 0) {
    setTimeout(() => {
      const rows = document.querySelectorAll('#pass-css-table tbody tr');
      rows.forEach(r => {
        if (r.cells[0] && r.cells[0].textContent.trim() === String(csId)) {
          r.scrollIntoView({ behavior: 'smooth', block: 'center' });
          r.style.outline = '2px solid #c084fc';
          setTimeout(() => { r.style.outline = 'none'; }, 2200);
        }
      });
    }, 250);
  }
}

function jumpToDataflow(avId, passNum) {
  switchTab('dataflow');

  const targetPass = passNum || currentDfPass || totalPasses;
  const pSel = document.getElementById('df-pass-select');
  if (pSel && targetPass !== currentDfPass) {
    pSel.value = targetPass;
    currentDfPass = targetPass;
  }

  const search = document.getElementById('df-search');
  if (search) {
    search.value = String(avId);
  }

  if (!currentDfData || currentDfPass !== targetPass) {
    loadPassData(targetPass, (data) => {
      currentDfData = data;
      renderDataflowTable(String(avId), avId);
    });
  } else {
    renderDataflowTable(String(avId), avId);
  }
}

function renderCFG() {
  if (typeof cytoscape === 'undefined') {
    loadCallGraphScripts(() => {
      renderCFGCurrent();
    });
  } else {
    renderCFGCurrent();
    if (cyCfgInstance) {
      setTimeout(() => {
        cyCfgInstance.resize();
        cyCfgInstance.fit(undefined, 35);
      }, 50);
    }
  }
}

// Tab: IFA Passes & Dynamic Script Loader
const passSlider = document.getElementById('pass-slider');
passSlider.max = Math.max(1, totalPasses);

function loadPassData(passNum, callback) {
  callback(getPassData(passNum));
}

let passDiffMode = true;
let passDiffBaseChoice = 'prev';
let passDiffChangedOnly = false;

let splitsScopeChoice = 'auto'; // 'auto' | 'decided' | 'entering' | 'all'
let splitsKindFilter = 'all';   // 'all' | 'EntrySet' | 'CreationSet' | 'other'
let splitsStageFilter = null;   // null or stage string
let splitsExpandAll = false;
let lastRenderedPassArgs = null;

function getSplitsForPassNum(pNum) {
  if (!pNum || pNum < 1 || pNum > totalPasses) return [];
  const pMeta = getPassMeta(pNum);
  const pData = getPassData(pNum);
  if (pData && Array.isArray(pData.splits)) return pData.splits;
  if (pData && pData.summary && Array.isArray(pData.summary.splits)) return pData.summary.splits;
  if (pMeta && Array.isArray(pMeta.splits)) return pMeta.splits;
  if (pMeta && pMeta.summary && Array.isArray(pMeta.summary.splits)) return pMeta.summary.splits;
  if (pMeta && pMeta.data && Array.isArray(pMeta.data.splits)) return pMeta.data.splits;
  if (pMeta && pMeta.data && pMeta.data.summary && Array.isArray(pMeta.data.summary.splits)) return pMeta.data.summary.splits;
  return [];
}

function getAllPassesSplits() {
  const all = [];
  for (let p = 1; p <= totalPasses; p++) {
    const spArr = getSplitsForPassNum(p);
    spArr.forEach(sp => {
      all.push(Object.assign({}, sp, { pass_num: sp.pass_num || p }));
    });
  }
  return all;
}

function setSplitsScope(scope) {
  splitsScopeChoice = scope;
  if (lastRenderedPassArgs) {
    renderPassSplitsSection.apply(null, lastRenderedPassArgs);
    applyPassFilter();
  }
}

function setSplitsKindFilter(kind) {
  splitsKindFilter = kind;
  ['all', 'es', 'cs', 'other'].forEach(k => {
    const btn = document.getElementById('btn-splits-kind-' + k);
    if (!btn) return;
    const match = (k === 'all' && kind === 'all') ||
                  (k === 'es' && kind === 'EntrySet') ||
                  (k === 'cs' && kind === 'CreationSet') ||
                  (k === 'other' && kind === 'other');
    btn.classList.toggle('active', match);
  });
  if (lastRenderedPassArgs) {
    renderPassSplitsSection.apply(null, lastRenderedPassArgs);
    applyPassFilter();
  }
}

function setSplitsStageFilter(stage) {
  if (splitsStageFilter === stage) {
    splitsStageFilter = null;
  } else {
    splitsStageFilter = stage;
  }
  document.querySelectorAll('#pass-events-badges .stage-event-chip').forEach(ch => {
    ch.classList.toggle('active', splitsStageFilter && ch.dataset.stage === splitsStageFilter);
  });
  if (lastRenderedPassArgs) {
    renderPassSplitsSection.apply(null, lastRenderedPassArgs);
    applyPassFilter();
  }
}

function toggleSplitsExpandAll() {
  splitsExpandAll = !splitsExpandAll;
  const btn = document.getElementById('btn-splits-expand-all');
  if (btn) {
    btn.classList.toggle('active', splitsExpandAll);
    btn.textContent = splitsExpandAll ? '⊟ Collapse All' : '⊞ Expand All';
  }
  document.querySelectorAll('.split-extra-items').forEach(el => {
    el.style.display = splitsExpandAll ? 'flex' : 'none';
  });
  document.querySelectorAll('.split-expand-btn').forEach(b => {
    const cnt = b.dataset.hiddenCount || '';
    b.textContent = splitsExpandAll ? '▴ Show fewer' : `▾ +${cnt} more affected item(s)`;
  });
}

function toggleSplitRowItems(btnId, extraId) {
  const extraEl = document.getElementById(extraId);
  const btnEl = document.getElementById(btnId);
  if (!extraEl || !btnEl) return;
  const isHidden = extraEl.style.display === 'none';
  extraEl.style.display = isHidden ? 'flex' : 'none';
  const cnt = btnEl.dataset.hiddenCount || '';
  btnEl.textContent = isHidden ? '▴ Show fewer' : `▾ +${cnt} more affected item(s)`;
}

const SPLIT_SUBKIND_TOOLTIPS = {
  'TYPE_COMPAT': 'Partition incoming call edges of an EntrySet by argument/return AVar type compatibility',
  'SETTER_COMPAT': 'Partition incoming call edges by setter equivalence class on a confluence AVar',
  'MARK_SETTER_COMPAT': 'Partition incoming call edges by mark-distance setter compatibility',
  'SPLIT_EDGES_DYNAMIC': 'Filter incoming call edges per receiver CreationSet at a dynamic dispatch site',
  'PER_CS_RECEIVER': 'Split a method EntrySet so each receiver CreationSet gets its own method contour',
  'CSM_ELEMENT_CS': 'Split a container method EntrySet when same-class containers carry divergent element types',
  'UNTAGGED_DISPATCH': 'Split calling EntrySet per receiver CreationSet to resolve untagged container dispatch',
  'BOXING_RECEIVER': '2-way caller partition on method EntrySet driven by a BOXING violation on receiver fields/elements',
  'ARG_VIOL_RECEIVER': '2-way caller partition on method EntrySet driven by an ARG_VIOLATIONS demand on receiver',
  'BLOCKING_ES': '2-way caller partition on a shared EntrySet so reachable creation points of a demanded CreationSet can separate',
  'ES_BY_ARG_TYPES': 'Split enclosing EntrySet by caller argument types to duplicate a single-def (defs=1) demanded CreationSet',
  'ES_BY_CALL_SITE': 'Split enclosing EntrySet by returned assign-set reachability for a single-def (defs=1) demanded CreationSet',
  'NUMERIC_SEED_SPLIT': 'Backtrack pure numeric {int, float} confluence to callers with pure numeric seeds',
  'CPA_BY_CONTOUR': 'Separate CreationSet allocation sites by enclosing EntrySet contour and static arity',
  'PEEL_SINGLE_DEF': 'Peel an individual creation point (AVar) off a multi-def CreationSet into a fresh CreationSet',
  'CS_ASSIGN_SET_KEY': 'Route 4 (CS_DEF_PARTITION): partition CreationSet defs by element assign-set type signature',
  'CS_MEMBER_KEY': 'Route 4 (CS_DEF_PARTITION): partition CreationSet defs by instance member/slot type signature',
  'PARENT_FIRST_JOINSPLIT': 'Peel split-child EntrySet creation point from its parent CreationSet on demand',
  'CSELEM_REJOIN': 'Re-merge speculatively split empty-mint CreationSet back into peer CreationSet with identical element type',
  'CSELEM_RESPLIT': 'Re-split previously merged CreationSet whose converged element types diverged',
  'SPLIT_CONSTANTS': 'Mark AVar with split_Constants so distinct constant values split contours on the next pass',
  'DEMOTE_MIXED_ARITY': 'Demote tuple CreationSet with mixed arity across creation points to dynamic length'
};

function renderPassSplitsSection(passNum, pMeta, passData, basePassNum, baseMeta, baseData) {
  lastRenderedPassArgs = [passNum, pMeta, passData, basePassNum, baseMeta, baseData];
  const tbody = document.querySelector('#pass-splits-table tbody');
  const summaryLabel = document.getElementById('pass-splits-summary-label');
  const contextBanner = document.getElementById('pass-splits-context-banner');
  const stageChip = document.getElementById('pass-splits-stage-chip');
  if (!tbody) return;

  const decidedSplits = getSplitsForPassNum(passNum);
  const enteringPassNum = (basePassNum !== null && basePassNum !== undefined) ? basePassNum : (passNum > 1 ? passNum - 1 : null);
  const enteringSplits = enteringPassNum ? getSplitsForPassNum(enteringPassNum) : [];
  const allSplits = getAllPassesSplits();

  // Update scope buttons text & counts
  const btnDecided = document.getElementById('btn-splits-scope-decided');
  const btnEntering = document.getElementById('btn-splits-scope-entering');
  const btnAll = document.getElementById('btn-splits-scope-all');
  if (btnDecided) btnDecided.textContent = `🔨 Decided in Pass ${passNum} (${decidedSplits.length})`;
  if (btnEntering) {
    btnEntering.textContent = enteringPassNum
      ? `📥 Entering from Pass ${enteringPassNum} (${enteringSplits.length})`
      : `📥 Entering Pass (0)`;
  }
  if (btnAll) btnAll.textContent = `📚 All Passes (${allSplits.length})`;

  // Resolve effective scope (smart auto mode)
  let effectiveScope = splitsScopeChoice;
  let autoSwitchedNote = '';
  if (effectiveScope === 'auto') {
    if (decidedSplits.length > 0) {
      effectiveScope = 'decided';
    } else if (enteringPassNum && enteringSplits.length > 0) {
      effectiveScope = 'entering';
      autoSwitchedNote = `Pass ${passNum} reached quiescence with 0 new splits decided at the end of Pass ${passNum}. Showing the <strong>${enteringSplits.length} split(s) decided at the end of Pass ${enteringPassNum}</strong> that created Pass ${passNum}'s newly specialized contours.`;
    } else if (allSplits.length > 0) {
      effectiveScope = 'all';
      autoSwitchedNote = `No splits occurred in Pass ${passNum}${enteringPassNum ? ` or Pass ${enteringPassNum}` : ''}. Showing all <strong>${allSplits.length} split(s)</strong> across the full ${totalPasses}-pass analysis.`;
    } else {
      effectiveScope = 'decided';
    }
  } else if (effectiveScope === 'entering' && enteringPassNum) {
    autoSwitchedNote = `Showing splits decided during <code>extend_analysis()</code> at the end of <strong>Pass ${enteringPassNum}</strong>, which took effect when entering <strong>Pass ${passNum}</strong>.`;
  } else if (effectiveScope === 'decided' && decidedSplits.length > 0 && passNum < totalPasses) {
    autoSwitchedNote = `Showing splits decided during <code>extend_analysis()</code> at the end of <strong>Pass ${passNum}</strong> (after Pass ${passNum}'s types converged), which take effect in <strong>Pass ${passNum + 1}</strong>.`;
  }

  if (btnDecided) btnDecided.classList.toggle('active', effectiveScope === 'decided');
  if (btnEntering) btnEntering.classList.toggle('active', effectiveScope === 'entering');
  if (btnAll) btnAll.classList.toggle('active', effectiveScope === 'all');

  if (contextBanner) {
    if (autoSwitchedNote) {
      contextBanner.innerHTML = autoSwitchedNote;
      contextBanner.style.display = 'block';
    } else {
      contextBanner.style.display = 'none';
    }
  }

  if (stageChip) {
    if (splitsStageFilter) {
      stageChip.style.display = 'inline-block';
      stageChip.innerHTML = `<span class="badge badge-prim" style="cursor:pointer;" onclick="setSplitsStageFilter('${escapeHtml(splitsStageFilter)}')" title="Click to clear stage filter">Stage: ${escapeHtml(splitsStageFilter)} &times;</span>`;
    } else {
      stageChip.style.display = 'none';
    }
  }

  let sourceSplits = [];
  if (effectiveScope === 'decided') {
    sourceSplits = decidedSplits.map(sp => Object.assign({}, sp, { pass_num: sp.pass_num || passNum }));
  } else if (effectiveScope === 'entering') {
    sourceSplits = enteringSplits.map(sp => Object.assign({}, sp, { pass_num: sp.pass_num || enteringPassNum }));
  } else {
    sourceSplits = allSplits;
  }

  // Apply Kind & Stage filters
  const filteredSplits = sourceSplits.filter(sp => {
    if (splitsKindFilter === 'EntrySet' && sp.kind !== 'EntrySet') return false;
    if (splitsKindFilter === 'CreationSet' && sp.kind !== 'CreationSet') return false;
    if (splitsKindFilter === 'other' && (sp.kind === 'EntrySet' || sp.kind === 'CreationSet')) return false;
    if (splitsStageFilter && sp.stage !== splitsStageFilter) return false;
    return true;
  });

  const esSplitCount = filteredSplits.filter(s => s.kind === 'EntrySet').length;
  const csSplitCount = filteredSplits.filter(s => s.kind === 'CreationSet').length;
  const otherSplitCount = filteredSplits.length - esSplitCount - csSplitCount;
  if (summaryLabel) {
    summaryLabel.textContent = `(${filteredSplits.length} split${filteredSplits.length === 1 ? '' : 's'}: ${esSplitCount} EntrySet, ${csSplitCount} CreationSet${otherSplitCount > 0 ? `, ${otherSplitCount} Const/Arity` : ''})`;
  }

  tbody.innerHTML = '';
  if (filteredSplits.length === 0) {
    const emptyMsg = allSplits.length === 0
      ? 'No contour or creation-set splits were needed for this program (converged in initial contours).'
      : `No splits match the current scope/filter (${effectiveScope === 'decided' && enteringSplits.length > 0 ? `<a href="#" onclick="setSplitsScope('entering'); return false;" style="color:var(--primary); text-decoration:underline;">view ${enteringSplits.length} entering split(s) from Pass ${enteringPassNum}</a> or ` : ''}<a href="#" onclick="setSplitsScope('all'); setSplitsKindFilter('all'); if(splitsStageFilter) setSplitsStageFilter(splitsStageFilter); return false;" style="color:var(--primary); text-decoration:underline;">view all ${allSplits.length} split(s) across all passes</a>).`;
    tbody.innerHTML = `<tr><td colspan="5" style="text-align:center; padding:1.35rem; color:var(--text-muted);">${emptyMsg}</td></tr>`;
    return;
  }

  const frag = document.createDocumentFragment();
  filteredSplits.forEach((sp, rowIdx) => {
    const tr = document.createElement('tr');
    tr.dataset.diffStatus = 'new';
    const spPass = sp.pass_num || passNum;
    const effectPass = Math.min(totalPasses, spPass + 1);

    // 1. Pass / #
    const passCellHtml = `
      <div style="display:flex; flex-direction:column; gap:0.2rem;">
        <a href="#" onclick="jumpToPassContour(${spPass}); return false;" class="badge badge-contour" style="text-align:center;" title="Inspect Pass ${spPass}">Pass ${spPass}</a>
        <span style="font-size:0.72rem; color:var(--text-muted); font-family:var(--font-mono);">#${sp.id || (rowIdx + 1)}</span>
      </div>
    `;

    // 2. Stage & Mechanism
    const isViolStage = sp.stage === 'VIOLATION' || !!sp.viol_kind;
    const isCsStage = sp.kind === 'CreationSet' || sp.stage === 'CPA' || sp.stage === 'CS_DEF_PART';
    const stageCls = isViolStage ? 'stage-viol' : (isCsStage ? 'stage-cs' : '');
    let kindBadge = '';
    if (sp.kind === 'EntrySet') {
      kindBadge = `<span class="badge badge-contour" style="cursor:default;">EntrySet</span>`;
    } else if (sp.kind === 'CreationSet') {
      kindBadge = `<span class="badge badge-kind">CreationSet</span>`;
    } else if (sp.kind === 'ConstDemand') {
      kindBadge = `<span class="badge badge-var" style="cursor:default;">ConstDemand</span>`;
    } else {
      kindBadge = `<span class="badge diff-badge-mod">${escapeHtml(sp.kind || 'Split')}</span>`;
    }
    const subTip = SPLIT_SUBKIND_TOOLTIPS[sp.sub_kind] || sp.sub_kind || '';
    const stageCellHtml = `
      <div style="display:flex; flex-direction:column; gap:0.28rem; align-items:flex-start;">
        <span class="split-stage-badge ${stageCls}" style="cursor:pointer;" onclick="setSplitsStageFilter('${escapeHtml(sp.stage)}')" title="Click to filter by stage ${escapeHtml(sp.stage)}">${escapeHtml(sp.stage || 'SPLIT')}</span>
        ${kindBadge}
        <span class="split-subkind-badge" title="${escapeHtml(subTip)}">${escapeHtml(sp.sub_kind || '')}</span>
      </div>
    `;

    // 3. What Was Split (Parent -> Products)
    let targetCellHtml = '';
    const products = sp.products || [];
    if (sp.kind === 'EntrySet') {
      const funLink = sp.target_fun_id >= 0
        ? `<a href="#" onclick="jumpToCFG(${sp.target_fun_id}, ${sp.target_id}, ${spPass}); return false;" style="color:var(--primary); font-weight:700; text-decoration:underline;">${escapeHtml(sp.target_name)}</a>`
        : `<strong>${escapeHtml(sp.target_name)}</strong>`;
      const parentBadge = `<span class="badge badge-contour" onclick="jumpToPassContour(${spPass}, ${sp.target_id})" title="Highlight ES#${sp.target_id} in EntrySets table">ES#${sp.target_id}</span>`;
      const prodsHtml = products.map(pr => {
        const pBadgeCls = pr.is_new ? 'diff-badge-new' : 'badge-contour';
        const pLabel = pr.is_new ? `NEW ES#${pr.id}` : `ES#${pr.id}`;
        const sigHtml = pr.sig ? `<code style="font-size:0.7rem; color:#e2e8f0;">${escapeHtml(pr.sig)}</code>` : '';
        const cfgBtn = sp.target_fun_id >= 0
          ? `<button class="btn-sm" style="padding:0.05rem 0.3rem; font-size:0.66rem;" onclick="jumpToCFG(${sp.target_fun_id}, ${pr.id}, ${effectPass})" title="Inspect ES#${pr.id} in CFG (Pass ${effectPass})">🔀</button>`
          : '';
        return `
          <div class="split-product-pill">
            <span class="badge ${pBadgeCls}" style="cursor:pointer;" onclick="jumpToPassContour(${effectPass}, ${pr.id})" title="Highlight ES#${pr.id} in Pass ${effectPass}">${pLabel}</span>
            <span style="color:var(--text-muted); font-size:0.7rem;">(${pr.moved_count || 0} edge${(pr.moved_count || 0) === 1 ? '' : 's'})</span>
            ${sigHtml}
            ${cfgBtn}
          </div>
        `;
      }).join('');

      targetCellHtml = `
        <div style="margin-bottom:0.25rem;">
          ${parentBadge} ${funLink}
          <span style="font-size:0.72rem; color:var(--text-muted); margin-left:0.25rem;">(${sp.stayed_count || 0} stayed, ${sp.moved_count || 0} moved)</span>
        </div>
        <div style="display:flex; flex-direction:column; gap:0.15rem;">
          ${prodsHtml || '<span style="font-size:0.74rem; color:var(--text-muted);">Re-routed incoming edges</span>'}
        </div>
      `;
    } else if (sp.kind === 'CreationSet') {
      const parentBadge = `<span class="badge badge-kind" style="cursor:pointer;" onclick="jumpToPassContour(${spPass}, null, ${sp.target_id})" title="Highlight CS#${sp.target_id} in CreationSets table">CS#${sp.target_id}</span>`;
      const prodsHtml = products.map(pr => {
        const pBadgeCls = pr.is_new ? 'diff-badge-new' : 'badge-kind';
        const pLabel = pr.is_new ? `NEW CS#${pr.id}` : `CS#${pr.id}`;
        const sigHtml = pr.sig ? `<code style="font-size:0.7rem; color:#e2e8f0;">${escapeHtml(pr.sig)}</code>` : '';
        return `
          <div class="split-product-pill">
            <span class="badge ${pBadgeCls}" style="cursor:pointer;" onclick="jumpToPassContour(${effectPass}, null, ${pr.id})" title="Highlight CS#${pr.id} in Pass ${effectPass}">${pLabel}</span>
            <span style="color:var(--text-muted); font-size:0.7rem;">(${pr.moved_count || 0} def${(pr.moved_count || 0) === 1 ? '' : 's'})</span>
            ${sigHtml}
          </div>
        `;
      }).join('');

      targetCellHtml = `
        <div style="margin-bottom:0.25rem;">
          ${parentBadge} <strong>${escapeHtml(sp.target_name)}</strong>
          <span style="font-size:0.72rem; color:var(--text-muted); margin-left:0.25rem;">(${sp.stayed_count || 0} stayed, ${sp.moved_count || 0} separated)</span>
        </div>
        <div style="display:flex; flex-direction:column; gap:0.15rem;">
          ${prodsHtml || '<span style="font-size:0.74rem; color:var(--text-muted);">Separated creation points</span>'}
        </div>
      `;
    } else if (sp.kind === 'ConstDemand') {
      const esBadge = `<span class="badge badge-contour" onclick="jumpToPassContour(${spPass}, ${sp.target_id})">ES#${sp.target_id}</span>`;
      const funLink = sp.target_fun_id >= 0
        ? `<a href="#" onclick="jumpToCFG(${sp.target_fun_id}, ${sp.target_id}, ${spPass}); return false;" style="color:var(--primary); font-weight:700; text-decoration:underline;">${escapeHtml(sp.target_name)}</a>`
        : `<strong>${escapeHtml(sp.target_name)}</strong>`;
      const favHtml = sp.trigger_av_id >= 0
        ? `<div style="margin-top:0.2rem; font-size:0.74rem;">Formal: <a href="#" onclick="jumpToDataflow(${sp.trigger_av_id}, ${spPass}); return false;" class="badge badge-var">${escapeHtml(sp.trigger_var_name || ('AVar #' + sp.trigger_av_id))}</a> <span style="color:var(--text-muted);">(${sp.moved_count || 0} constant partition(s))</span></div>`
        : '';
      targetCellHtml = `
        <div>
          ${esBadge} ${funLink}
          ${favHtml}
        </div>
      `;
    } else {
      targetCellHtml = `
        <div>
          <span class="badge badge-kind" style="cursor:pointer;" onclick="jumpToPassContour(${spPass}, null, ${sp.target_id})">CS#${sp.target_id}</span>
          <strong>${escapeHtml(sp.target_name)}</strong>
          <div style="margin-top:0.25rem; font-size:0.73rem; color:var(--text-muted);">Marked <code>no_static_arity = 1</code></div>
        </div>
      `;
    }

    // 4. Why It Split (Trigger, Demand & Converged Union)
    let triggerMetaParts = [];
    if (sp.viol_kind) {
      const vTitle = sp.viol_inferred ? 'Inferred: the first violation on the trigger AVar or in the split contour; the splitter did not name it' : 'Type Violation at Quiescence';
      triggerMetaParts.push(`<span class="badge badge-dead" style="text-decoration:none;" title="${vTitle}">⚠️ ${escapeHtml(sp.viol_kind)}${sp.viol_inferred ? ' (inferred)' : ''}</span>`);
    }
    if (sp.demand_cs_id !== undefined && sp.demand_cs_id >= 0) {
      triggerMetaParts.push(`<span class="badge badge-kind" style="cursor:pointer;" onclick="jumpToPassContour(${spPass}, null, ${sp.demand_cs_id})" title="Demanded CreationSet driving this split">Demanded: CS#${sp.demand_cs_id} (${escapeHtml(sp.demand_cs_sym || 'CS')})</span>`);
    }
    if (sp.trigger_av_id !== undefined && sp.trigger_av_id >= 0) {
      const vLabel = sp.trigger_var_name ? `${sp.trigger_var_name} (#${sp.trigger_av_id})` : `AVar #${sp.trigger_av_id}`;
      triggerMetaParts.push(`<span>Trigger: <a href="#" onclick="jumpToDataflow(${sp.trigger_av_id}, ${spPass}); return false;" class="badge badge-var" title="Trace AVar #${sp.trigger_av_id} in Bidirectional Dataflow Tracer">${escapeHtml(vLabel)}</a></span>`);
      if (sp.trigger_role) {
        triggerMetaParts.push(`<span style="color:var(--text-dim);">(${escapeHtml(sp.trigger_role)})</span>`);
      }
    }
    if (sp.trigger_fun_id !== undefined && sp.trigger_fun_id >= 0) {
      const cId = (sp.trigger_contour_id !== undefined && sp.trigger_contour_id >= 0) ? sp.trigger_contour_id : 'all';
      const cText = (sp.trigger_contour_id !== undefined && sp.trigger_contour_id >= 0) ? ` [ES#${sp.trigger_contour_id}]` : '';
      triggerMetaParts.push(`<span>in <a href="#" onclick="jumpToCFG(${sp.trigger_fun_id}, '${cId}', ${spPass}); return false;" style="color:var(--primary); text-decoration:underline;">${escapeHtml(sp.trigger_fun_name || ('fun #' + sp.trigger_fun_id))}${escapeHtml(cText)}</a></span>`);
    }
    if (sp.trigger_type_str) {
      const unionParts = sp.trigger_type_str.split(' | ').filter(Boolean);
      const uBadges = unionParts.slice(0, 8).map(t => `<span class="badge badge-type">${escapeHtml(t)}</span>`).join(' ');
      const moreU = unionParts.length > 8 ? ` <span style="font-size:0.7rem; color:var(--text-muted);">+${unionParts.length - 8} more</span>` : '';
      triggerMetaParts.push(`<span>Union: ${uBadges}${moreU}</span>`);
    }

    const whyCellHtml = `
      <div class="split-reason-box">${escapeHtml(sp.reason || '')}</div>
      <div class="split-trigger-meta">${triggerMetaParts.join(' ')}</div>
    `;

    // 5. What Was Affected (Call Edges / Creation Points)
    let impactCellHtml = '';
    const edges = sp.affected_edges || [];
    const defs = sp.affected_defs || [];

    if (edges.length > 0) {
      const renderEdgeItem = (e) => {
        const callerLink = e.from_fun_id >= 0
          ? `<a href="#" onclick="jumpToCFGAndNode(${e.from_fun_id}, ${e.pnode_id}, ${e.from_es}, ${spPass}); return false;" style="color:var(--primary); font-weight:600; text-decoration:underline;" title="Open Caller CFG at PNode #${e.pnode_id}">${escapeHtml(e.from_fun_name)}</a>`
          : escapeHtml(e.from_fun_name || 'caller');
        const fromEsBadge = e.from_es >= 0 ? `<span class="badge badge-contour" onclick="jumpToPassContour(${spPass}, ${e.from_es})">ES#${e.from_es}</span>` : '';
        const lineBtn = e.line > 0
          ? `<a href="#" onclick="jumpToSource(${e.line}); return false;" style="color:var(--text-muted); text-decoration:underline;" title="Jump to Python Source Line ${e.line}">L${e.line}</a>`
          : '';
        const argsCode = e.arg_summary ? `<code style="font-size:0.68rem; color:#cbd5e1;">(${escapeHtml(e.arg_summary)})</code>` : '';
        return `
          <div class="split-impact-item">
            <span style="color:var(--text-dim); font-family:var(--font-mono);">Edge #${e.edge_id}:</span>
            ${callerLink} ${fromEsBadge}
            <span style="color:var(--text-muted);">@ PNode #${e.pnode_id}</span> ${lineBtn}
            <span style="font-family:var(--font-mono);"><span class="diff-old">ES#${e.old_to_es}</span> &rarr; <span class="diff-new">ES#${e.new_to_es}</span></span>
            ${argsCode}
          </div>
        `;
      };
      const visibleEdges = edges.slice(0, 3);
      const hiddenEdges = edges.slice(3);
      const extraId = `split-extra-${spPass}-${rowIdx}`;
      const btnId = `split-btn-${spPass}-${rowIdx}`;
      impactCellHtml = `
        <div style="font-size:0.74rem; font-weight:600; color:#38bdf8; margin-bottom:0.2rem;">
          🔀 ${edges.length} Call Edge${edges.length === 1 ? '' : 's'} Re-routed
        </div>
        <div class="split-impact-list">
          ${visibleEdges.map(renderEdgeItem).join('')}
        </div>
        ${hiddenEdges.length > 0 ? `
          <div id="${extraId}" class="split-impact-list split-extra-items" style="display:${splitsExpandAll ? 'flex' : 'none'}; margin-top:0.3rem;">
            ${hiddenEdges.map(renderEdgeItem).join('')}
          </div>
          <button id="${btnId}" class="btn-sm split-expand-btn" data-hidden-count="${hiddenEdges.length}" style="margin-top:0.3rem; padding:0.12rem 0.45rem; font-size:0.7rem;" onclick="toggleSplitRowItems('${btnId}', '${extraId}')">
            ${splitsExpandAll ? '▴ Show fewer' : `▾ +${hiddenEdges.length} more affected item(s)`}
          </button>
        ` : ''}
      `;
    } else if (defs.length > 0) {
      const renderDefItem = (d) => {
        const avBadge = d.av_id >= 0
          ? `<a href="#" onclick="jumpToDataflow(${d.av_id}, ${spPass}); return false;" class="badge badge-var" title="Inspect Creation Point AVar #${d.av_id} in Dataflow Tracer">AVar #${d.av_id}</a>`
          : '';
        const varCode = d.var_name ? `<code>${escapeHtml(d.var_name)}</code>` : '';
        const funLink = (d.fun_id >= 0 && d.pnode_id >= 0)
          ? `<a href="#" onclick="jumpToCFGAndNode(${d.fun_id}, ${d.pnode_id}, ${d.es_id}, ${spPass}); return false;" style="color:var(--primary); text-decoration:underline;" title="Inspect Allocation Site PNode #${d.pnode_id} in CFG">${escapeHtml(d.fun_name || ('fun #' + d.fun_id))}</a>`
          : (d.fun_id >= 0
              ? `<a href="#" onclick="jumpToCFG(${d.fun_id}, ${d.es_id >= 0 ? d.es_id : "'all'"}, ${spPass}); return false;" style="color:var(--primary); text-decoration:underline;">${escapeHtml(d.fun_name || ('fun #' + d.fun_id))}</a>`
              : escapeHtml(d.fun_name || 'global'));
        const esBadge = d.es_id >= 0 ? `<span class="badge badge-contour" onclick="jumpToPassContour(${spPass}, ${d.es_id})">ES#${d.es_id}</span>` : '';
        const lineBtn = d.line > 0
          ? `<a href="#" onclick="jumpToSource(${d.line}); return false;" style="color:var(--text-muted); text-decoration:underline;" title="Jump to Python Source Line ${d.line}">L${d.line}</a>`
          : '';
        const detHtml = d.detail ? `<code style="font-size:0.68rem; color:#cbd5e1;">[${escapeHtml(d.detail)}]</code>` : '';
        return `
          <div class="split-impact-item">
            ${avBadge} ${varCode}
            <span>in ${funLink}</span> ${esBadge}
            ${d.pnode_id >= 0 ? `<span style="color:var(--text-muted);">@ PNode #${d.pnode_id}</span>` : ''} ${lineBtn}
            <span style="font-family:var(--font-mono);"><span class="diff-old">CS#${d.old_cs}</span> &rarr; <span class="diff-new">CS#${d.new_cs}</span></span>
            ${detHtml}
          </div>
        `;
      };
      const visibleDefs = defs.slice(0, 3);
      const hiddenDefs = defs.slice(3);
      const extraId = `split-extra-${spPass}-${rowIdx}`;
      const btnId = `split-btn-${spPass}-${rowIdx}`;
      impactCellHtml = `
        <div style="font-size:0.74rem; font-weight:600; color:#c084fc; margin-bottom:0.2rem;">
          📦 ${defs.length} Creation Point${defs.length === 1 ? '' : 's'} Separated
        </div>
        <div class="split-impact-list">
          ${visibleDefs.map(renderDefItem).join('')}
        </div>
        ${hiddenDefs.length > 0 ? `
          <div id="${extraId}" class="split-impact-list split-extra-items" style="display:${splitsExpandAll ? 'flex' : 'none'}; margin-top:0.3rem;">
            ${hiddenDefs.map(renderDefItem).join('')}
          </div>
          <button id="${btnId}" class="btn-sm split-expand-btn" data-hidden-count="${hiddenDefs.length}" style="margin-top:0.3rem; padding:0.12rem 0.45rem; font-size:0.7rem;" onclick="toggleSplitRowItems('${btnId}', '${extraId}')">
            ${splitsExpandAll ? '▴ Show fewer' : `▾ +${hiddenDefs.length} more affected item(s)`}
          </button>
        ` : ''}
      `;
    } else if (sp.kind === 'ConstDemand') {
      impactCellHtml = `
        <div class="split-impact-item">
          <span>Enables constant-value contour splitting on</span>
          <a href="#" onclick="jumpToDataflow(${sp.target_id}, ${spPass}); return false;" class="badge badge-var">AVar #${sp.target_id}</a>
          <span>in Pass ${effectPass}</span>
        </div>
      `;
    } else if (sp.kind === 'ArityDemote') {
      impactCellHtml = `
        <div class="split-impact-item">
          <span>Tuple</span>
          <span class="badge badge-kind" onclick="jumpToPassContour(${spPass}, null, ${sp.target_id})">CS#${sp.target_id}</span>
          <span>widened to dynamic length (<code>no_static_arity</code>) for Pass ${effectPass}</span>
        </div>
      `;
    } else {
      impactCellHtml = `<span style="font-size:0.74rem; color:var(--text-muted);">${sp.moved_count || 0} item(s) affected</span>`;
    }

    tr.innerHTML = `
      <td>${passCellHtml}</td>
      <td>${stageCellHtml}</td>
      <td>${targetCellHtml}</td>
      <td>${whyCellHtml}</td>
      <td>${impactCellHtml}</td>
    `;
    frag.appendChild(tr);
  });
  tbody.appendChild(frag);
}

const passDiffBaseSelect = document.getElementById('pass-diff-base-select');
if (passDiffBaseSelect) {
  for (let p = 1; p <= totalPasses; p++) {
    const opt = document.createElement('option');
    opt.value = String(p);
    opt.textContent = `Pass ${p}`;
    passDiffBaseSelect.appendChild(opt);
  }
  passDiffBaseSelect.addEventListener('change', () => {
    passDiffBaseChoice = passDiffBaseSelect.value;
    displayPass(currentPassIndex + 1);
  });
}

const btnPassDiffToggle = document.getElementById('btn-pass-diff-toggle');
if (btnPassDiffToggle) {
  btnPassDiffToggle.addEventListener('click', () => {
    passDiffMode = !passDiffMode;
    btnPassDiffToggle.classList.toggle('active', passDiffMode);
    btnPassDiffToggle.textContent = passDiffMode ? '🔀 Diff Mode: ON' : '🔀 Diff Mode: OFF';
    if (!passDiffMode && passDiffChangedOnly) {
      passDiffChangedOnly = false;
      const coBtn = document.getElementById('btn-pass-changed-only');
      if (coBtn) coBtn.classList.remove('active');
    }
    displayPass(currentPassIndex + 1);
  });
}

const btnPassChangedOnly = document.getElementById('btn-pass-changed-only');
if (btnPassChangedOnly) {
  btnPassChangedOnly.addEventListener('click', () => {
    passDiffChangedOnly = !passDiffChangedOnly;
    btnPassChangedOnly.classList.toggle('active', passDiffChangedOnly);
    if (passDiffChangedOnly && !passDiffMode) {
      passDiffMode = true;
      if (btnPassDiffToggle) {
        btnPassDiffToggle.classList.add('active');
        btnPassDiffToggle.textContent = '🔀 Diff Mode: ON';
      }
      displayPass(currentPassIndex + 1);
    } else {
      applyPassFilter();
    }
  });
}

function getResolvedBasePassNum(passNum) {
  if (!passDiffMode || totalPasses <= 1) return null;
  if (passDiffBaseChoice === 'prev') {
    return passNum > 1 ? passNum - 1 : null;
  }
  const b = parseInt(passDiffBaseChoice, 10);
  if (!isNaN(b) && b >= 1 && b <= totalPasses && b !== passNum) return b;
  return null;
}

function formatDeltaBadge(currVal, baseVal, warnOnIncrease) {
  const c = currVal || 0;
  const b = baseVal || 0;
  const d = c - b;
  if (d === 0) return `<span class="diff-delta zero">±0</span>`;
  if (d > 0) return `<span class="diff-delta ${warnOnIncrease ? 'warn-plus' : 'plus'}">+${d}</span>`;
  return `<span class="diff-delta minus">${d}</span>`;
}

function formatEsArgType(a) {
  return `[${(a.type || []).map(t => escapeHtml(t.sym) + (t.is_const ? `=${escapeHtml(t.val)}` : '')).join('|')}]`;
}

function formatEsRetType(r) {
  return `[${(r.type || []).map(t => escapeHtml(t.sym)).join('|')}]`;
}

function formatCsFieldType(f) {
  return `[${(f.type || []).map(t => escapeHtml(t.sym)).join('|')}]`;
}

function formatAvarTypeSet(av) {
  if (!av || !av.type || av.type.length === 0) return 'void';
  return Array.from(new Set(av.type.map(t => t.sym + (t.is_const ? `=${t.val}` : '')))).join(' | ');
}

function applyPassFilter() {
  const filterInput = document.getElementById('pass-filter-input');
  const q = filterInput ? filterInput.value.trim().toLowerCase() : '';
  const hasActiveDiff = getResolvedBasePassNum(currentPassIndex + 1) !== null;
  document.querySelectorAll('#pass-splits-table tbody tr, #pass-ess-table tbody tr, #pass-css-table tbody tr, #pass-viol-table tbody tr, #pass-resolved-viol-table tbody tr, #pass-avar-diff-table tbody tr').forEach(tr => {
    if (tr.cells.length <= 1) return;
    const matchesText = !q || tr.textContent.toLowerCase().includes(q);
    const matchesChanged = !(passDiffChangedOnly && hasActiveDiff) || (tr.dataset.diffStatus && tr.dataset.diffStatus !== 'unchanged');
    tr.style.display = (matchesText && matchesChanged) ? '' : 'none';
  });
}

const passFilterInput = document.getElementById('pass-filter-input');
if (passFilterInput) {
  passFilterInput.addEventListener('input', applyPassFilter);
}

function displayPass(passNum) {
  document.getElementById('pass-indicator').textContent = `Pass ${passNum} / ${totalPasses}`;
  passSlider.value = passNum;

  const pMeta = getPassMeta(passNum);
  const basePassNum = getResolvedBasePassNum(passNum);

  loadPassData(passNum, (passData) => {
    if (!passData && !pMeta) return;
    if (basePassNum !== null) {
      loadPassData(basePassNum, (baseData) => {
        renderPassWithOptionalDiff(passNum, pMeta, passData, basePassNum, getPassMeta(basePassNum), baseData);
      });
    } else {
      renderPassWithOptionalDiff(passNum, pMeta, passData, null, null, null);
    }
  });
}

function renderPassWithOptionalDiff(passNum, pMeta, passData, basePassNum, baseMeta, baseData) {
  const s = (passData && passData.summary) || (pMeta && pMeta.summary) || passData || {};
  const bs = basePassNum !== null ? ((baseData && baseData.summary) || (baseMeta && baseMeta.summary) || baseData || null) : null;

  const decidedSplits = getSplitsForPassNum(passNum);
  const enteringPassNum = basePassNum !== null ? basePassNum : (passNum > 1 ? passNum - 1 : null);
  const enteringSplits = enteringPassNum ? getSplitsForPassNum(enteringPassNum) : [];

  document.getElementById('pass-card-ess').innerHTML = `${s.ess_count || 0}${bs ? formatDeltaBadge(s.ess_count, bs.ess_count, false) : ''}`;
  document.getElementById('pass-card-css').innerHTML = `${s.css_count || 0}${bs ? formatDeltaBadge(s.css_count, bs.css_count, false) : ''}`;
  const splitsCardEl = document.getElementById('pass-card-splits');
  if (splitsCardEl) {
    const subHint = (decidedSplits.length === 0 && enteringSplits.length > 0)
      ? `<span class="diff-delta plus" title="${enteringSplits.length} split(s) entered from Pass ${enteringPassNum}">${enteringSplits.length} in</span>`
      : '';
    splitsCardEl.innerHTML = `${decidedSplits.length}${subHint}`;
  }
  document.getElementById('pass-card-viol').innerHTML = `${s.violations_count || 0}${bs ? formatDeltaBadge(s.violations_count, bs.violations_count, true) : ''}`;
  document.getElementById('pass-card-dup').innerHTML = `${(s.dup_splits || 0) + (s.cs_dup_splits || 0)}${bs ? formatDeltaBadge((s.dup_splits || 0) + (s.cs_dup_splits || 0), (bs.dup_splits || 0) + (bs.cs_dup_splits || 0), true) : ''}`;

  const summaryOnlyBanner = document.getElementById('pass-summary-only-banner');
  if (summaryOnlyBanner) {
    summaryOnlyBanner.style.display = ((pMeta && pMeta.summary_only) || (passData && passData.summary_only)) ? 'block' : 'none';
  }

  // Splitter Stage Events (supports both Array of stage event objects and key/value map)
  const eventsSec = document.getElementById('pass-events-section');
  const eventsBadges = document.getElementById('pass-events-badges');
  if (eventsSec && eventsBadges) {
    eventsBadges.innerHTML = '';
    const rawEvents = s.events || (passData && passData.events) || [];
    if (Array.isArray(rawEvents)) {
      const activeEvents = rawEvents.filter(ev => ev && ev.splits > 0);
      if (activeEvents.length > 0) {
        eventsSec.style.display = 'block';
        activeEvents.forEach(ev => {
          const chip = document.createElement('div');
          chip.className = 'stage-event-chip' + (splitsStageFilter === ev.stage ? ' active' : '');
          chip.dataset.stage = ev.stage;
          chip.onclick = () => setSplitsStageFilter(ev.stage);
          const deltas = [];
          if (ev.ess_before !== ev.ess_after) deltas.push(`ESS ${ev.ess_before}&rarr;${ev.ess_after}`);
          if (ev.css_before !== ev.css_after) deltas.push(`CSS ${ev.css_before}&rarr;${ev.css_after}`);
          if (ev.viol_before !== undefined && ev.viol_after !== undefined) deltas.push(`Viol ${ev.viol_before}`);
          const deltaStr = deltas.length > 0 ? `<span style="color:var(--text-muted); font-size:0.7rem;">(${deltas.join(', ')})</span>` : '';
          chip.innerHTML = `<strong style="color:#38bdf8; font-family:var(--font-mono);">${escapeHtml(ev.stage)}</strong>: <span>${ev.splits} split${ev.splits === 1 ? '' : 's'}</span> ${deltaStr}`;
          eventsBadges.appendChild(chip);
        });
      } else {
        eventsSec.style.display = 'none';
      }
    } else {
      const activeEvents = Object.entries(rawEvents).filter(([k, v]) => v > 0);
      if (activeEvents.length > 0) {
        eventsSec.style.display = 'block';
        activeEvents.forEach(([k, v]) => {
          const span = document.createElement('span');
          span.className = 'stage-event-chip' + (splitsStageFilter === k ? ' active' : '');
          span.dataset.stage = k;
          span.onclick = () => setSplitsStageFilter(k);
          span.textContent = `${k}: ${v}`;
          eventsBadges.appendChild(span);
        });
      } else {
        eventsSec.style.display = 'none';
      }
    }
  }

  // Render Deep-Dive Splits Section
  renderPassSplitsSection(passNum, pMeta, passData, basePassNum, baseMeta, baseData);

  const hasFullDiff = basePassNum !== null && baseData && !baseData.summary_only && passData && !passData.summary_only;
  const getViolAvId = (vl) => (vl.av_id !== undefined ? vl.av_id : vl.avar_id);
  const getViolTypes = (vl) => Array.isArray(vl.types) ? vl.types : (vl.type_str ? vl.type_str.split(' | ').filter(Boolean) : []);
  const violKey = (vl) => `${vl.kind}:${getViolAvId(vl)}:${vl.var_id}:${vl.contour_id}`;
  const baseViolMap = new Map();
  if (hasFullDiff && baseData.violations) {
    baseData.violations.forEach(vl => baseViolMap.set(violKey(vl), vl));
  }

  // Type Violations Table
  const violSec = document.getElementById('pass-violations-section');
  const violTbody = document.querySelector('#pass-viol-table tbody');
  const currViolKeys = new Set();
  if (violSec && violTbody) {
    violTbody.innerHTML = '';
    const viols = (passData && passData.violations) || [];
    if (viols.length > 0) {
      violSec.style.display = 'block';
      viols.forEach(vl => {
        const k = violKey(vl);
        currViolKeys.add(k);
        const avId = getViolAvId(vl);
        const vTypes = getViolTypes(vl);
        const isNewViol = hasFullDiff && !baseViolMap.has(k);
        const tr = document.createElement('tr');
        tr.dataset.diffStatus = isNewViol ? 'new' : 'changed';
        if (isNewViol) tr.className = 'diff-row-added';
        const statusBadge = hasFullDiff
          ? (isNewViol ? '<span class="badge diff-badge-new" style="margin-right:0.3rem;">NEW</span>' : '<span class="badge diff-badge-pers" style="margin-right:0.3rem;">PERSISTENT</span>')
          : '';
        const funLabel = vl.fun_name ? `${escapeHtml(vl.fun_name)} (#${vl.fun_id})` : (vl.fun_id >= 0 ? `fun #${vl.fun_id}` : 'global');
        const funLink = vl.fun_id >= 0
          ? `<a href="#" onclick="jumpToCFG(${vl.fun_id}, ${vl.contour_id}, ${passNum}); return false;" style="color:var(--primary); text-decoration:underline;">${funLabel}</a>`
          : funLabel;
        const typesBadges = vTypes.map(t => `<span class="badge badge-type">${escapeHtml(t)}</span>`).join(' ');
        tr.innerHTML = `
          <td>${statusBadge}<span class="badge badge-dead" style="text-decoration:none;">${escapeHtml(vl.kind)}</span></td>
          <td><a href="#" onclick="jumpToDataflow(${avId}, ${passNum}); return false;">#${avId}</a></td>
          <td><code>${escapeHtml(vl.var_name || ('v' + vl.var_id))}</code></td>
          <td>${funLink}</td>
          <td>${vl.contour_id >= 0 ? `<a href="#" onclick="jumpToPassContour(${passNum}, ${vl.contour_id}); return false;">ES#${vl.contour_id}</a>` : '-'}</td>
          <td>${typesBadges || '-'}</td>
        `;
        violTbody.appendChild(tr);
      });
    } else {
      violSec.style.display = 'none';
    }
  }

  // Resolved Violations Table (violations in basePassNum that were eliminated in passNum)
  const resViolSec = document.getElementById('pass-resolved-viol-section');
  const resViolTbody = document.querySelector('#pass-resolved-viol-table tbody');
  if (resViolSec && resViolTbody) {
    resViolTbody.innerHTML = '';
    const resolvedViols = hasFullDiff
      ? (baseData.violations || []).filter(vl => !currViolKeys.has(violKey(vl)))
      : [];
    if (resolvedViols.length > 0) {
      resViolSec.style.display = 'block';
      resolvedViols.forEach(vl => {
        const avId = getViolAvId(vl);
        const vTypes = getViolTypes(vl);
        const tr = document.createElement('tr');
        tr.dataset.diffStatus = 'resolved';
        tr.className = 'diff-row-added';
        const funLabel = vl.fun_name ? `${escapeHtml(vl.fun_name)} (#${vl.fun_id})` : (vl.fun_id >= 0 ? `fun #${vl.fun_id}` : 'global');
        const funLink = vl.fun_id >= 0
          ? `<a href="#" onclick="jumpToCFG(${vl.fun_id}, ${vl.contour_id}, ${passNum}); return false;" style="color:var(--primary); text-decoration:underline;">${funLabel}</a>`
          : funLabel;
        const typesBadges = vTypes.map(t => `<span class="badge badge-type">${escapeHtml(t)}</span>`).join(' ');
        tr.innerHTML = `
          <td><span class="badge diff-badge-resolved" style="margin-right:0.3rem;">RESOLVED</span><span class="badge badge-kind">${escapeHtml(vl.kind)}</span></td>
          <td><a href="#" onclick="jumpToDataflow(${avId}, ${passNum}); return false;">#${avId}</a></td>
          <td><code>${escapeHtml(vl.var_name || ('v' + vl.var_id))}</code></td>
          <td>${funLink}</td>
          <td>${vl.contour_id >= 0 ? `<a href="#" onclick="jumpToPassContour(${passNum}, ${vl.contour_id}); return false;">ES#${vl.contour_id}</a>` : '-'}</td>
          <td>${typesBadges || '-'}</td>
        `;
        resViolTbody.appendChild(tr);
      });
    } else {
      resViolSec.style.display = 'none';
    }
  }

  // EntrySets table with Pass-to-Pass Diff
  const essTbody = document.querySelector('#pass-ess-table tbody');
  const essDiffSummary = document.getElementById('pass-ess-diff-summary');
  essTbody.innerHTML = '';
  const baseEsMap = new Map();
  if (hasFullDiff && baseData.entry_sets) {
    baseData.entry_sets.forEach(es => baseEsMap.set(es.id, es));
  }
  const currEsMap = new Map();
  (passData && passData.entry_sets || []).forEach(es => currEsMap.set(es.id, es));

  let newEsCount = 0, modEsCount = 0;
  (passData && passData.entry_sets || []).forEach(es => {
    const tr = document.createElement('tr');
    let diffStatus = 'unchanged';
    let diffBadgeHtml = '';
    let argsHtml = '';
    let retsHtml = '';

    if (hasFullDiff) {
      const baseEs = baseEsMap.get(es.id);
      if (!baseEs) {
        diffStatus = 'new';
        newEsCount++;
        tr.className = 'diff-row-added';
        diffBadgeHtml = `<span class="badge diff-badge-new" style="margin-right:0.35rem;">NEW SPLIT</span>`;
        const parentEs = baseEsMap.get(es.split_origin) || currEsMap.get(es.split_origin);
        const parentArgMap = new Map((parentEs && parentEs.args || []).map(a => [a.var_name, formatEsArgType(a)]));
        argsHtml = (es.args || []).map(a => {
          const curT = formatEsArgType(a);
          const parT = parentArgMap.get(a.var_name);
          if (parT && parT !== curT) {
            return `${escapeHtml(a.var_name)}: <span class="diff-old">${parT}</span> &rarr; <span class="diff-new">${curT}</span>`;
          }
          return `${escapeHtml(a.var_name)}: ${curT}`;
        }).join(', ');
        retsHtml = (es.rets || []).map(r => formatEsRetType(r)).join(', ');
      } else {
        const baseArgMap = new Map((baseEs.args || []).map(a => [a.var_name, formatEsArgType(a)]));
        let anyChanged = (baseEs.can_raise !== es.can_raise);
        argsHtml = (es.args || []).map(a => {
          const curT = formatEsArgType(a);
          const oldT = baseArgMap.get(a.var_name);
          if (oldT !== undefined && oldT !== curT) {
            anyChanged = true;
            return `${escapeHtml(a.var_name)}: <span class="diff-old">${oldT}</span> &rarr; <span class="diff-new">${curT}</span>`;
          }
          return `${escapeHtml(a.var_name)}: ${curT}`;
        }).join(', ');

        const baseRets = (baseEs.rets || []).map(r => formatEsRetType(r));
        retsHtml = (es.rets || []).map((r, idx) => {
          const curR = formatEsRetType(r);
          const oldR = baseRets[idx];
          if (oldR !== undefined && oldR !== curR) {
            anyChanged = true;
            return `<span class="diff-old">${oldR}</span> &rarr; <span class="diff-new">${curR}</span>`;
          }
          return curR;
        }).join(', ');

        if (anyChanged) {
          diffStatus = 'changed';
          modEsCount++;
          tr.className = 'diff-row-changed';
          diffBadgeHtml = `<span class="badge diff-badge-mod" style="margin-right:0.35rem;">MODIFIED</span>`;
        }
      }
    } else {
      argsHtml = (es.args || []).map(a => `${escapeHtml(a.var_name)}: ${formatEsArgType(a)}`).join(', ');
      retsHtml = (es.rets || []).map(r => formatEsRetType(r)).join(', ');
    }

    tr.dataset.diffStatus = diffStatus;
    tr.innerHTML = `
      <td>${es.id}</td>
      <td><a href="#" onclick="jumpToCFG(${es.fun_id}, ${es.id}, ${passNum}); return false;" style="color:var(--primary); font-weight:600; text-decoration:underline;">${escapeHtml(es.fun_name)}</a></td>
      <td>${argsHtml || '-'}</td>
      <td>${retsHtml || '-'}</td>
      <td>${es.can_raise ? '<span class="badge badge-dead">RAISES</span>' : '<span class="badge badge-live">SAFE</span>'}</td>
      <td>${diffBadgeHtml}${es.split_origin >= 0 ? `Split from <a href="#" onclick="jumpToPassContour(${passNum}, ${es.split_origin}); return false;" style="color:var(--primary); text-decoration:underline;">ES#${es.split_origin}</a>` : 'Original'} <button class="btn-sm" style="padding:0.1rem 0.35rem; font-size:0.7rem; margin-left:0.25rem;" onclick="jumpToCFG(${es.fun_id}, ${es.id}, ${passNum})">🔀 CFG</button></td>
    `;
    essTbody.appendChild(tr);
  });
  if (essDiffSummary) {
    essDiffSummary.textContent = hasFullDiff
      ? `(vs Pass ${basePassNum}: +${newEsCount} new splits, ${modEsCount} modified)`
      : '';
  }

  // CreationSets table with Pass-to-Pass Diff
  const cssTbody = document.querySelector('#pass-css-table tbody');
  const cssDiffSummary = document.getElementById('pass-css-diff-summary');
  cssTbody.innerHTML = '';
  const baseCsMap = new Map();
  if (hasFullDiff && baseData.creation_sets) {
    baseData.creation_sets.forEach(cs => baseCsMap.set(cs.id, cs));
  }
  const currCsMap = new Map();
  (passData && passData.creation_sets || []).forEach(cs => currCsMap.set(cs.id, cs));

  let newCsCount = 0, modCsCount = 0;
  (passData && passData.creation_sets || []).forEach(cs => {
    const tr = document.createElement('tr');
    let diffStatus = 'unchanged';
    let diffBadgeHtml = '';
    let fldsHtml = '';
    const arityStr = cs.static_arity >= 0 ? String(cs.static_arity) : (cs.no_static_arity ? 'Dynamic' : '-');
    let arityHtml = arityStr;

    if (hasFullDiff) {
      const baseCs = baseCsMap.get(cs.id);
      if (!baseCs) {
        diffStatus = 'new';
        newCsCount++;
        tr.className = 'diff-row-added';
        diffBadgeHtml = `<span class="badge diff-badge-new" style="margin-right:0.35rem;">NEW SPLIT</span>`;
        const parentCs = baseCsMap.get(cs.split_origin) || currCsMap.get(cs.split_origin);
        const parentFldMap = new Map((parentCs && parentCs.fields || []).map(f => [f.name || ('f' + f.index), formatCsFieldType(f)]));
        fldsHtml = (cs.fields || []).map(f => {
          const fname = f.name || ('f' + f.index);
          const curF = formatCsFieldType(f);
          const parF = parentFldMap.get(fname);
          if (parF && parF !== curF) {
            return `${escapeHtml(fname)}: <span class="diff-old">${parF}</span> &rarr; <span class="diff-new">${curF}</span>`;
          }
          return `${escapeHtml(fname)}: ${curF}`;
        }).join(', ');
      } else {
        const baseArityStr = baseCs.static_arity >= 0 ? String(baseCs.static_arity) : (baseCs.no_static_arity ? 'Dynamic' : '-');
        let anyChanged = false;
        if (baseArityStr !== arityStr) {
          anyChanged = true;
          arityHtml = `<span class="diff-old">${baseArityStr}</span> &rarr; <span class="diff-new">${arityStr}</span>`;
        }
        const baseFldMap = new Map((baseCs.fields || []).map(f => [f.name || ('f' + f.index), formatCsFieldType(f)]));
        fldsHtml = (cs.fields || []).map(f => {
          const fname = f.name || ('f' + f.index);
          const curF = formatCsFieldType(f);
          const oldF = baseFldMap.get(fname);
          if (oldF !== undefined && oldF !== curF) {
            anyChanged = true;
            return `${escapeHtml(fname)}: <span class="diff-old">${oldF}</span> &rarr; <span class="diff-new">${curF}</span>`;
          } else if (oldF === undefined && (baseCs.fields || []).length > 0) {
            anyChanged = true;
            return `${escapeHtml(fname)}: <span class="diff-new">${curF} (new)</span>`;
          }
          return `${escapeHtml(fname)}: ${curF}`;
        }).join(', ');

        if (anyChanged) {
          diffStatus = 'changed';
          modCsCount++;
          tr.className = 'diff-row-changed';
          diffBadgeHtml = `<span class="badge diff-badge-mod" style="margin-right:0.35rem;">MODIFIED</span>`;
        }
      }
    } else {
      fldsHtml = (cs.fields || []).map(f => `${escapeHtml(f.name || 'f'+f.index)}: ${formatCsFieldType(f)}`).join(', ');
    }

    tr.dataset.diffStatus = diffStatus;
    tr.innerHTML = `
      <td>${cs.id}</td>
      <td><strong>${escapeHtml(cs.sym)}</strong></td>
      <td>${arityHtml}</td>
      <td>${fldsHtml || '-'}</td>
      <td>${diffBadgeHtml}${cs.split_origin >= 0 ? `Split from <a href="#" onclick="jumpToPassContour(${passNum}, null, ${cs.split_origin}); return false;" style="color:#c084fc; text-decoration:underline;">CS#${cs.split_origin}</a>` : 'Original'}</td>
    `;
    cssTbody.appendChild(tr);
  });
  if (cssDiffSummary) {
    cssDiffSummary.textContent = hasFullDiff
      ? `(vs Pass ${basePassNum}: +${newCsCount} new splits, ${modCsCount} modified)`
      : '';
  }

  // AVars with Converged Type Changes in this Pass
  const avarDiffSec = document.getElementById('pass-avar-diff-section');
  const avarDiffTbody = document.querySelector('#pass-avar-diff-table tbody');
  const avarDiffSummary = document.getElementById('pass-avar-diff-summary');
  if (avarDiffSec && avarDiffTbody) {
    avarDiffTbody.innerHTML = '';
    if (hasFullDiff && passData.avars && baseData._avarById) {
      const funMap = new Map((DATA.phase2_cfg.functions || []).map(f => [f.id, f.name]));
      const changedAvars = [];
      passData.avars.forEach(av => {
        const newT = formatAvarTypeSet(av);
        const baseAv = baseData._avarById.get(av.id);
        if (!baseAv) {
          if (newT !== 'void') {
            changedAvars.push({ av, kind: 'NEW CONTOUR AVAR', badgeCls: 'diff-badge-new', oldT: null, newT, priority: 2 });
          }
        } else {
          const oldT = formatAvarTypeSet(baseAv);
          if (oldT !== newT) {
            const oldCount = (baseAv.type || []).length;
            const newCount = (av.type || []).length;
            let kind = 'TYPE CHANGED';
            let badgeCls = 'diff-badge-mod';
            if (oldCount > newCount && newCount > 0) {
              kind = 'NARROWED';
              badgeCls = 'diff-badge-resolved';
            } else if (newCount > oldCount) {
              kind = 'WIDENED';
              badgeCls = 'diff-badge-mod';
            }
            changedAvars.push({ av, kind, badgeCls, oldT, newT, priority: 1 });
          }
        }
      });

      changedAvars.sort((a, b) => a.priority - b.priority || a.av.id - b.av.id);
      if (changedAvars.length > 0) {
        avarDiffSec.style.display = 'block';
        const shown = changedAvars.slice(0, 300);
        if (avarDiffSummary) {
          avarDiffSummary.textContent = changedAvars.length > 300
            ? `(vs Pass ${basePassNum}: showing 300 of ${changedAvars.length} changed AVars)`
            : `(vs Pass ${basePassNum}: ${changedAvars.length} changed AVars)`;
        }
        const frag = document.createDocumentFragment();
        shown.forEach(item => {
          const av = item.av;
          const tr = document.createElement('tr');
          tr.dataset.diffStatus = 'changed';
          tr.className = item.priority === 1 ? 'diff-row-changed' : 'diff-row-added';
          const funName = av.fun_name || funMap.get(av.fun_id) || (av.fun_id >= 0 ? `fun #${av.fun_id}` : 'global');
          const transitionHtml = item.oldT !== null
            ? `<span class="diff-old">${escapeHtml(item.oldT)}</span> &rarr; <span class="diff-new">${escapeHtml(item.newT)}</span>`
            : `<span class="diff-new">${escapeHtml(item.newT)}</span>`;
          tr.innerHTML = `
            <td><a href="#" onclick="jumpToDataflow(${av.id}, ${passNum}); return false;"><strong>#${av.id}</strong></a></td>
            <td><code>${escapeHtml(av.var_name || ('v' + av.var_id))}</code></td>
            <td>${av.fun_id >= 0 ? `<a href="#" onclick="jumpToCFG(${av.fun_id}, ${av.contour_id}, ${passNum}); return false;" style="color:var(--primary); text-decoration:underline;">${escapeHtml(funName)}</a>` : escapeHtml(funName)}</td>
            <td>${av.contour_id >= 0 ? `<a href="#" onclick="jumpToPassContour(${passNum}, ${av.contour_id}); return false;">ES#${av.contour_id}</a>` : '-'}</td>
            <td><span class="badge ${item.badgeCls}">${item.kind}</span></td>
            <td><code>${transitionHtml}</code></td>
          `;
          frag.appendChild(tr);
        });
        avarDiffTbody.appendChild(frag);
      } else {
        avarDiffSec.style.display = 'none';
      }
    } else {
      avarDiffSec.style.display = 'none';
    }
  }

  applyPassFilter();
}

document.getElementById('btn-prev-pass').addEventListener('click', () => {
  if (currentPassIndex > 0) {
    currentPassIndex--;
    displayPass(currentPassIndex + 1);
  }
});

document.getElementById('btn-next-pass').addEventListener('click', () => {
  if (currentPassIndex < totalPasses - 1) {
    currentPassIndex++;
    displayPass(currentPassIndex + 1);
  }
});

passSlider.addEventListener('input', () => {
  currentPassIndex = parseInt(passSlider.value) - 1;
  displayPass(currentPassIndex + 1);
});

if (totalPasses > 0) {
  currentPassIndex = totalPasses - 1;
  displayPass(totalPasses);
}

// Tab: Dataflow Tracer
let currentDfPass = totalPasses;
let currentDfData = null;

function renderDataflowTable(filterText, highlightAvId) {
  const tbody = document.querySelector('#df-table tbody');
  const countEl = document.getElementById('df-count');
  if (!tbody) return;

  if (!currentDfData || !currentDfData.avars) {
    loadPassData(currentDfPass, (passData) => {
      if (passData && passData.avars) {
        currentDfData = passData;
        renderDataflowTable(filterText, highlightAvId);
      }
    });
    tbody.innerHTML = '<tr><td colspan="7" style="text-align:center; padding:1.5rem; color:var(--text-muted);">Loading AVar data...</td></tr>';
    if (countEl) countEl.textContent = '';
    return;
  }

  const allAvars = currentDfData.avars;
  const searchInput = document.getElementById('df-search');
  const qRaw = filterText !== undefined ? filterText : (searchInput ? searchInput.value : '');
  const q = qRaw.trim();
  const funMap = new Map((DATA.phase2_cfg.functions || []).map(f => [f.id, f.name]));

  let avarsToShow = [];
  if (!q) {
    avarsToShow = allAvars.slice(0, 500);
    if (countEl) {
      countEl.textContent = allAvars.length > 500
        ? `Showing 500 of ${allAvars.length} AVars (type in search box to search all variables)`
        : `Showing all ${allAvars.length} AVars`;
    }
  } else {
    const qLower = q.toLowerCase();
    const cleanQ = q.startsWith('#') ? q.slice(1) : q;
    const qNum = parseInt(cleanQ, 10);
    const isNumeric = !isNaN(qNum) && String(qNum) === cleanQ;

    const scored = [];
    for (let i = 0; i < allAvars.length; i++) {
      const av = allAvars[i];
      let score = 0;
      const vName = (av.var_name || '').toLowerCase();
      const fName = (av.fun_name || funMap.get(av.fun_id) || '').toLowerCase();
      const typeStr = (av.type || []).map(t => t.sym + (t.is_const ? `(${t.val})` : '')).join(' | ').toLowerCase();
      const varIdStr = 'v' + av.var_id;

      if (isNumeric && av.id === qNum) {
        score = 1000;
      } else if (vName === qLower) {
        score = 500;
      } else if (varIdStr === qLower) {
        score = 450;
      } else if (vName.startsWith(qLower)) {
        score = 300;
      } else if (vName.includes(qLower)) {
        score = 200;
      } else if (String(av.id).includes(cleanQ)) {
        score = 150;
      } else if (String(av.var_id).includes(cleanQ)) {
        score = 120;
      } else if (fName === qLower) {
        score = 110;
      } else if (fName.includes(qLower)) {
        score = 90;
      } else if (typeStr.includes(qLower)) {
        score = 70;
      }

      if (score > 0) {
        scored.push({ av, score });
      }
    }

    scored.sort((a, b) => b.score - a.score || a.av.id - b.av.id);
    const totalMatches = scored.length;
    avarsToShow = scored.slice(0, 500).map(s => s.av);
    if (countEl) {
      countEl.textContent = `Found ${totalMatches} matching AVar(s) for "${q}" (showing ${avarsToShow.length})`;
    }
  }

  tbody.innerHTML = '';
  if (avarsToShow.length === 0) {
    tbody.innerHTML = `<tr><td colspan="7" style="text-align:center; padding:1.5rem; color:var(--text-muted);">No AVars found matching "${escapeHtml(q)}"</td></tr>`;
    return;
  }

  const fragment = document.createDocumentFragment();
  avarsToShow.forEach(av => {
    const tr = document.createElement('tr');
    if (highlightAvId && av.id === highlightAvId) {
      tr.style.background = 'rgba(16, 185, 129, 0.15)';
      tr.style.outline = '2px solid var(--accent-green)';
    }
    const typeStr = (av.type || []).map(t => t.sym + (t.is_const ? `(${t.val})` : '')).join(' | ');
    const funName = av.fun_name || funMap.get(av.fun_id) || ('fun #' + av.fun_id);
    tr.innerHTML = `
      <td><strong>#${av.id}</strong></td>
      <td><a href="#" onclick="jumpToCFG(${av.fun_id}, ${av.contour_id}, ${currentDfPass}); return false;" style="color:var(--accent-green); font-weight:600; text-decoration:underline;" title="View in CFG">${escapeHtml(av.var_name || 'v'+av.var_id)}</a></td>
      <td><a href="#" onclick="jumpToCFG(${av.fun_id}, ${av.contour_id}, ${currentDfPass}); return false;" style="color:var(--text-main); text-decoration:underline;">${escapeHtml(funName)}</a></td>
      <td><a href="#" onclick="jumpToPassContour(${currentDfPass}, ${av.contour_id}); return false;" style="color:var(--primary); text-decoration:underline;">contour #${av.contour_id}</a></td>
      <td><code>${escapeHtml(typeStr || 'void')}</code></td>
      <td>${(av.backward || []).map(id => `<a href="#" onclick="jumpToDataflow(${id}, ${currentDfPass}); return false;">#${id}</a>`).join(', ') || '-'}</td>
      <td>${(av.forward || []).map(id => `<a href="#" onclick="jumpToDataflow(${id}, ${currentDfPass}); return false;">#${id}</a>`).join(', ') || '-'}</td>
    `;
    fragment.appendChild(tr);
  });
  tbody.appendChild(fragment);
}

function initDataflow() {
  const pSel = document.getElementById('df-pass-select');
  if (pSel) {
    pSel.innerHTML = '';
    for (let p = 1; p <= totalPasses; p++) {
      const opt = document.createElement('option');
      opt.value = p;
      opt.textContent = p === totalPasses ? `Pass ${p} (Converged)` : `Pass ${p}`;
      if (p === totalPasses) opt.selected = true;
      pSel.appendChild(opt);
    }
    pSel.addEventListener('change', () => {
      currentDfPass = parseInt(pSel.value);
      loadPassData(currentDfPass, (passData) => {
        currentDfData = passData;
        renderDataflowTable();
      });
    });
  }

  loadPassData(totalPasses, (lastPassData) => {
    currentDfData = lastPassData;
    renderDataflowTable();
  });
}
initDataflow();

const dfSearchInput = document.getElementById('df-search');
if (dfSearchInput) {
  dfSearchInput.addEventListener('input', () => {
    renderDataflowTable();
  });
}

// Tab: Transfer Functions
let allTransferRows = [];

function buildTransferFunctionCatalog() {
  allTransferRows = [];
  (DATA.phase2_cfg.functions || []).forEach(f => {
    (f.pnodes || []).forEach(pn => {
      if (pn.prim) {
        const searchStr = `${pn.id} ${f.name} ${pn.prim} ${(pn.rvals||[]).map(v => (v.name||'') + ' ' + (v.type||'')).join(' ')} ${(pn.lvals||[]).map(v => (v.name||'') + ' ' + (v.type||'')).join(' ')}`.toLowerCase();
        allTransferRows.push({ f, pn, searchStr });
      }
    });
  });
}

function renderTransferFunctions(filterQuery) {
  const tbody = document.querySelector('#tf-table tbody');
  const countEl = document.getElementById('tf-count');
  if (!tbody) return;
  const q = (filterQuery || '').trim().toLowerCase();
  const matched = q ? allTransferRows.filter(item => item.searchStr.includes(q)) : allTransferRows;
  const shown = matched.slice(0, 500);
  if (countEl) {
    countEl.textContent = matched.length > 500
      ? `Showing 500 of ${matched.length} primitive operations`
      : `Showing ${matched.length} primitive operations`;
  }
  tbody.innerHTML = '';
  const frag = document.createDocumentFragment();
  shown.forEach(({ f, pn }) => {
    const tr = document.createElement('tr');
    let statusBadge = '<span class="badge badge-live">ACTIVE</span>';
    if (!pn.live && !pn.fa_live) statusBadge = '<span class="badge" style="background:#475569; color:#cbd5e1; font-size:0.65rem;">DEAD</span>';
    else if (!pn.live && pn.fa_live) statusBadge = '<span class="badge" style="background:#334155; color:#94a3b8; font-size:0.65rem;">FOLDED</span>';
    const rvalsHtml = (pn.rvals || []).map(v =>
      `<span class="badge badge-var" onclick="jumpToCFG(${f.id}); showVarInspector(${f.id}, ${v.id});">${escapeHtml(v.name || 'v'+v.id)}</span> <span class="badge badge-type">${escapeHtml(v.type || 'void')}</span>`
    ).join(', ') || '-';
    const lvalsHtml = (pn.lvals || []).map(v =>
      `<span class="badge badge-var" onclick="jumpToCFG(${f.id}); showVarInspector(${f.id}, ${v.id});">${escapeHtml(v.name || 'v'+v.id)}</span> <span class="badge badge-type">${escapeHtml(v.type || 'void')}</span>`
    ).join(', ') || '-';
    tr.innerHTML = `
      <td><a href="#" onclick="jumpToCFGAndNode(${f.id}, ${pn.id}); return false;" style="color:var(--primary); font-weight:600; text-decoration:underline;">#${pn.id}</a></td>
      <td><a href="#" onclick="jumpToCFG(${f.id}); return false;" style="color:var(--text-main); text-decoration:underline;">${escapeHtml(f.name)}</a></td>
      <td><span class="badge badge-prim">${escapeHtml(pn.prim)}</span></td>
      <td>${rvalsHtml}</td>
      <td>${lvalsHtml}</td>
      <td>${statusBadge}</td>
    `;
    frag.appendChild(tr);
  });
  tbody.appendChild(frag);
}

buildTransferFunctionCatalog();
renderTransferFunctions('');

const tfSearchInput = document.getElementById('tf-search');
if (tfSearchInput) {
  tfSearchInput.addEventListener('input', (e) => {
    renderTransferFunctions(e.target.value);
  });
}

// Tab: Clones & Memory Layouts
function initMemoryLayouts() {
  // 1. Function Clones & Specializations Table
  const clonesTbody = document.querySelector('#clones-table tbody');
  if (clonesTbody) {
    clonesTbody.innerHTML = '';
    const fclones = DATA.phase4_cloning.function_clones || [];
    if (fclones.length === 0) {
      clonesTbody.innerHTML = '<tr><td colspan="8" style="text-align:center; color:var(--text-muted); padding:1rem;">No function clones recorded.</td></tr>';
    } else {
      fclones.forEach(fc => {
        const tr = document.createElement('tr');
        const formalsStr = (fc.formals || []).map(f => `${escapeHtml(f.name)}: <code>${escapeHtml(f.type)}</code>`).join(', ') || '()';
        const retsStr = (fc.returns || []).map(r => `<code>${escapeHtml(r.type)}</code>`).join(', ') || 'void';
        const loc = fc.line > 0
          ? `<a href="#" onclick="jumpToSource(${fc.line}); return false;">${escapeHtml(fc.filename ? `${fc.filename}:${fc.line}` : `line ${fc.line}`)}</a>`
          : (fc.filename ? escapeHtml(fc.filename) : '-');
        const kindBadge = fc.is_clone 
          ? `<span class="badge badge-kind">CLONE of #${fc.wraps_id}</span>` 
          : `<span class="badge badge-live">ORIGINAL</span>`;
        const essBadges = (fc.entry_sets || []).map(cid => `<span class="badge badge-contour" onclick="jumpToPassContour(null, ${cid})">ES#${cid}</span>`).join(' ') || '-';
        const cgBtn = (fc.cg_start_line && fc.cg_start_line > 0)
          ? ` <button class="btn-sm" style="padding:0.15rem 0.4rem; font-size:0.75rem;" onclick="jumpToCodegen(${fc.cg_start_line}, ${fc.cg_end_line}, ${fc.id})" title="${escapeHtml(fc.cg_name)} (L${fc.cg_start_line}–${fc.cg_end_line})">⚙️ ${escapeHtml((cgData && cgData.language) || 'Code')}</button>`
          : '';

        tr.innerHTML = `
          <td>${fc.id}</td>
          <td><a href="#" onclick="jumpToCFG(${fc.id}); return false;" style="color:var(--primary); font-weight:600; text-decoration:underline;">${escapeHtml(fc.name)}</a></td>
          <td>${kindBadge}</td>
          <td style="color:var(--text-muted); font-size:0.8rem;">${loc}</td>
          <td>${essBadges}</td>
          <td>${formalsStr}</td>
          <td>${retsStr}</td>
          <td style="white-space:nowrap;"><button class="btn-sm" style="padding:0.15rem 0.4rem; font-size:0.75rem;" onclick="jumpToCFG(${fc.id})">🔀 CFG</button>${cgBtn}</td>
        `;
        clonesTbody.appendChild(tr);
      });
    }
  }

  // 2. Concrete Types Memory Layouts
  const container = document.getElementById('memory-layouts-container');
  if (container) {
    container.innerHTML = '';
    const types = DATA.phase4_cloning.concrete_types || [];
    if (types.length === 0) {
      container.innerHTML = '<p style="color:var(--text-muted); padding:1rem;">No composite concrete records allocated.</p>';
    } else {
      types.forEach(t => {
        const card = document.createElement('div');
        card.className = 'card memory-type-card';
        card.style.marginBottom = '1.25rem';
        let memBar = '<div class="memory-bar">';
        let curOff = 0;
        (t.fields || []).forEach(f => {
          if (f.offset > curOff) {
            const padBytes = f.offset - curOff;
            memBar += `<div class="memory-chunk padding" style="flex:${Math.max(1, padBytes)};" title="Alignment Padding (${padBytes}B @ offset ${curOff})">pad<br><span style="font-size:0.65rem;">${padBytes}B</span></div>`;
          }
          memBar += `<div class="memory-chunk field" style="flex:${Math.max(1, f.size)};" title="${escapeHtml(f.name)}: ${escapeHtml(f.type)} (${f.size}B @ offset ${f.offset})">${escapeHtml(f.name)}<br><span style="font-size:0.65rem;">${f.size}B</span></div>`;
          curOff = f.offset + f.size;
        });
        if (t.size > curOff && (t.fields || []).length > 0) {
          const tailPad = t.size - curOff;
          memBar += `<div class="memory-chunk padding" style="flex:${Math.max(1, tailPad)};" title="Tail Alignment Padding (${tailPad}B @ offset ${curOff})">pad<br><span style="font-size:0.65rem;">${tailPad}B</span></div>`;
        } else if ((t.fields || []).length === 0) {
          memBar += `<div class="memory-chunk padding" style="flex:1;">${t.element_type ? `Container Element: ${escapeHtml(t.element_type)}` : 'No live instance fields'}</div>`;
        }
        memBar += '</div>';

        const elemBadge = t.element_type ? ` <span class="badge badge-type">elem: ${escapeHtml(t.element_type)}</span>` : '';
        const elidedBadge = t.elided_slots > 0 ? ` <span class="badge badge-live">${t.elided_slots} elided slot(s)</span>` : '';

        card.innerHTML = `
          <div style="display:flex; justify-content:space-between; align-items:center; flex-wrap:wrap; gap:0.5rem;">
            <h4>${escapeHtml(t.name)} (id: ${t.id})${elemBadge}${elidedBadge}</h4>
            <span class="badge badge-kind">${escapeHtml(t.type_kind)} | Total: ${t.size} bytes | Align: ${t.alignment}</span>
          </div>
          ${memBar}
          <div style="margin-top:0.75rem; font-size:0.8rem; color:var(--text-muted);">
            Fields: ${(t.fields || []).map(f => `<strong>${escapeHtml(f.name)}</strong> (<code>${escapeHtml(f.type)}</code>, ${f.size}B @ offset ${f.offset})`).join(', ') || 'none'}
          </div>
        `;
        container.appendChild(card);
      });
    }
  }

  // 3. Search / Filter Input
  const searchInput = document.getElementById('memory-search');
  if (searchInput) {
    searchInput.addEventListener('input', () => {
      const q = searchInput.value.trim().toLowerCase();
      document.querySelectorAll('#clones-table tbody tr').forEach(tr => {
        tr.style.display = (!q || tr.textContent.toLowerCase().includes(q)) ? '' : 'none';
      });
      document.querySelectorAll('.memory-type-card').forEach(card => {
        card.style.display = (!q || card.textContent.toLowerCase().includes(q)) ? '' : 'none';
      });
    });
  }
}
initMemoryLayouts();

