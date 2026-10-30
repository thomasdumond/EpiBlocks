# EpiBlocks

**Build compartmental epidemic models by drawing them.**

EpiBlocks is an R Shiny app for teaching compartmental ODE models (SI, SIR,
SEIR, SIS, SLADR, …) in veterinary and aquatic animal health epidemiology.
Users draw boxes (compartments) and arrows (flows), give each arrow a rate,
and the app writes the differential equations, finds the parameters, solves
the model and plots the epicurve. No coding needed.

**▶ Try it online: <https://thomasdumond-epiblocks.share.connect.posit.cloud/>**
(the first visit after a quiet period can take 30–60 s while the app wakes up)

> Beta version: tested by colleagues first, then used in a workshop.
> Feedback is very welcome.

---

## 1. Run it on your computer

You need **R ≥ 4.3** and, ideally, RStudio.

1. Download or clone this folder.
2. Double-click `EpiBlocks.Rproj` to open the project in RStudio.
3. The first time only, install the exact package versions used by the app:
   ```r
   renv::restore()
   ```
   (If renv is not installed: `install.packages("renv")` first.)
4. Start the app:
   ```r
   shiny::runApp()
   ```
   or open `app.R` and click **Run App**.

### Run the tests

```r
renv::install("testthat")   # once; tests are not needed to run the app
testthat::test_dir("tests/testthat")
```

The tests check the formula reader (unsafe or broken formulas are rejected),
the ODE builder (the SIR template matches a textbook SIR solved directly with
deSolve), the base-R fallback solver, the templates, save/load and the LaTeX
output.

---

## 2. Put it online

### Option A — Posit Connect Cloud (recommended, free)

Connect Cloud runs regular R, so everything works, including deSolve.

1. Put this folder in a **public GitHub repository** (the free plan deploys
   from public repositories).
2. Connect Cloud reads the package list and R version from **`renv.lock`**,
   which is already in the repository, so no extra file is needed. Do **not**
   add a `manifest.json`: if both exist, Connect Cloud uses `manifest.json`
   and ignores `renv.lock`.
3. Go to <https://connect.posit.cloud>, sign in with GitHub, choose
   **Publish → Shiny**, select the repository, the branch and `app.R`, then
   **Publish**.
4. Share the link. Every push to GitHub can redeploy the app.

After adding or updating a package, run `renv::snapshot()` and push the
updated `renv.lock`.

The app uses no local file paths and needs no secrets.

### Option B — shinylive on GitHub Pages (runs entirely in the browser)

shinylive runs R inside the browser with webR, so no server is needed. Checked
against the webR package repository:

| Package    | Available in webR? |
|------------|--------------------|
| shiny, bslib, visNetwork, plotly, DT, jsonlite, htmlwidgets | yes |
| **deSolve** | **no** (it contains Fortran code) |

EpiBlocks handles this automatically: when deSolve is missing it uses a
built-in Runge–Kutta 4 solver written in base R (see `R/solver.R`). The tests
check that both solvers agree. So a shinylive version should work, but it has
**not been tested yet**. It will also be slower to start (the browser
downloads R and the packages, ~30–60 s the first time) and the RK4 solver can
struggle with very stiff models (very fast rates). To try it:

```r
install.packages("shinylive")
shinylive::export(".", "site")
httpuv::runStaticServer("site")   # test locally, then publish "site" with GitHub Pages
```

---

## 3. User guide (for workshop participants)

### The screen

- **Left, top — Model diagram.** Your model as boxes and arrows.
- **Left, bottom — Tabs.** *Epicurve* (the plot), *Equations* (the ODEs),
  *Table* (numbers, CSV download) and *Help*.
- **Right — Editor.** Shows what you clicked (a box or an arrow), the
  **Parameters** and the **Simulation settings** (end time, time step).

### Start

Choose a model in the **Template** menu and press **Load**, or press **New**
for an empty diagram. Loading a template shows a short description and a
reference.

### Add a compartment (box)

Click **Add compartment** (top of the diagram), then click on an empty space.
Click the new box to:

- give it a **name** and press **Rename** (letters, digits and `_`, starting
  with a letter; `N` and `t` are reserved). Every formula using the old name
  is updated for you;
- set its **initial value** (number of animals at time 0);
- tick or untick **Counts towards N** (N = total living population; untick
  it for dead animals or carcasses).

### Draw a flow (arrow)

Click **Draw flow**, then press on a box and drag to another box.

- Draw **from the Source** for births or other inflows.
- Draw **into the Sink** for deaths, culling or removal.

### Set the rate of a flow

Click an arrow and choose its **type**. A formula is filled in for you:

| Type | Formula |
|------|---------|
| Infection (frequency dependent) | `beta * S * I / N` |
| Infection (density dependent)   | `beta * S * I` |
| Progression / recovery / death  | `gamma * X` (X = the box the arrow leaves) |
| Constant inflow                 | `lambda` |
| Custom formula                  | anything you type |

For infections, tick which boxes are **infectious** (several are allowed, e.g.
acute *and* dead shrimp). You can always edit the formula. A formula may
contain numbers, box names, parameter names, `N`, `t`, `+ - * / ^`, brackets
and `exp()`, `log()`, `sqrt()`, `min()`, `max()`.

Any other name you type (e.g. `theta`) becomes a **parameter**: it appears
on the right with a default value that you can change.

### Explore

- Change parameter values: the epicurve updates straight away.
- Tick/untick boxes above the plot to show or hide them.
- **Equations** shows the model as dN/dt = flows in − flows out.
- **Table** → **Download CSV** for the numbers.

### Keep your work

**Save** downloads your model as a `.json` file. **Open** loads it again.

### When something is wrong

The app explains the problem above the plot instead of crashing, for example:
a formula it cannot read, a box without an initial value, or values going
negative. A frequent one: **an infection needs at least one infectious animal
at time 0**, otherwise the curves stay flat.

---

## 4. Templates

| Template | Compartments | Notes |
|----------|-------------|-------|
| SIR  | S, I, R | R0 = beta / gamma |
| SI   | S, I    | lifelong infection |
| SIS  | S, I    | no immunity, endemic equilibrium |
| SEIR | S, E, I, R | latent period 1 / sigma |
| SLADR | S, L, A, D, R | white spot disease in shrimp |

**SLADR** is adapted from Lotz JM, Soto MA (2002) *Model of white spot
syndrome virus (WSSV) epidemics in Litopenaeus vannamei.* Dis Aquat Org
50:199–209. Transmission comes from acute (A) and dead (D) shrimp,
sick shrimp are eaten (c_A), and carcasses decay or are eaten (delta, c_D).
**The parameter values are illustrative** and should be checked against the
paper or replaced with your own data.

To add a template, copy a block in `R/templates.R` and add it to `TEMPLATES`
at the bottom of that file.

---

## 5. For developers

```
EpiBlocks/
├── app.R               page layout + server wiring
├── R/                  loaded automatically by Shiny
│   ├── expr_safe.R     reads formulas safely (parse tree whitelist, never eval of raw text)
│   ├── model.R         how a model is stored; flow types and default formulas
│   ├── ode_build.R     boxes + arrows -> ODEs (stoichiometry matrix); checks
│   ├── solver.R        deSolve (lsoda), or base-R RK4 fallback; friendly errors
│   ├── latex.R         equations as LaTeX for MathJax
│   ├── model_io.R      templates and saved files share one "spec" format (JSON)
│   ├── templates.R     the ready-made models
│   ├── canvas.R        the editable diagram (visNetwork + small JS handlers)
│   ├── help.R          the Help tab
│   └── server_*.R      server logic: canvas, editor, outputs, files
├── www/mathjax/        MathJax 4 (Apache 2.0), bundled so equations render
│                       offline and behind proxies (copied from the mathjaxr package)
├── tests/testthat/     unit tests
├── renv.lock           exact package versions
└── EpiBlocks.Rproj
```

**How formulas stay safe.** User text goes through `parse()` (which runs
nothing), then every element of the parse tree is checked: only numbers,
names, `+ - * / ^ (` and `exp log sqrt min max` are allowed. The checked
formula is then evaluated in an environment that contains only those
functions and whose parent is the empty environment.

**Package versions** are locked with renv (`renv.lock`, R 4.3.1). After
adding a package, run `renv::snapshot()`; Connect Cloud deploys from
`renv.lock` directly.
