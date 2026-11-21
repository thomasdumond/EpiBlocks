# EpiBlocks

**Build compartmental epidemic models by drawing them.**

EpiBlocks is an R Shiny app for teaching compartmental ODE models (SI, SIR,
SEIR, SIS, …) in veterinary and aquatic animal health epidemiology.
Users draw boxes (compartments) and arrows (flows), give each arrow a rate,
and the app writes the differential equations, finds the parameters, solves
the model and plots the epicurve. No coding needed.

**▶ Try it online: <https://shiny.vet.unimelb.edu.au/epi/epi.blocks/>**

> Beta version: tested by colleagues first, then used in a workshop.
> Feedback is very welcome.

---

## 1. Run it on your computer

You need **R ≥ 4.3** and, ideally, RStudio.

1. Download or clone this folder.
2. Double-click `EpiBlocks.Rproj` to open the project in RStudio (if you
   downloaded the ZIP, open `app.R` instead).
3. The first time only, install the packages:
   ```r
   install.packages(c("shiny", "bslib", "visNetwork", "plotly", "DT",
                      "jsonlite", "deSolve"))
   ```
   (deSolve is optional: without it the app uses its built-in solver.)
4. Start the app:
   ```r
   shiny::runApp()
   ```
   or open `app.R` and click **Run App**.

### Run the tests

```r
install.packages("testthat")   # once; tests are not needed to run the app
testthat::test_dir("tests/testthat")
```

The tests check the formula reader (unsafe or broken formulas are rejected),
the ODE builder (the SIR template matches a textbook SIR solved directly with
deSolve), the base-R fallback solver, the templates, save/load and the LaTeX
output.

---

## 2. Put it online

### On a Shiny Server (e.g. a university server)

1. On GitHub, click **Code → Download ZIP** and unzip it. The ZIP contains
   only `app.R`, `R/`, `www/`, `tests/` and `README.md`.
2. Ask the server admin to install the packages once (same list as above):
   ```r
   install.packages(c("shiny", "bslib", "visNetwork", "plotly", "DT",
                      "jsonlite", "deSolve"))
   ```
3. Copy the unzipped folder to the server's app directory (often
   `/srv/shiny-server/epiblocks/`).

To update the app, download a new ZIP and replace the folder. The app uses no
local file paths and needs no secrets. If it does not start, the error is in
the server log (on Shiny Server usually `/var/log/shiny-server/`).

### Alternative: shinylive on GitHub Pages (runs entirely in the browser)

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

Models are **closed** by default: no animal enters or leaves. To open the
model, use the buttons above the diagram:

- **+ Source (births, inflow)**, then draw arrows **from the Source**.
- **+ Sink (deaths, removal)**, then draw arrows **into the Sink**.

Click a Source or Sink to remove it again (its arrows are removed too).

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
acute *and* dead animals). You can always edit the formula. A formula may
contain numbers, box names, parameter names, `N`, `t`, `+ - * / ^`, brackets
and `exp()`, `log()`, `sqrt()`, `min()`, `max()`.

Any other name you type (e.g. `theta`) becomes a **parameter**: it appears
on the right with a default value that you can change.

### Explore

- Change parameter values: the epicurve updates straight away.
- Tick/untick boxes above the plot to show or hide them.
- **Equations** shows the model as dN/dt = flows in − flows out.
- **Table** → **Download CSV** for the numbers.
- **R code** gives the model as a standalone deSolve script (parameters with
  the values currently chosen, initial values, model function, `ode()` call
  and a plot). **Copy** it into R or an R Markdown chunk, or **Download .R**.

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
| SEEEEIIIIR | S, E1–E4, I1–I4, R | periods in 4 stages each; all E stages share sigma, all I stages share gamma |

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
│   ├── rcode.R         the model as a standalone deSolve script ("R code" tab)
│   ├── model_io.R      templates and saved files share one "spec" format (JSON)
│   ├── templates.R     the ready-made models
│   ├── canvas.R        the editable diagram (visNetwork + small JS handlers)
│   ├── help.R          the Help tab
│   └── server_*.R      server logic: canvas, editor, outputs, files
├── www/mathjax/        MathJax 4 (Apache 2.0), bundled so equations render
│                       offline and behind proxies (copied from the mathjaxr package)
├── tests/testthat/     unit tests
└── EpiBlocks.Rproj
```

**How formulas stay safe.** User text goes through `parse()` (which runs
nothing), then every element of the parse tree is checked: only numbers,
names, `+ - * / ^ (` and `exp log sqrt min max` are allowed. The checked
formula is then evaluated in an environment that contains only those
functions and whose parent is the empty environment.

**Tested with** R 4.3.1, shiny 1.14.0, bslib 0.11.0, visNetwork 2.1.4,
plotly 4.12.0, DT 0.34.0, jsonlite 2.0.0 and deSolve 1.42. Newer versions
should work. If you add a package, add it to the `install.packages()` lists
in sections 1 and 2.
