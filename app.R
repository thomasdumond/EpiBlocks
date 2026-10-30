# =============================================================================
# EpiBlocks -- build compartmental epidemic models by drawing them
# =============================================================================
#
# Run locally:  shiny::runApp()
#
# Files in R/ are loaded automatically by Shiny:
#   expr_safe.R      reading rate formulas safely
#   model.R          how a model is stored; flow types
#   ode_build.R      boxes + arrows -> ODE system
#   solver.R         solving the ODEs (deSolve, or a base-R fallback)
#   latex.R          equations as LaTeX
#   model_io.R       templates and saved files (JSON)
#   templates.R      the ready-made models
#   canvas.R         the editable diagram (visNetwork)
#   help.R           the Help tab
#   server_*.R       the server logic, split by panel
# =============================================================================

library(shiny)
library(bslib)
library(visNetwork)
library(plotly)
# deSolve, DT and jsonlite are called with deSolve::, DT:: and jsonlite::

ui <- page_sidebar(
  title = "EpiBlocks — build your own compartmental model",
  theme = bs_theme(version = 5, primary = "#0f6e8c"),
  fillable = FALSE,
  # MathJax is bundled in www/mathjax (copied from the mathjaxr package), so
  # equations render without internet access or a CDN.
  tags$head(
    tags$script(HTML("
      window.MathJax = {
        startup: { typeset: false },
        // The bundled copy has no speech/braille files: turn these features off,
        // otherwise MathJax 4 waits for them for ever
        options: {
          enableEnrichment: false, enableSpeech: false, enableBraille: false,
          enableExplorer: false,
          menuOptions: { settings: { enrich: false, speech: false, braille: false } }
        }
      };
      // Typeset the equations panel once MathJax has finished loading
      function epiTypeset() {
        var el = document.getElementById('equations');
        if (!el) return;
        if (window.MathJax && MathJax.startup && MathJax.startup.promise) {
          MathJax.startup.promise
            .then(function () { MathJax.typesetClear([el]); return MathJax.typesetPromise([el]); })
            .catch(function (err) { console.log('MathJax: ' + err.message); });
        } else {
          setTimeout(epiTypeset, 300);  // MathJax still loading: try again shortly
        }
      }
    ")),
    tags$script(src = "mathjax/tex-chtml.js", async = NA)
  ),
  tags$head(tags$style(HTML("
    .param-grid { display: grid; grid-template-columns: 1fr 1fr; column-gap: .75rem; }
    .param-grid .form-group { margin-bottom: .5rem; }
    /* toolbar above the diagram */
    .model-toolbar .form-group { margin-bottom: 0; }
    .model-toolbar .shiny-input-container { width: auto; }
    .model-toolbar select { padding-top: .25rem; padding-bottom: .25rem; font-size: .875rem; }
    .open-file .input-group > .form-control, .open-file .progress { display: none; }
    .open-file .btn { padding: .25rem .5rem; font-size: .875rem; border-radius: .25rem !important; }
    /* vis-network toolbar: make it look like the rest of the app */
    div.vis-network div.vis-manipulation { background: #f8fafc; border-bottom: 1px solid #e2e8f0; }
    .equations { overflow-x: auto; font-size: 1.05rem; }
    .help-text h5 { margin-top: 1rem; font-size: 1rem; font-weight: 600; }
  "))),

  # --- Right-hand panel: editor, parameters, settings -------------------------
  sidebar = sidebar(
    position = "right", width = 380, open = "always",
    card(card_body(uiOutput("editor"))),
    accordion(
      multiple = TRUE,
      accordion_panel("Parameters", icon = icon("sliders"), uiOutput("param_inputs")),
      accordion_panel(
        "Simulation settings", icon = icon("clock"),
        numericInput("t_end", "End time", value = 100, min = 1),
        numericInput("dt", "Time step (between output points)", value = 0.5, min = 0.001)
      )
    )
  ),

  # --- Main area: diagram, then outputs in tabs -------------------------------
  card(
    full_screen = TRUE,
    card_header(
      div(
        class = "model-toolbar d-flex flex-wrap gap-2 align-items-center",
        span(class = "fw-semibold me-auto", "Model diagram"),
        span(class = "small text-muted", "Template:"),
        # selectize = FALSE gives a plain dropdown (no typing)
        selectInput("template", NULL, choices = names(TEMPLATES), width = "250px",
                    selectize = FALSE),
        actionButton("load_template", "Load", class = "btn-sm btn-primary"),
        actionButton("reset", "New", icon = icon("file"), class = "btn-sm btn-outline-secondary"),
        downloadButton("save_model", "Save", class = "btn-sm btn-outline-secondary"),
        div(class = "open-file",
            fileInput("open_model", NULL, accept = c(".json", "application/json"),
                      buttonLabel = tagList(icon("folder-open"), "Open"), width = "auto"))
      )
    ),
    uiOutput("model_info"),
    visNetworkOutput("net", height = "430px")
  ),
  navset_card_tab(
    nav_panel("Epicurve", uiOutput("sim_status"), uiOutput("show_ui"),
              plotlyOutput("epicurve", height = "420px")),
    nav_panel("Equations", uiOutput("equations")),
    nav_panel("Table",
              div(class = "mb-2", downloadButton("download_csv", "Download CSV", class = "btn-sm")),
              DT::DTOutput("results_table")),
    nav_panel("Help", icon = icon("circle-question"), help_ui())
  )
)

server <- function(input, output, session) {
  # Shared state of the app (see server_canvas.R for a description)
  rv <- reactiveValues(
    model = empty_model(),
    params = numeric(0),
    params_version = 0,
    xy = complete_positions(empty_model(), data.frame(id = character(0), x = numeric(0), y = numeric(0))),
    sel = list(type = "none", id = NULL),
    canvas_version = 0,
    shown_all = NULL
  )

  proxy <- server_canvas(input, output, session, rv)
  server_editor(input, output, session, rv, proxy)
  server_outputs(input, output, session, rv)
  server_files(input, output, session, rv)   # also loads the first template
}

shinyApp(ui, server)
