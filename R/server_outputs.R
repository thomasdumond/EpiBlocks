# =============================================================================
# server_outputs.R -- simulation, epicurve, equations, results table
# =============================================================================

# Colour-blind friendly palette (Okabe-Ito), one colour per compartment
COMPARTMENT_COLOURS <- c("#0072B2", "#D55E00", "#009E73", "#CC79A7",
                         "#E69F00", "#56B4E9", "#000000", "#999999")

compartment_colours <- function(n) rep_len(COMPARTMENT_COLOURS, n)

server_outputs <- function(input, output, session, rv) {

  # ===========================================================================
  # Simulation: re-runs automatically, but waits until the user has stopped
  # typing for 0.4 s (debounce) to avoid running on every key press
  # ===========================================================================
  sim_request <- reactive({
    list(model = rv$model, params = rv$params, t_end = input$t_end, dt = input$dt)
  }) |> debounce(400)

  sim <- reactive({
    r <- sim_request()
    run_simulation(r$model, r$params, t_end = r$t_end, dt = r$dt)
  })

  # Errors (red) and warnings (yellow) above the plot
  output$sim_status <- renderUI({
    s <- sim()
    tagList(
      if (length(s$errors)) div(
        class = "alert alert-danger py-2",
        strong("The model cannot be simulated yet:"),
        tags$ul(class = "mb-0", lapply(s$errors, tags$li))
      ),
      if (length(s$warnings)) div(
        class = "alert alert-warning py-2",
        tags$ul(class = "mb-0", lapply(s$warnings, tags$li))
      )
    )
  })

  # ===========================================================================
  # Epicurve
  # ===========================================================================

  # Compartment names, only updated when they really change
  comp_names <- reactiveVal(character(0))
  observe({
    nm <- rv$model$compartments$name
    if (!identical(nm, comp_names())) comp_names(nm)
  })

  # "Show" check boxes: keep the user's choices when boxes are added/renamed
  output$show_ui <- renderUI({
    nm <- comp_names()
    if (length(nm) == 0) return(NULL)
    old_all <- isolate(rv$shown_all)
    old_sel <- isolate(input$show_comps)
    selected <- if (is.null(old_all)) nm else union(intersect(old_sel, nm), setdiff(nm, old_all))
    rv$shown_all <- nm
    checkboxGroupInput("show_comps", "Show:", choices = nm, selected = selected, inline = TRUE)
  })

  output$epicurve <- renderPlotly({
    s <- sim()
    validate(need(s$ok, "Fix the problems listed above to see the epicurve."))
    d <- s$data
    all_names <- names(d)[-1]
    shown <- if (is.null(rv$shown_all)) all_names else intersect(input$show_comps, all_names)
    cols <- stats::setNames(compartment_colours(length(all_names)), all_names)

    p <- plot_ly(type = "scatter", mode = "lines")
    for (nm in shown) {
      p <- add_lines(p, x = d$time, y = d[[nm]], name = nm,
                     line = list(color = cols[[nm]], width = 2.5),
                     hovertemplate = paste0(nm, ": %{y:.2f}<extra></extra>"))
    }
    p |>
      layout(
        xaxis = list(title = "Time"),
        yaxis = list(title = "Number of animals", rangemode = "tozero"),
        hovermode = "x unified",
        legend = list(orientation = "h", y = 1.08)
      ) |>
      config(displaylogo = FALSE)
  })

  # ===========================================================================
  # Equations (plain text for now)
  # ===========================================================================
  output$equations <- renderUI({
    model <- rv$model
    if (nrow(model$compartments) == 0) {
      return(p(class = "text-muted", "Add compartments and flows to see the equations."))
    }
    flows <- model$flows
    type_labels <- stats::setNames(names(FLOW_TYPES), FLOW_TYPES)
    params <- rv$params[detect_parameters(model)]

    tagList(
      # Ask the bundled MathJax (see app.R) to typeset the new equations
      tags$script("epiTypeset();"),
      p(class = "text-muted small",
        "For each compartment: rate of change = flows in − flows out."),
      div(class = "equations", model_equations_latex(model)),
      if (nrow(flows)) tagList(
        h6(class = "mt-3", "Flows"),
        tags$table(
          class = "table table-sm small",
          tags$thead(tags$tr(tags$th("Flow"), tags$th("Type"), tags$th("Rate"))),
          tags$tbody(lapply(seq_len(nrow(flows)), function(i) tags$tr(
            tags$td(flow_label(model, flows[i, ])),
            tags$td(type_labels[[flows$type[i]]]),
            tags$td(tags$code(flows$expr[i]))
          )))
        )
      ),
      if (length(params)) tagList(
        h6("Parameter values"),
        p(class = "small", paste0(names(params), " = ", signif(params, 4), collapse = ";  "))
      )
    )
  })

  # ===========================================================================
  # Results table + CSV download
  # ===========================================================================
  output$results_table <- DT::renderDT({
    s <- sim()
    validate(need(s$ok, "No results yet."))
    d <- s$data
    d[] <- lapply(d, function(x) round(x, 4))
    DT::datatable(d, rownames = FALSE,
                  options = list(pageLength = 15, scrollX = TRUE, dom = "tip"))
  })

  output$download_csv <- downloadHandler(
    filename = function() paste0("epiblocks_results_", format(Sys.Date()), ".csv"),
    content = function(file) {
      s <- sim()
      if (!s$ok) stop("No results to download: fix the model first.")
      utils::write.csv(s$data, file, row.names = FALSE)
    }
  )

  sim
}
