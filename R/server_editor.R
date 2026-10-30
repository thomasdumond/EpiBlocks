# =============================================================================
# server_editor.R -- the panel on the right: edit a box, a flow, parameters
# =============================================================================

server_editor <- function(input, output, session, rv, proxy) {

  # Index of the selected compartment / flow in the model (NA if none)
  sel_comp_index <- function() {
    s <- rv$sel
    if (!identical(s$type, "node")) return(NA_integer_)
    match(s$id, rv$model$compartments$id)
  }
  sel_flow_index <- function() {
    s <- rv$sel
    if (!identical(s$type, "edge")) return(NA_integer_)
    match(s$id, rv$model$flows$id)
  }

  # Update the label of one or more arrows on the diagram
  refresh_edge_labels <- function(model, ids = model$flows$id) {
    if (length(ids) == 0) return()
    f <- model$flows[model$flows$id %in% ids, ]
    visUpdateEdges(proxy, data.frame(id = f$id, label = f$expr))
  }

  # ===========================================================================
  # Editor panel: what is shown depends on the selection only
  # ===========================================================================
  output$editor <- renderUI({
    s <- rv$sel
    model <- isolate(rv$model)

    if (identical(s$type, "node")) {
      comp <- model$compartments[model$compartments$id == s$id, ]
      if (nrow(comp) == 0) return(editor_hint())
      return(tagList(
        h5(icon("square"), " Compartment"),
        div(
          class = "d-flex gap-2 align-items-end",
          div(class = "flex-grow-1", textInput("node_name", "Name", comp$name)),
          actionButton("node_rename", "Rename", class = "btn-sm btn-outline-primary mb-3")
        ),
        numericInput("node_init", "Initial value (number at time 0)", comp$init, min = 0),
        checkboxInput("node_inN", "Counts towards N (living population)", comp$in_N),
        actionButton("node_delete", "Delete compartment", icon = icon("trash"),
                     class = "btn-sm btn-outline-danger")
      ))
    }

    if (identical(s$type, "edge")) {
      i <- match(s$id, model$flows$id)
      if (is.na(i)) return(editor_hint())
      f <- model$flows[i, ]
      comp_choices <- stats::setNames(model$compartments$id, model$compartments$name)
      types <- if (f$from == SOURCE_ID) FLOW_TYPES[FLOW_TYPES %in% c("constant", "custom")] else FLOW_TYPES
      return(tagList(
        h5(icon("arrow-right"), " Flow: ", flow_label(model, f)),
        selectInput("flow_type", "Type of flow", choices = types, selected = f$type,
                    selectize = FALSE),
        conditionalPanel(
          "input.flow_type == 'infection_freq' || input.flow_type == 'infection_dens'",
          checkboxGroupInput("flow_inf", "Infectious compartment(s)", choices = comp_choices,
                             selected = split_ids(f$infectious), inline = TRUE)
        ),
        textAreaInput("flow_expr", "Rate (animals per time unit)", f$expr,
                      rows = 2, resize = "vertical"),
        uiOutput("flow_check"),
        actionButton("flow_delete", "Delete flow", icon = icon("trash"),
                     class = "btn-sm btn-outline-danger")
      ))
    }

    editor_hint()
  })

  # ===========================================================================
  # Compartment editing
  # ===========================================================================
  observeEvent(input$node_rename, {
    i <- sel_comp_index()
    req(!is.na(i))
    model <- rv$model
    new <- trimws(input$node_name)
    old <- model$compartments$name[i]
    if (identical(new, old)) return()
    taken <- c(model$compartments$name[-i], param_names())
    msg <- check_name(new, taken)
    if (!is.null(msg)) {
      showNotification(msg, type = "error")
      return()
    }
    model <- rename_compartment(model, model$compartments$id[i], new)
    rv$model <- model
    visUpdateNodes(proxy, data.frame(id = model$compartments$id[i], label = new))
    refresh_edge_labels(model)
    showNotification(paste0("Renamed ", old, " to ", new, " (formulas updated)."),
                     type = "message", duration = 3)
  })

  observeEvent(input$node_init, {
    i <- sel_comp_index()
    req(!is.na(i))
    v <- input$node_init
    v <- if (is.numeric(v) && length(v) == 1) v else NA_real_
    if (!identical(v, rv$model$compartments$init[i])) rv$model$compartments$init[i] <- v
  }, ignoreNULL = FALSE)

  observeEvent(input$node_inN, {
    i <- sel_comp_index()
    req(!is.na(i))
    if (!identical(input$node_inN, rv$model$compartments$in_N[i])) {
      rv$model$compartments$in_N[i] <- input$node_inN
    }
  })

  observeEvent(input$node_delete, {
    i <- sel_comp_index()
    req(!is.na(i))
    model <- rv$model
    id <- model$compartments$id[i]
    gone_flows <- model$flows$id[model$flows$from == id | model$flows$to == id]
    rv$model <- remove_elements(model, id)
    rv$xy <- rv$xy[rv$xy$id != id, ]
    visRemoveEdges(proxy, gone_flows)
    visRemoveNodes(proxy, id)
    rv$sel <- list(type = "none", id = NULL)
  })

  # ===========================================================================
  # Flow editing
  # ===========================================================================

  # A type was picked in the menu: pre-fill the formula
  observeEvent(input$flow_type, {
    i <- sel_flow_index()
    req(!is.na(i))
    model <- rv$model
    if (identical(input$flow_type, model$flows$type[i])) return()
    model$flows$type[i] <- input$flow_type

    # Infection: by default the arrow's destination is the infectious box
    if (is_infection_type(input$flow_type) && length(split_ids(model$flows$infectious[i])) == 0 &&
        model$flows$to[i] %in% model$compartments$id) {
      model$flows$infectious[i] <- model$flows$to[i]
      updateCheckboxGroupInput(session, "flow_inf", selected = model$flows$to[i])
    }
    if (input$flow_type != "custom") {
      model$flows$expr[i] <- regenerate_flow_expr(model, i, keep_param = FALSE)
      updateTextAreaInput(session, "flow_expr", value = model$flows$expr[i])
    }
    rv$model <- model
    refresh_edge_labels(model, model$flows$id[i])
  })

  # Infectious compartments changed: rebuild the formula, keep the parameter
  observeEvent(input$flow_inf, {
    i <- sel_flow_index()
    req(!is.na(i))
    model <- rv$model
    new <- paste(input$flow_inf, collapse = ",")
    if (identical(new, model$flows$infectious[i])) return()
    model$flows$infectious[i] <- new
    if (is_infection_type(model$flows$type[i])) {
      model$flows$expr[i] <- regenerate_flow_expr(model, i, keep_param = TRUE)
      updateTextAreaInput(session, "flow_expr", value = model$flows$expr[i])
    }
    rv$model <- model
    refresh_edge_labels(model, model$flows$id[i])
  }, ignoreNULL = FALSE)

  # The formula was typed in
  observeEvent(input$flow_expr, {
    i <- sel_flow_index()
    req(!is.na(i))
    if (identical(input$flow_expr, rv$model$flows$expr[i])) return()
    rv$model$flows$expr[i] <- input$flow_expr
    refresh_edge_labels(rv$model, rv$model$flows$id[i])
  })

  # Feedback under the formula: error, or the parameters it uses
  output$flow_check <- renderUI({
    i <- sel_flow_index()
    req(!is.na(i))
    model <- rv$model
    p <- parse_rate(model$flows$expr[i])
    if (!p$ok) {
      return(div(class = "text-danger small mb-3", icon("triangle-exclamation"), " ", p$error))
    }
    pars <- flow_parameters(model, model$flows$expr[i])
    div(class = "text-muted small mb-3",
        icon("check"), " Formula OK.",
        if (length(pars)) paste0(" Parameters: ", paste(pars, collapse = ", ")))
  })

  observeEvent(input$flow_delete, {
    i <- sel_flow_index()
    req(!is.na(i))
    id <- rv$model$flows$id[i]
    rv$model <- remove_elements(rv$model, flow_ids = id)
    visRemoveEdges(proxy, id)
    rv$sel <- list(type = "none", id = NULL)
  })

  # ===========================================================================
  # Parameters: one numeric input per parameter found in the formulas
  # ===========================================================================

  # Parameter names, only updated when the set of names really changes
  # (so the inputs are not redrawn while someone is typing a value)
  param_names <- reactiveVal(character(0))
  observe({
    nm <- detect_parameters(rv$model)
    if (!identical(nm, param_names())) param_names(nm)
    # Give new parameters a default value
    p <- isolate(rv$params)
    new <- setdiff(nm, names(p))
    if (length(new)) {
      p[new] <- vapply(new, default_param_value, numeric(1))
      rv$params <- p
    }
  })

  # Input ids include params_version (e.g. "par_3_beta"), so that after a
  # template or file is loaded, values typed in the old inputs are ignored.
  par_id <- function(n) paste0("par_", rv$params_version, "_", n)

  output$param_inputs <- renderUI({
    nm <- param_names()
    rv$params_version  # redraw when parameters are replaced (template / file)
    if (length(nm) == 0) {
      return(p(class = "text-muted small",
               "Parameters appear here automatically when you use them in a rate formula."))
    }
    p <- isolate(rv$params)
    div(
      class = "param-grid",
      lapply(nm, function(n) {
        v <- p[[n]]
        step <- if (is.finite(v) && v > 0) 10^(floor(log10(v)) - 1) else 0.01
        numericInput(par_id(n), n, value = v, step = step)
      })
    )
  })

  # Copy values typed in the inputs into rv$params
  observe({
    for (n in param_names()) {
      v <- input[[par_id(n)]]
      if (is.null(v)) next
      v <- if (is.numeric(v) && length(v) == 1) v else NA_real_
      if (!identical(unname(v), unname(isolate(rv$params[n])))) rv$params[n] <- v
    }
  })

  param_names
}

#' What the editor shows when nothing is selected.
editor_hint <- function() {
  div(
    class = "text-muted small",
    h5(icon("hand-pointer"), " Nothing selected"),
    p("Click a box to rename it or set its initial value."),
    p("Click an arrow to choose its type and edit its rate."),
    p("Use ", strong("Add compartment"), " and ", strong("Draw flow"),
      " at the top of the diagram to build your model.")
  )
}
