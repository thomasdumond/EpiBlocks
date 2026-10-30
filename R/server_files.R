# =============================================================================
# server_files.R -- templates, new model, save and open
# =============================================================================

server_files <- function(input, output, session, rv) {

  # Replace the whole app state with a spec (template or file)
  apply_spec <- function(spec) apply_state(model_from_spec(spec))

  apply_state <- function(s) {
    rv$model <- s$model
    rv$params <- s$model$params
    rv$xy <- s$xy
    rv$sel <- list(type = "none", id = NULL)
    rv$shown_all <- NULL
    rv$params_version <- rv$params_version + 1
    rv$canvas_version <- rv$canvas_version + 1
    if (!is.null(s$settings$t_end)) updateNumericInput(session, "t_end", value = s$settings$t_end)
    if (!is.null(s$settings$dt)) updateNumericInput(session, "dt", value = s$settings$dt)
  }

  # Start with the first template so there is something to look at
  isolate(apply_spec(TEMPLATES[[1]]))

  model_is_empty <- function() nrow(rv$model$compartments) == 0

  # Ask before replacing a model that has content. `action` runs on "Yes".
  pending_action <- reactiveVal(NULL)
  confirm_then <- function(message, action) {
    if (model_is_empty()) return(action())
    pending_action(action)
    showModal(modalDialog(
      title = "Replace the current model?",
      p(message),
      p(class = "text-muted small", "Tip: use Save first if you want to keep your work."),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_yes", "Yes, continue", class = "btn-danger")
      ),
      easyClose = TRUE
    ))
  }
  observeEvent(input$confirm_yes, {
    removeModal()
    action <- pending_action()
    pending_action(NULL)
    if (is.function(action)) action()
  })

  # --- Templates -----------------------------------------------------------------
  observeEvent(input$load_template, {
    name <- input$template
    req(name %in% names(TEMPLATES))
    confirm_then(
      paste0("Loading the ", name, " template will replace the model on the diagram."),
      function() apply_spec(TEMPLATES[[name]])
    )
  })

  # --- New (empty) model ---------------------------------------------------------
  observeEvent(input$reset, {
    confirm_then(
      "This will delete all compartments and flows and start from an empty diagram.",
      function() {
        empty <- empty_model()
        apply_state(list(model = empty, settings = list(),
                         xy = complete_positions(empty, data.frame(id = character(0), x = numeric(0), y = numeric(0)))))
      }
    )
  })

  # --- Save ------------------------------------------------------------------------
  output$save_model <- downloadHandler(
    filename = function() {
      title <- rv$model$meta$title
      base <- if (nzchar(title %||% "")) gsub("[^A-Za-z0-9]+", "_", title) else "my_model"
      paste0(base, "_", format(Sys.Date()), ".json")
    },
    content = function(file) {
      spec <- spec_from_state(rv$model, rv$params, rv$xy,
                              list(t_end = input$t_end, dt = input$dt))
      writeLines(spec_to_json(spec), file)
    }
  )

  # --- Open ------------------------------------------------------------------------
  observeEvent(input$open_model, {
    path <- input$open_model$datapath
    spec <- tryCatch(spec_from_json_file(path), error = function(e) e)
    if (inherits(spec, "error")) {
      showNotification(conditionMessage(spec), type = "error", duration = 8)
      return()
    }
    confirm_then(
      paste0("Opening '", input$open_model$name, "' will replace the model on the diagram."),
      function() {
        ok <- tryCatch({ apply_spec(spec); TRUE }, error = function(e) e)
        if (inherits(ok, "error")) {
          showNotification(paste("Could not open this model:", conditionMessage(ok)),
                           type = "error", duration = 8)
        } else {
          showNotification("Model opened.", type = "message", duration = 3)
        }
      }
    )
  })

  # --- Description of the loaded template ------------------------------------------
  output$model_info <- renderUI({
    meta <- rv$model$meta
    if (!nzchar(meta$title %||% "")) return(NULL)
    div(
      class = "alert alert-info py-2 m-2 small",
      strong(meta$title), br(),
      meta$description,
      if (nzchar(meta$reference %||% "")) div(class = "mt-1 fst-italic", "Reference: ", meta$reference)
    )
  })
}
