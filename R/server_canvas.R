# =============================================================================
# server_canvas.R -- reacting to what users do on the diagram
# =============================================================================
#
# `rv` is the app's shared state (created in app.R):
#   rv$model           the model (see model.R)
#   rv$params          parameter values
#   rv$xy              diagram positions (id, x, y)
#   rv$sel             what is selected: list(type = "none"/"node"/"edge", id)
#   rv$canvas_version  increase it to redraw the whole diagram from the model
#
# Small changes (a new label, a removed arrow) are sent to the existing
# diagram through a "proxy", so the view does not jump around.
# =============================================================================

server_canvas <- function(input, output, session, rv) {

  # Full redraw: only when canvas_version changes (start, template, reset, load)
  output$net <- renderVisNetwork({
    rv$canvas_version
    draw_canvas(isolate(rv$model), isolate(rv$xy))
  })

  proxy <- visNetworkProxy(CANVAS_ID)

  # --- A box was placed ---------------------------------------------------------
  observeEvent(input$net_add_node, {
    ev <- input$net_add_node
    model <- rv$model
    name <- suggest_compartment_name(c(model$compartments$name, names(rv$params)))
    model$compartments <- rbind(model$compartments, data.frame(
      id = ev$id, name = name, init = 0, in_N = TRUE, stringsAsFactors = FALSE
    ))
    rv$model <- model
    rv$xy <- rbind(rv$xy[rv$xy$id != ev$id, ], data.frame(id = ev$id, x = ev$x, y = ev$y))
    visUpdateNodes(proxy, data.frame(id = ev$id, label = name))
    visSelectNodes(proxy, id = ev$id)
    rv$sel <- list(type = "node", id = ev$id)
  })

  # --- An arrow was drawn ---------------------------------------------------------
  observeEvent(input$net_add_edge, {
    ev <- input$net_add_edge
    model <- rv$model
    type <- default_flow_type(ev$from, ev$to)
    model$flows <- rbind(model$flows, data.frame(
      id = ev$id, from = ev$from, to = ev$to, type = type, expr = "",
      infectious = "", stringsAsFactors = FALSE
    ))
    i <- nrow(model$flows)
    model$flows$expr[i] <- regenerate_flow_expr(model, i)
    rv$model <- model
    visUpdateEdges(proxy, data.frame(id = ev$id, label = model$flows$expr[i]))
    visSelectEdges(proxy, id = ev$id)
    rv$sel <- list(type = "edge", id = ev$id)
  })

  observeEvent(input$net_bad_edge, {
    ev <- input$net_bad_edge
    msg <- if (identical(ev$from, ev$to)) {
      "An arrow must go from one box to a different box."
    } else if (is_sink_id(ev$from)) {
      "Nothing can come out of the Sink. Draw arrows into the Sink for deaths or removals."
    } else {
      "Nothing can flow into the Source. Draw arrows out of the Source for births or inflow."
    }
    showNotification(msg, type = "warning")
  })

  # --- Items were deleted with the toolbar -----------------------------------
  observeEvent(input$net_delete, {
    ev <- input$net_delete
    rv$model <- remove_elements(rv$model, unlist(ev$nodes), unlist(ev$edges))
    rv$xy <- rv$xy[!rv$xy$id %in% unlist(ev$nodes), ]
    rv$sel <- list(type = "none", id = NULL)
  })

  # --- Sources / Sinks: added only when the user wants an open model ----------
  # As many as needed: each press adds one more (Source, Source 2, ...).
  # @param base SOURCE_ID or SINK_ID
  add_special <- function(base) {
    model <- rv$model
    id <- next_special_id(base, model_special(model))
    model$special <- sort_special(c(model_special(model), id))
    rv$model <- model
    rv$xy <- complete_positions(model, rv$xy[rv$xy$id != id, ])
    pos <- rv$xy[rv$xy$id == id, ]
    visUpdateNodes(proxy, data.frame(id = id, label = special_label(id),
                                     group = "special", x = pos$x, y = pos$y))
    rv$sel <- list(type = "special", id = id)
  }
  observeEvent(input$add_source, add_special(SOURCE_ID))
  observeEvent(input$add_sink, add_special(SINK_ID))

  # Remove a Source or Sink (button in the right panel); its arrows go too
  observeEvent(input$special_delete, {
    id <- rv$sel$id
    req(identical(rv$sel$type, "special"))
    gone_flows <- rv$model$flows$id[rv$model$flows$from == id | rv$model$flows$to == id]
    rv$model <- remove_elements(rv$model, id)
    rv$xy <- rv$xy[rv$xy$id != id, ]
    visRemoveEdges(proxy, gone_flows)
    visRemoveNodes(proxy, id)
    rv$sel <- list(type = "none", id = NULL)
  })

  # Buttons above the diagram (open_buttons_ui() in canvas.R) stay in place so
  # more can be added; only their wording follows the model
  open_state <- reactive({
    special <- model_special(rv$model)
    list(closed = length(special) == 0,
         source = any(is_source_id(special)), sink = any(is_sink_id(special)))
  })
  output$open_prefix <- renderText(
    if (open_state()$closed) "Closed model. To add births or deaths:" else "Add:"
  )
  observeEvent(open_state()$source, ignoreInit = FALSE, {
    updateActionButton(session, "add_source", icon = icon("plus"),
      label = if (open_state()$source) "Another Source" else "Source (births, inflow)")
  })
  observeEvent(open_state()$sink, ignoreInit = FALSE, {
    updateActionButton(session, "add_sink", icon = icon("plus"),
      label = if (open_state()$sink) "Another Sink" else "Sink (deaths, removal)")
  })

  # --- Selection: decides what the editor on the right shows ---------------------
  observeEvent(input$net_click, {
    ev <- input$net_click
    all_nodes <- unlist(ev$nodes)
    nodes <- all_nodes[!is_special_id(all_nodes)]
    edges <- unlist(ev$edges)
    rv$sel <- if (length(nodes)) {
      list(type = "node", id = nodes[1])
    } else if (length(all_nodes)) {
      list(type = "special", id = all_nodes[1])
    } else if (length(edges)) {
      list(type = "edge", id = edges[1])
    } else {
      list(type = "none", id = NULL)
    }
  })

  # --- Boxes were moved: remember positions (used when redrawing/saving) -------
  observeEvent(input$net_drag, {
    pos <- input$net_drag$pos
    xy <- rv$xy
    for (id in names(pos)) {
      xy <- xy[xy$id != id, ]
      xy <- rbind(xy, data.frame(id = id, x = pos[[id]]$x, y = pos[[id]]$y))
    }
    rv$xy <- xy
  })

  # Return the proxy so other parts of the server can update the diagram
  proxy
}
