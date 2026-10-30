# =============================================================================
# canvas.R -- the editable diagram (visNetwork)
# =============================================================================
#
# The diagram is drawn by visNetwork (an R wrapper around the vis-network
# JavaScript library). Its "manipulation" toolbar lets users add boxes, draw
# arrows and delete things. Each of those actions runs a small JavaScript
# function (below) that updates the drawing AND tells Shiny what happened via
# Shiny.setInputValue(). The server (server_canvas.R) then updates the model.
#
# Inputs sent to Shiny (all prefixed with the output id "net"):
#   net_add_node   {id, x, y}         a box was placed on the canvas
#   net_add_edge   {id, from, to}     an arrow was drawn
#   net_bad_edge   {from, to}         an arrow that is not allowed was refused
#   net_delete     {nodes, edges}     items were deleted
#   net_bad_delete                    the user tried to delete Source/Sink
#   net_click      {nodes, edges}     the user clicked something (or nothing)
#   net_drag       {pos}              boxes were moved (new positions)
# =============================================================================

CANVAS_ID <- "net"

# Colours used on the canvas
COL_BOX        <- "#dbeafe"
COL_BOX_BORDER <- "#1d4ed8"
COL_SPECIAL    <- "#f3f4f6"
COL_SPECIAL_BORDER <- "#9ca3af"
COL_ARROW      <- "#475569"

# --- JavaScript handlers (run in the browser) ---------------------------------

js_new_id <- "Math.random().toString(36).slice(2, 10)"

JS_ADD_NODE <- sprintf("function(data, callback) {
  data.id = 'c_' + %s;
  data.label = '...';
  callback(data);
  Shiny.setInputValue('net_add_node', {id: data.id, x: data.x, y: data.y}, {priority: 'event'});
}", js_new_id)

# Refuse arrows that make no sense; Shiny shows the reason
JS_ADD_EDGE <- sprintf("function(data, callback) {
  var bad = data.from === data.to || data.from === 'SINK' || data.to === 'SOURCE';
  if (bad) {
    callback(null);
    Shiny.setInputValue('net_bad_edge', {from: data.from, to: data.to}, {priority: 'event'});
    return;
  }
  data.id = 'f_' + %s;
  callback(data);
  Shiny.setInputValue('net_add_edge', {id: data.id, from: data.from, to: data.to}, {priority: 'event'});
}", js_new_id)

# The Source and Sink cannot be deleted
JS_DELETE <- "function(data, callback) {
  if (data.nodes.indexOf('SOURCE') >= 0 || data.nodes.indexOf('SINK') >= 0) {
    callback(null);
    Shiny.setInputValue('net_bad_delete', Math.random());
    return;
  }
  callback(data);
  Shiny.setInputValue('net_delete', {nodes: data.nodes, edges: data.edges}, {priority: 'event'});
}"

# The toolbar's "Edit" button just opens the box in the panel on the right
JS_EDIT_NODE <- "function(data, callback) {
  callback(null);
  Shiny.setInputValue('net_click', {nodes: [data.id], edges: []}, {priority: 'event'});
}"

JS_CLICK <- "function(p) {
  Shiny.setInputValue('net_click', {nodes: p.nodes, edges: p.edges}, {priority: 'event'});
}"

JS_DRAG_END <- "function(p) {
  if (p.nodes.length > 0) {
    Shiny.setInputValue('net_drag', {pos: this.getPositions(p.nodes)}, {priority: 'event'});
  }
}"

# Wording of the diagram toolbar (vis-network "locale")
CANVAS_LOCALE <- list(
  edit = "Edit", del = "Delete selected", back = "Back", close = "Close",
  addNode = "Add compartment", addEdge = "Draw flow",
  editNode = "Edit", editEdge = "Edit",
  addDescription = "Click on an empty space to place a new compartment.",
  edgeDescription = "Press on a box (or Source) and drag the arrow to another box (or Sink).",
  editEdgeDescription = "Drag the ends of the arrow.",
  createEdgeError = "Cannot link edges to a cluster.",
  deleteClusterError = "Clusters cannot be deleted.",
  editClusterError = "Clusters cannot be edited."
)


# --- Positions ----------------------------------------------------------------

#' Default positions for nodes that have none yet: compartments in a row,
#' Source above-left, Sink below-right.
complete_positions <- function(model, xy) {
  ids <- c(SOURCE_ID, SINK_ID, model$compartments$id)
  missing <- setdiff(ids, xy$id)
  if (length(missing) == 0) return(xy)
  n <- nrow(model$compartments)
  auto <- data.frame(
    id = c(SOURCE_ID, SINK_ID, model$compartments$id),
    x = c(-120, (n - 1) * 180 + 120, (seq_len(n) - 1) * 180),
    y = c(-160, 160, rep(0, n)),
    stringsAsFactors = FALSE
  )
  rbind(xy, auto[auto$id %in% missing, , drop = FALSE])
}


# --- Building the diagram -------------------------------------------------------

#' Nodes data frame for visNetwork.
canvas_nodes <- function(model, xy) {
  comps <- model$compartments
  nodes <- data.frame(
    id    = c(SOURCE_ID, SINK_ID, comps$id),
    label = c("Source\n(births, inflow)", "Sink\n(deaths, removal)", comps$name),
    group = c("special", "special", rep("compartment", nrow(comps))),
    stringsAsFactors = FALSE
  )
  pos <- xy[match(nodes$id, xy$id), ]
  nodes$x <- pos$x
  nodes$y <- pos$y
  nodes
}

#' Edges data frame for visNetwork (label = the rate formula).
canvas_edges <- function(model) {
  data.frame(
    id = model$flows$id, from = model$flows$from, to = model$flows$to,
    label = model$flows$expr, stringsAsFactors = FALSE
  )
}

#' The full diagram widget.
draw_canvas <- function(model, xy) {
  xy <- complete_positions(model, xy)
  net <- visNetwork(canvas_nodes(model, xy), canvas_edges(model), width = "100%") |>
    # Default look of a box (also used for boxes added by the user)
    visNodes(
      shape = "box", physics = FALSE, margin = 12,
      font = list(size = 20, face = "arial"),
      color = list(background = COL_BOX, border = COL_BOX_BORDER,
                   highlight = list(background = "#bfdbfe", border = COL_BOX_BORDER)),
      borderWidth = 2
    ) |>
    # Boxes from templates/files have group "compartment": same look as above
    visGroups(
      groupname = "compartment", shape = "box",
      color = list(background = COL_BOX, border = COL_BOX_BORDER,
                   highlight = list(background = "#bfdbfe", border = COL_BOX_BORDER))
    ) |>
    visGroups(
      groupname = "special", shape = "box",
      font = list(size = 13, color = "#6b7280"),
      color = list(background = COL_SPECIAL, border = COL_SPECIAL_BORDER,
                   highlight = list(background = "#e5e7eb", border = "#6b7280")),
      shapeProperties = list(borderDashes = c(6, 4))
    ) |>
    visEdges(
      arrows = "to", width = 2,
      color = list(color = COL_ARROW, highlight = "#dc2626"),
      font = list(size = 13, align = "middle", background = "white"),
      smooth = list(enabled = TRUE, type = "curvedCW", roundness = 0.15)
    ) |>
    visPhysics(enabled = FALSE) |>
    visInteraction(multiselect = FALSE, hover = TRUE) |>
    visOptions(manipulation = list(
      enabled = TRUE,
      initiallyActive = TRUE,   # show the toolbar straight away
      addNode = htmlwidgets::JS(JS_ADD_NODE),
      addEdge = htmlwidgets::JS(JS_ADD_EDGE),
      deleteNode = htmlwidgets::JS(JS_DELETE),
      deleteEdge = htmlwidgets::JS(JS_DELETE),
      editNode = htmlwidgets::JS(JS_EDIT_NODE),
      editEdge = FALSE
    )) |>
    visEvents(click = JS_CLICK, dragEnd = JS_DRAG_END)

  # Friendlier toolbar wording
  net$x$options$locales <- list(epi = CANVAS_LOCALE)
  net$x$options$locale <- "epi"
  net
}
