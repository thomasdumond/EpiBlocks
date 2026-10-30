# =============================================================================
# model_io.R -- model "specs": templates and saved files share one format
# =============================================================================
#
# A spec is a readable description of a model, using compartment NAMES rather
# than internal ids. Templates (templates.R) are written as specs, and saved
# files are specs written as JSON. Example:
#
#   list(
#     title = "SIR", description = "...", reference = "...",
#     compartments = data.frame(name = c("S","I","R"), init = c(999,1,0),
#                               in_N = TRUE, x = c(0,200,400), y = 0),
#     flows = data.frame(from = c("S","I"), to = c("I","R"),
#                        type = c("infection_freq","linear"),
#                        expr = c("beta * S * I / N", "gamma * I"),
#                        infectious = c("I", "")),   # names, comma separated
#     parameters = list(beta = 0.3, gamma = 0.1),
#     settings = list(t_end = 100, dt = 0.5)
#   )
#
# In flows, "SOURCE" and "SINK" stand for the Source and the Sink.
# =============================================================================

SPEC_VERSION <- 1

#' Turn a spec into the app's state.
#'
#' @return list(model, xy, settings). Stops with a friendly message if the
#'   spec is not usable.
model_from_spec <- function(spec) {
  comps <- as.data.frame(spec$compartments %||% list(), stringsAsFactors = FALSE)
  flows <- as.data.frame(spec$flows %||% list(), stringsAsFactors = FALSE)

  if (nrow(comps) == 0 || is.null(comps$name)) {
    stop("The file does not contain any compartments.", call. = FALSE)
  }
  if (anyDuplicated(comps$name)) {
    stop("Two compartments have the same name: ",
         paste(unique(comps$name[duplicated(comps$name)]), collapse = ", "), call. = FALSE)
  }
  comps$init <- as.numeric(comps$init %||% 0)
  comps$in_N <- as.logical(comps$in_N %||% TRUE)
  comps$id <- vapply(seq_len(nrow(comps)), function(i) new_id("c"), "")

  name_to_id <- function(x) {
    out <- ifelse(x %in% c(SOURCE_ID, SINK_ID), x, comps$id[match(x, comps$name)])
    if (anyNA(out)) {
      stop("A flow refers to a compartment that does not exist: ",
           paste(unique(x[is.na(out)]), collapse = ", "), call. = FALSE)
    }
    out
  }

  if (nrow(flows) > 0) {
    flows$from <- name_to_id(flows$from)
    flows$to <- name_to_id(flows$to)
    flows$type <- ifelse(flows$type %in% FLOW_TYPES, flows$type, "custom")
    flows$expr <- as.character(flows$expr)
    flows$infectious <- vapply(as.character(flows$infectious %||% rep("", nrow(flows))), function(x) {
      nm <- split_ids(x)
      paste(comps$id[match(nm[nm %in% comps$name], comps$name)], collapse = ",")
    }, "", USE.NAMES = FALSE)
    flows$id <- vapply(seq_len(nrow(flows)), function(i) new_id("f"), "")
  } else {
    flows <- empty_model()$flows
  }

  params <- unlist(spec$parameters %||% list())
  params <- stats::setNames(as.numeric(params), names(params))

  model <- list(
    compartments = comps[, c("id", "name", "init", "in_N")],
    flows = flows[, c("id", "from", "to", "type", "expr", "infectious")],
    params = params,
    meta = list(
      title = spec$title %||% "",
      description = spec$description %||% "",
      reference = spec$reference %||% ""
    )
  )
  rownames(model$compartments) <- NULL
  rownames(model$flows) <- NULL

  # Positions: compartments from the spec, Source/Sink if given
  xy <- data.frame(id = character(0), x = numeric(0), y = numeric(0))
  if (!is.null(comps$x) && !is.null(comps$y)) {
    xy <- data.frame(id = comps$id, x = as.numeric(comps$x), y = as.numeric(comps$y))
  }
  for (sp in spec$special_positions %||% list()) {
    xy <- rbind(xy, data.frame(id = sp$id, x = sp$x, y = sp$y))
  }
  xy <- complete_positions(model, xy[stats::complete.cases(xy), ])

  list(model = model, xy = xy, settings = spec$settings %||% list())
}


#' Turn the app's state into a spec (for saving).
spec_from_state <- function(model, params, xy, settings) {
  comps <- model$compartments
  pos <- xy[match(comps$id, xy$id), ]
  id_to_name <- function(x) {
    ifelse(x %in% c(SOURCE_ID, SINK_ID), x, comps$name[match(x, comps$id)])
  }
  flows <- model$flows
  infectious <- vapply(flows$infectious, function(x) {
    paste(comps$name[match(split_ids(x), comps$id)], collapse = ",")
  }, "", USE.NAMES = FALSE)
  special <- xy[xy$id %in% c(SOURCE_ID, SINK_ID), ]
  used <- detect_parameters(model)

  list(
    epiblocks_version = SPEC_VERSION,
    title = model$meta$title %||% "",
    description = model$meta$description %||% "",
    reference = model$meta$reference %||% "",
    compartments = data.frame(
      name = comps$name, init = comps$init, in_N = comps$in_N,
      x = round(pos$x), y = round(pos$y)
    ),
    flows = data.frame(
      from = id_to_name(flows$from), to = id_to_name(flows$to),
      type = flows$type, expr = flows$expr, infectious = infectious
    ),
    parameters = as.list(params[intersect(used, names(params))]),
    settings = settings,
    special_positions = lapply(seq_len(nrow(special)), function(i) {
      list(id = special$id[i], x = round(special$x[i]), y = round(special$y[i]))
    })
  )
}

#' Write a spec as JSON text.
spec_to_json <- function(spec) {
  jsonlite::toJSON(spec, auto_unbox = TRUE, pretty = TRUE, digits = NA, dataframe = "rows")
}

#' Read a spec from a JSON file, with a friendly error if it is not valid.
spec_from_json_file <- function(path) {
  spec <- tryCatch(
    jsonlite::fromJSON(path, simplifyVector = TRUE, simplifyDataFrame = TRUE),
    error = function(e) stop("This file is not a valid EpiBlocks model (could not read the JSON).", call. = FALSE)
  )
  if (is.null(spec$compartments)) {
    stop("This file is not an EpiBlocks model (no compartments found).", call. = FALSE)
  }
  # special_positions comes back as a data frame; turn it into a list of rows
  sp <- spec$special_positions
  if (is.data.frame(sp)) spec$special_positions <- lapply(seq_len(nrow(sp)), function(i) as.list(sp[i, ]))
  spec
}
