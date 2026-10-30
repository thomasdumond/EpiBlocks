# =============================================================================
# model.R -- how a model is stored, and the flow-type menu
# =============================================================================
#
# A model is a plain R list:
#
#   compartments  data frame, one row per box:
#                   id    internal id used by the diagram (never shown)
#                   name  name used in formulas, e.g. "S"
#                   init  initial value (number of animals at t = 0)
#                   in_N  TRUE if it counts towards N (living population)
#   flows         data frame, one row per arrow:
#                   id          internal id
#                   from, to    compartment ids, or "SOURCE" / "SINK"
#                   type        one of FLOW_TYPES (see below)
#                   expr        the rate formula as text
#                   infectious  for infection flows: ids of the infectious
#                               compartments, comma separated
#   params        named numeric vector of parameter values
#   meta          title / description / reference (used by templates)
#
# Diagram positions are kept separately (see canvas.R) so that dragging a box
# does not re-run the simulation.
# =============================================================================

SOURCE_ID <- "SOURCE"
SINK_ID   <- "SINK"

#' An empty model: no compartments, no flows.
empty_model <- function() {
  list(
    compartments = data.frame(
      id = character(0), name = character(0), init = numeric(0),
      in_N = logical(0), stringsAsFactors = FALSE
    ),
    flows = data.frame(
      id = character(0), from = character(0), to = character(0),
      type = character(0), expr = character(0), infectious = character(0),
      stringsAsFactors = FALSE
    ),
    params = numeric(0),
    meta = list(title = "", description = "", reference = "")
  )
}

#' Translate a compartment id (or SOURCE/SINK) to a display name.
node_name <- function(model, id) {
  vapply(id, function(i) {
    if (identical(i, SOURCE_ID)) return("Source")
    if (identical(i, SINK_ID)) return("Sink")
    nm <- model$compartments$name[model$compartments$id == i]
    if (length(nm)) nm else "?"
  }, character(1), USE.NAMES = FALSE)
}

#' Short label for a flow, e.g. "S → I".
flow_label <- function(model, flow_row) {
  paste(node_name(model, flow_row$from), "→", node_name(model, flow_row$to))
}

#' Make a new unique id, e.g. "c_k3x9a".
new_id <- function(prefix) {
  paste0(prefix, "_", paste(sample(c(letters, 0:9), 8, replace = TRUE), collapse = ""))
}

#' Suggest a free compartment name: C1, C2, ...
suggest_compartment_name <- function(taken) {
  i <- 1
  while (paste0("C", i) %in% taken) i <- i + 1
  paste0("C", i)
}


# -----------------------------------------------------------------------------
# Flow types: the menu users pick from, with a pre-filled formula
# -----------------------------------------------------------------------------

# Named vector: label shown in the menu = internal code
# (the pre-filled formulas are in flow_formula() below)
FLOW_TYPES <- c(
  "Infection (frequency dependent)" = "infection_freq",  # beta * S * I / N
  "Infection (density dependent)"   = "infection_dens",  # beta * S * I
  "Progression / recovery / death"  = "linear",          # rate * origin
  "Constant inflow"                 = "constant",        # lambda
  "Custom formula"                  = "custom"
)

is_infection_type <- function(type) type %in% c("infection_freq", "infection_dens")

#' Usual parameter name for a new flow of this type.
default_param_base <- function(type, to) {
  switch(type,
    infection_freq = "beta",
    infection_dens = "beta",
    linear   = if (identical(to, SINK_ID)) "mu" else "gamma",
    constant = "lambda",
    "k"
  )
}

#' Pick a parameter name that is not used yet: beta, beta2, beta3, ...
unique_param_name <- function(base, taken) {
  if (!base %in% taken) return(base)
  i <- 2
  while (paste0(base, i) %in% taken) i <- i + 1
  paste0(base, i)
}

#' Sensible starting value for a newly detected parameter.
default_param_value <- function(name) {
  if (startsWith(name, "beta"))   return(0.3)
  if (startsWith(name, "gamma"))  return(0.1)
  if (startsWith(name, "sigma"))  return(0.2)
  if (startsWith(name, "mu"))     return(0.01)
  if (startsWith(name, "lambda")) return(1)
  0.1
}

#' Pre-filled formula for a flow type.
#'
#' @param type        flow type code (see FLOW_TYPES)
#' @param from_name   name of the origin compartment ("Source" if none)
#' @param inf_names   names of infectious compartments (infection types only)
#' @param param       parameter name to use
flow_formula <- function(type, from_name, inf_names, param) {
  # Several infectious compartments are added up: beta * S * (A + D) / N
  inf_term <- if (length(inf_names) == 0) {
    "I"
  } else if (length(inf_names) == 1) {
    inf_names
  } else {
    paste0("(", paste(inf_names, collapse = " + "), ")")
  }
  switch(type,
    infection_freq = sprintf("%s * %s * %s / N", param, from_name, inf_term),
    infection_dens = sprintf("%s * %s * %s", param, from_name, inf_term),
    linear         = sprintf("%s * %s", param, from_name),
    constant       = param,
    param
  )
}

#' Default type for a newly drawn arrow.
default_flow_type <- function(from, to) {
  if (identical(from, SOURCE_ID)) "constant" else "linear"
}

#' Names used in a model's formulas that are parameters (i.e. not
#' compartments, N or t), in order of first appearance.
detect_parameters <- function(model) {
  comp_names <- model$compartments$name
  syms <- unlist(lapply(model$flows$expr, function(x) {
    p <- parse_rate(x)
    if (p$ok) p$symbols else character(0)
  }))
  setdiff(unique(syms), c(comp_names, RESERVED_NAMES))
}

#' Parameters used by one flow's formula.
flow_parameters <- function(model, expr) {
  p <- parse_rate(expr)
  if (!p$ok) return(character(0))
  setdiff(p$symbols, c(model$compartments$name, RESERVED_NAMES))
}

#' Regenerate the formula of flow i from its type (used when the user picks
#' a type in the menu or changes the infectious compartments).
#'
#' @param keep_param If TRUE and the current formula has exactly one
#'   parameter, keep that name; otherwise suggest a fresh one.
regenerate_flow_expr <- function(model, i, keep_param = FALSE) {
  f <- model$flows[i, ]
  if (f$type == "custom") return(f$expr)

  own_params <- flow_parameters(model, f$expr)
  # Names used elsewhere: other flows' parameters and compartment names
  other <- model$flows[-i, , drop = FALSE]
  taken <- c(
    model$compartments$name,
    unlist(lapply(other$expr, function(x) flow_parameters(model, x)))
  )
  param <- if (keep_param && length(own_params) == 1) {
    own_params
  } else {
    unique_param_name(default_param_base(f$type, f$to), taken)
  }

  inf_ids <- split_ids(f$infectious)
  inf_names <- node_name(model, inf_ids[inf_ids %in% model$compartments$id])
  type <- f$type
  # A flow coming from the Source has no origin compartment to multiply by
  if (identical(f$from, SOURCE_ID) && type != "constant") type <- "constant"
  flow_formula(type, node_name(model, f$from), inf_names, param)
}

#' Remove compartments (and every flow touching them) and flows from a model.
remove_elements <- function(model, comp_ids = character(0), flow_ids = character(0)) {
  comps <- model$compartments
  flows <- model$flows
  comps <- comps[!comps$id %in% comp_ids, , drop = FALSE]
  flows <- flows[!flows$id %in% flow_ids &
                 !flows$from %in% comp_ids & !flows$to %in% comp_ids, , drop = FALSE]
  # Forget deleted compartments in the "infectious" lists
  flows$infectious <- vapply(flows$infectious, function(x) {
    paste(setdiff(split_ids(x), comp_ids), collapse = ",")
  }, character(1), USE.NAMES = FALSE)
  model$compartments <- comps
  model$flows <- flows
  model
}

#' Rename a compartment and update every formula that uses its name.
rename_compartment <- function(model, id, new_name) {
  i <- match(id, model$compartments$id)
  old <- model$compartments$name[i]
  model$compartments$name[i] <- new_name
  model$flows$expr <- vapply(model$flows$expr, rename_symbol, character(1),
                             old = old, new = new_name, USE.NAMES = FALSE)
  model
}

split_ids <- function(x) {
  x <- x %||% ""
  if (is.na(x) || !nzchar(x)) return(character(0))
  strsplit(x, ",", fixed = TRUE)[[1]]
}
