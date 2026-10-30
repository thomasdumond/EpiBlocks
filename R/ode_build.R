# =============================================================================
# ode_build.R -- from boxes and arrows to a system of ODEs
# =============================================================================
#
# Each arrow (flow) moves animals from one compartment to another at a rate
# given by its formula. The ODE for a compartment X is therefore
#
#     dX/dt = (sum of rates of arrows into X) - (sum of rates of arrows out of X)
#
# Arrows from the Source only add, arrows into the Sink only remove.
#
# We store this bookkeeping in a "stoichiometry matrix" M with one row per
# compartment and one column per flow: M[X, f] = +1 if flow f enters X,
# -1 if it leaves X, 0 otherwise. Then  dy/dt = M %*% rates.
# =============================================================================

#' Check a model and collect everything needed to simulate it.
#'
#' @return A list with
#'   errors   character vector of friendly messages; empty if the model is OK
#'   warnings character vector of non-blocking remarks
#'   params   parameter names detected in the formulas
#'   exprs    list of parsed formulas (one per flow), when there are no errors
analyse_model <- function(model) {
  comps <- model$compartments
  flows <- model$flows
  errors <- character(0)
  warnings <- character(0)

  if (nrow(comps) == 0) {
    errors <- c(errors, "Add at least one compartment (box) to the diagram.")
  }

  # --- Compartments ---------------------------------------------------------
  for (i in seq_len(nrow(comps))) {
    nm <- comps$name[i]
    msg <- check_name(nm, taken = comps$name[-i])
    if (!is.null(msg)) errors <- c(errors, paste0("Compartment '", nm, "': ", msg))
    if (is.na(comps$init[i])) {
      errors <- c(errors, paste0("Compartment '", nm, "' has no initial value."))
    } else if (comps$init[i] < 0) {
      errors <- c(errors, paste0("Compartment '", nm, "' has a negative initial value."))
    }
  }

  # --- Flows ----------------------------------------------------------------
  valid_ends <- c(comps$id, SOURCE_ID, SINK_ID)
  exprs <- vector("list", nrow(flows))
  symbols <- character(0)
  for (i in seq_len(nrow(flows))) {
    f <- flows[i, ]
    label <- flow_label(model, f)
    if (!f$from %in% valid_ends || !f$to %in% valid_ends) {
      errors <- c(errors, paste0("The flow ", label, " points to a compartment that no longer exists. Delete it and draw it again."))
      next
    }
    if (f$from == f$to) {
      errors <- c(errors, paste0("The flow ", label, " starts and ends at the same compartment."))
    }
    if (f$from == SOURCE_ID && f$to == SINK_ID) {
      errors <- c(errors, "A flow cannot go directly from the Source to the Sink.")
    }
    p <- parse_rate(f$expr)
    if (!p$ok) {
      errors <- c(errors, paste0("Flow ", label, ": ", p$error))
    } else {
      exprs[[i]] <- p$expr
      symbols <- c(symbols, p$symbols)
    }
  }

  params <- setdiff(unique(symbols), c(comps$name, RESERVED_NAMES))

  if ("N" %in% symbols && !any(comps$in_N)) {
    errors <- c(errors, "A formula uses N, but no compartment is ticked as counting towards N.")
  }
  # Infection flows need at least one infectious animal at time 0, otherwise
  # beta * S * I / N stays 0 for ever and the epicurve is flat
  for (i in seq_len(nrow(flows))) {
    if (!is_infection_type(flows$type[i])) next
    inf <- comps[comps$id %in% split_ids(flows$infectious[i]), , drop = FALSE]
    if (nrow(inf) > 0 && all(!is.na(inf$init) & inf$init == 0)) {
      warnings <- c(warnings, paste0(
        "Flow ", flow_label(model, flows[i, ]), ": there are no infectious animals at time 0 (",
        paste0(inf$name, " = 0", collapse = ", "), "), so the infection cannot start. ",
        "Click ", inf$name[1], " and give it an initial value above 0 (e.g. 1)."
      ))
    }
  }

  if (nrow(flows) == 0 && nrow(comps) > 0) {
    warnings <- c(warnings, "There are no flows (arrows) yet, so nothing changes over time.")
  }

  list(errors = errors, warnings = warnings, params = params, exprs = exprs)
}


#' Stoichiometry matrix: rows = compartments, columns = flows.
stoichiometry <- function(model) {
  comps <- model$compartments
  flows <- model$flows
  M <- matrix(0, nrow = nrow(comps), ncol = nrow(flows),
              dimnames = list(comps$name, flows$id))
  for (j in seq_len(nrow(flows))) {
    from <- match(flows$from[j], comps$id)  # NA for the Source
    to   <- match(flows$to[j], comps$id)    # NA for the Sink
    if (!is.na(from)) M[from, j] <- M[from, j] - 1
    if (!is.na(to))   M[to, j]   <- M[to, j] + 1
  }
  M
}


#' Build the derivative function dy/dt = f(t, y) for a checked model.
#'
#' @param model    the model
#' @param analysis output of analyse_model(model) (must have no errors)
#' @param params   named numeric vector of parameter values
#' @return function(t, y) returning the vector of derivatives, in the same
#'   order as model$compartments.
build_rhs <- function(model, analysis, params) {
  comps <- model$compartments
  M <- stoichiometry(model)
  exprs <- analysis$exprs
  in_N <- comps$in_N
  comp_names <- comps$name
  param_values <- as.list(params[analysis$params])
  flow_labels <- vapply(seq_len(nrow(model$flows)),
                        function(i) flow_label(model, model$flows[i, ]), "")

  function(t, y) {
    # One small environment per evaluation: compartment values, parameters,
    # N and t. Its parent only contains the whitelisted functions.
    env <- new.env(parent = safe_eval_env())
    values <- c(stats::setNames(as.list(y), comp_names), param_values,
                list(N = sum(y[in_N]), t = t))
    list2env(values, envir = env)

    rates <- numeric(length(exprs))
    for (j in seq_along(exprs)) {
      r <- eval(exprs[[j]], env)
      if (length(r) != 1 || !is.numeric(r)) {
        stop("The rate of flow ", flow_labels[j], " is not a single number.", call. = FALSE)
      }
      rates[j] <- r
    }
    dy <- as.vector(M %*% rates)
    names(dy) <- comp_names
    dy
  }
}


#' The ODE system as text, one line per compartment, e.g.
#'   "dS/dt = - beta * S * I / N"
model_equations_text <- function(model) {
  comps <- model$compartments
  flows <- model$flows
  vapply(seq_len(nrow(comps)), function(i) {
    id <- comps$id[i]
    ins  <- flows$expr[flows$to == id]
    outs <- flows$expr[flows$from == id]
    terms <- c(
      if (length(ins))  paste0("+ (", ins, ")"),
      if (length(outs)) paste0("- (", outs, ")")
    )
    rhs <- if (length(terms)) paste(terms, collapse = " ") else "0"
    paste0("d", comps$name[i], "/dt = ", rhs)
  }, character(1))
}
