# =============================================================================
# rcode.R -- the model as a standalone R script (the "R code" tab)
# =============================================================================
#
# Produces a script users can copy into R or R Markdown: parameters (with the
# values currently chosen in the app), initial values, time points, the model
# function in deSolve style, the ode() call and a quick plot.
# =============================================================================

#' Format a number for R code: 0.3 -> "0.3", 1e-04 -> "1e-04", 1000 -> "1000".
r_num <- function(x) {
  vapply(x, function(v) {
    if (is.na(v)) return("NA")
    format(v, digits = 15, scientific = abs(v) < 1e-4 && v != 0, trim = TRUE)
  }, "")
}

#' Right-hand side of one ODE as R code, e.g. "beta * S * I / N - gamma * I".
r_rhs <- function(model, comp_id) {
  flows <- model$flows
  terms <- character(0)
  for (j in seq_len(nrow(flows))) {
    expr <- trimws(flows$expr[j])
    p <- parse_rate(expr)
    is_sum_expr <- p$ok && is_sum(p$expr)
    if (flows$to[j] == comp_id) terms <- c(terms, paste("+", expr))
    if (flows$from[j] == comp_id) {
      terms <- c(terms, paste("-", if (is_sum_expr) paste0("(", expr, ")") else expr))
    }
  }
  if (length(terms) == 0) return("0")
  rhs <- paste(terms, collapse = " ")
  rhs <- sub("^\\+ ", "", rhs)  # "+ a - b" -> "a - b"
  sub("^- ", "-", rhs)          # "- a"     -> "-a"
}

#' The whole script as one string.
#'
#' @param model  the model
#' @param params named numeric vector of parameter values (from the side panel)
#' @param t_end,dt simulation settings
model_r_code <- function(model, params, t_end, dt) {
  comps <- model$compartments
  flows <- model$flows
  pnames <- detect_parameters(model)
  title <- model$meta$title %||% ""

  # Helper: "name = value" lines, aligned
  assign_lines <- function(nm, val, comment = NULL) {
    w <- max(nchar(nm))
    lines <- paste0("  ", formatC(nm, width = -w), " = ", val)
    if (!is.null(comment)) lines <- paste0(lines, ifelse(nzchar(comment), paste0("  # ", comment), ""))
    n <- length(lines)
    if (n > 1) lines[-n] <- sub("^(.*?)( *#.*)?$", "\\1,\\2", lines[-n], perl = TRUE)
    lines
  }

  # --- Header ---------------------------------------------------------------
  out <- c(
    paste0("# ", if (nzchar(title)) title else "Compartmental model", " (built with EpiBlocks)"),
    if (nzchar(model$meta$reference %||% "")) paste0("# Reference: ", model$meta$reference),
    "# Requires the deSolve package: install.packages(\"deSolve\")",
    "",
    "library(deSolve)",
    ""
  )

  # --- Flows as comments (what each arrow means) ------------------------------
  if (nrow(flows)) {
    labels <- vapply(seq_len(nrow(flows)), function(i) flow_label(model, flows[i, ]), "")
    labels <- gsub("→", "->", labels)
    type_labels <- stats::setNames(names(FLOW_TYPES), FLOW_TYPES)
    w <- max(nchar(labels))
    out <- c(out, "# Flows (arrows) and their rates:",
             paste0("#   ", formatC(labels, width = -w), " : ", flows$expr,
                    "   (", type_labels[flows$type], ")"),
             "")
  }

  # --- Parameters -------------------------------------------------------------
  out <- c(out, "# Parameters (values chosen in the app: change them here)")
  if (length(pnames)) {
    vals <- params[pnames]
    out <- c(out, "params <- c(", assign_lines(pnames, r_num(vals)), ")", "")
  } else {
    out <- c(out, "params <- c()", "")
  }

  # --- Initial values -----------------------------------------------------------
  out <- c(out,
           "# Initial values (number at time 0)",
           "init <- c(", assign_lines(comps$name, r_num(comps$init)), ")",
           "")

  # --- Times --------------------------------------------------------------------
  out <- c(out,
           "# Time points",
           paste0("times <- seq(0, ", r_num(t_end), ", by = ", r_num(dt), ")"),
           "")

  # --- Model function -----------------------------------------------------------
  uses_N <- any(vapply(flows$expr, function(x) "N" %in% parse_rate(x)$symbols, TRUE))
  in_N <- comps$name[comps$in_N]
  body <- character(0)
  if (uses_N) {
    body <- c(body, paste0("  N <- ", if (length(in_N)) paste(in_N, collapse = " + ") else "0",
                           "   # total population counted in N"))
  }
  dnames <- paste0("d", comps$name)
  w <- max(nchar(dnames))
  body <- c(body, paste0("  ", formatC(dnames, width = -w), " <- ",
                         vapply(comps$id, function(id) r_rhs(model, id), "")))
  body <- c(body, paste0("  list(c(", paste(dnames, collapse = ", "), "))"))

  out <- c(out,
           "# Model: for each compartment, rate of change = flows in - flows out",
           "model <- function(t, y, p) with(as.list(c(y, p)), {",
           body,
           "})",
           "")

  # --- Solve and plot -------------------------------------------------------------
  out <- c(out,
           "# Solve the ODEs",
           "out <- as.data.frame(ode(y = init, times = times, func = model,",
           "                         parms = params, method = \"lsoda\"))",
           "head(out)",
           "",
           "# Plot",
           "matplot(out$time, out[, -1], type = \"l\", lty = 1, lwd = 2,",
           "        col = seq_along(init), xlab = \"Time\", ylab = \"Number\")",
           "legend(\"right\", legend = names(init), col = seq_along(init), lty = 1, lwd = 2)")

  paste(out, collapse = "\n")
}
