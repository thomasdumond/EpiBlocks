# =============================================================================
# solver.R -- solving the ODEs
# =============================================================================
#
# We use deSolve::ode() (method "lsoda", which adapts its step size and copes
# with stiff systems). If deSolve is not installed -- e.g. when the app runs
# in the browser with shinylive/webR, where deSolve is not available -- we
# fall back to a simple fixed-step Runge-Kutta 4 solver written in base R.
# =============================================================================

#' Fixed-step 4th-order Runge-Kutta solver (fallback when deSolve is missing).
#'
#' @param y0    named initial values
#' @param times output times (equally spaced)
#' @param rhs   function(t, y) returning dy/dt
#' @param max_h largest internal step; each output interval is split into
#'              sub-steps no longer than this
#' @return matrix with a "time" column and one column per compartment
rk4_solve <- function(y0, times, rhs, max_h = 0.05) {
  out <- matrix(NA_real_, nrow = length(times), ncol = length(y0) + 1,
                dimnames = list(NULL, c("time", names(y0))))
  y <- y0
  out[1, ] <- c(times[1], y)
  for (k in seq_along(times)[-1]) {
    t0 <- times[k - 1]
    n_sub <- max(1, ceiling((times[k] - t0) / max_h))
    h <- (times[k] - t0) / n_sub
    for (s in seq_len(n_sub)) {
      t <- t0 + (s - 1) * h
      k1 <- rhs(t, y)
      k2 <- rhs(t + h / 2, y + h / 2 * k1)
      k3 <- rhs(t + h / 2, y + h / 2 * k2)
      k4 <- rhs(t + h, y + h * k3)
      y <- y + h / 6 * (k1 + 2 * k2 + 2 * k3 + k4)
    }
    out[k, ] <- c(times[k], y)
    if (any(!is.finite(y))) break
  }
  out
}


#' Simulate a model.
#'
#' @param model  the model (see model.R)
#' @param params named numeric vector of parameter values
#' @param t_end  end time
#' @param dt     time step between output points
#' @param use_desolve set to FALSE to force the base-R RK4 solver
#' @return A list with
#'   ok       TRUE if a result was produced
#'   data     data frame: time + one column per compartment
#'   errors   friendly error messages (when ok is FALSE)
#'   warnings friendly warnings (the result is still shown)
#'   solver   which solver was used
run_simulation <- function(model, params, t_end = 100, dt = 1,
                           use_desolve = requireNamespace("deSolve", quietly = TRUE)) {
  fail <- function(msgs, warnings = character(0)) {
    list(ok = FALSE, data = NULL, errors = msgs, warnings = warnings, solver = NA)
  }

  # --- Check the model first --------------------------------------------------
  analysis <- analyse_model(model)
  if (length(analysis$errors)) return(fail(analysis$errors, analysis$warnings))

  # --- Check simulation settings -------------------------------------------
  if (!is.numeric(t_end) || is.na(t_end) || t_end <= 0) return(fail("The end time must be a positive number."))
  if (!is.numeric(dt) || is.na(dt) || dt <= 0) return(fail("The time step must be a positive number."))
  if (t_end / dt > 20000) return(fail("Too many time points: please use a larger time step or a shorter end time."))

  # --- Parameter values ----------------------------------------------------
  missing <- setdiff(analysis$params, names(params))
  if (length(missing)) {
    return(fail(paste0("Missing value for parameter(s): ", paste(missing, collapse = ", "), ".")))
  }
  bad <- analysis$params[!is.finite(params[analysis$params])]
  if (length(bad)) {
    return(fail(paste0("Parameter(s) without a valid number: ", paste(bad, collapse = ", "), ".")))
  }

  rhs <- build_rhs(model, analysis, params)
  y0 <- stats::setNames(model$compartments$init, model$compartments$name)
  times <- seq(0, t_end, by = dt)
  if (utils::tail(times, 1) < t_end) times <- c(times, t_end)

  # Try the derivatives once at t = 0 to catch formula problems early with a
  # clear message (e.g. log of a negative number, wrong number of arguments)
  first <- tryCatch(suppressWarnings(rhs(0, y0)), error = function(e) e)
  if (inherits(first, "error")) {
    return(fail(paste0("A rate could not be calculated: ", conditionMessage(first))))
  }
  if (any(!is.finite(first))) {
    bad <- names(first)[!is.finite(first)]
    return(fail(paste0(
      "At time 0 the rates give an invalid value for: ", paste(bad, collapse = ", "),
      ". Check for division by zero (e.g. N = 0) or log/sqrt of negative values."
    )))
  }

  # Nothing moves at time 0 (and so never will, unless a rate depends on t)
  if (nrow(model$flows) > 0 && all(first == 0) && length(analysis$warnings) == 0) {
    analysis$warnings <- c(analysis$warnings, paste0(
      "All flows are zero at time 0, so the curves stay flat. ",
      "Check the initial values: e.g. an infection needs at least one infectious animal."
    ))
  }

  # --- Solve -----------------------------------------------------------------
  solver_warnings <- character(0)
  out <- withCallingHandlers(
    tryCatch({
      if (use_desolve) {
        # capture.output() hides lsoda's console messages; problems are
        # still reported as R warnings and turned into a friendly message
        res <- NULL
        utils::capture.output(
          res <- deSolve::ode(y = y0, times = times,
                              func = function(t, y, p) list(rhs(t, y)),
                              parms = NULL, method = "lsoda")
        )
        res
      } else {
        rk4_solve(y0, times, rhs)
      }
    }, error = function(e) e),
    warning = function(w) {
      solver_warnings <<- c(solver_warnings, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  if (inherits(out, "error")) {
    return(fail(paste0("The solver stopped: ", conditionMessage(out))))
  }

  data <- as.data.frame(unclass(out))[, c("time", model$compartments$name), drop = FALSE]
  data <- data[stats::complete.cases(data), , drop = FALSE]
  values <- as.matrix(data[, -1, drop = FALSE])

  warnings <- analysis$warnings
  if (nrow(data) < length(times) || any(!is.finite(values)) || length(solver_warnings)) {
    warnings <- c(warnings, paste0(
      "The solver had trouble and the results may stop early or be inaccurate. ",
      "Check for very large rates, division by N when N can be 0, or log/sqrt of negative values."
    ))
  }
  # Ignore tiny negative values caused by rounding
  tol <- 1e-6 * max(1, max(abs(values[is.finite(values)]), 0))
  neg <- colnames(values)[apply(values, 2, function(v) any(v < -tol, na.rm = TRUE))]
  if (length(neg)) {
    warnings <- c(warnings, paste0(
      "Negative values in: ", paste(neg, collapse = ", "),
      ". A flow is probably removing more animals than the compartment contains ",
      "(e.g. a constant outflow). Make outflow rates proportional to the origin compartment."
    ))
  }

  rownames(data) <- NULL
  list(ok = TRUE, data = data, errors = character(0), warnings = warnings,
       solver = if (use_desolve) "deSolve (lsoda)" else "built-in Runge-Kutta 4")
}
