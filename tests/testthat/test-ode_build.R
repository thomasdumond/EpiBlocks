# Tests for building and solving the ODE system (R/ode_build.R, R/solver.R)

# The SIR model written by hand, the "textbook" way, to compare against
sir_by_hand <- function(t, y, p) {
  with(as.list(c(y, p)), {
    N <- S + I + R
    dS <- -beta * S * I / N
    dI <- beta * S * I / N - gamma * I
    dR <- gamma * I
    list(c(dS, dI, dR))
  })
}

test_that("the SIR template gives the same ODEs as the textbook SIR", {
  m <- model_of(tpl_SIR)
  a <- analyse_model(m)
  expect_length(a$errors, 0)
  expect_setequal(a$params, c("beta", "gamma"))

  rhs <- build_rhs(m, a, c(beta = 0.3, gamma = 0.1))
  y <- c(S = 900, I = 80, R = 20)
  expect_equal(unname(rhs(0, y)), sir_by_hand(0, y, c(beta = 0.3, gamma = 0.1))[[1]])
})

test_that("the SIR template reproduces a direct deSolve solution", {
  skip_if_not_installed("deSolve")
  p <- c(beta = 0.3, gamma = 0.1)
  res <- run_simulation(model_of(tpl_SIR), p, t_end = 100, dt = 1)
  expect_true(res$ok)

  ref <- deSolve::ode(y = c(S = 999, I = 1, R = 0), times = 0:100,
                      func = sir_by_hand, parms = p, method = "lsoda")
  expect_equal(unname(as.matrix(res$data)), unname(unclass(ref)[, c("time", "S", "I", "R")]),
               tolerance = 1e-6)
})

test_that("the base-R RK4 fallback agrees with deSolve", {
  skip_if_not_installed("deSolve")
  p <- c(beta = 0.3, gamma = 0.1)
  a <- run_simulation(model_of(tpl_SIR), p, 100, 1, use_desolve = TRUE)
  b <- run_simulation(model_of(tpl_SIR), p, 100, 1, use_desolve = FALSE)
  expect_equal(b$solver, "built-in Runge-Kutta 4")
  expect_equal(a$data, b$data, tolerance = 1e-5)
})

test_that("closed models keep the total population constant", {
  for (tpl in list(tpl_SI, tpl_SIR, tpl_SIS, tpl_SEIR, tpl_SLADR)) {
    m <- model_of(tpl)
    res <- run_simulation(m, m$params, tpl$settings$t_end, tpl$settings$dt)
    expect_true(res$ok, info = tpl$title)
    total <- rowSums(res$data[, -1])
    expect_equal(max(total) - min(total), 0, tolerance = 1e-6, info = tpl$title)
  }
})

test_that("flows from the Source and into the Sink change the total", {
  spec <- list(
    compartments = data.frame(name = "S", init = 100, in_N = TRUE),
    flows = data.frame(from = c("SOURCE", "S"), to = c("S", "SINK"),
                       type = c("constant", "linear"), expr = c("lambda", "mu * S"),
                       infectious = "")
  )
  res <- run_simulation(model_of(spec), c(lambda = 10, mu = 0.1), t_end = 200, dt = 1)
  expect_true(res$ok)
  # Equilibrium S* = lambda / mu = 100 ... start there, stay there
  expect_equal(tail(res$data$S, 1), 100, tolerance = 1e-6)

  res2 <- run_simulation(model_of(spec), c(lambda = 0, mu = 0.1), t_end = 10, dt = 1)
  expect_equal(tail(res2$data$S, 1), 100 * exp(-1), tolerance = 1e-5)
})

test_that("N only counts the ticked compartments", {
  m <- model_of(tpl_SLADR)
  a <- analyse_model(m)
  rhs <- build_rhs(m, a, m$params)
  y <- c(S = 500, L = 0, A = 100, D = 400, R = 0)
  # N = S + L + A = 600 (dead and removed excluded)
  expected_infection <- with(as.list(m$params), beta_A * 500 * 100 / 600 + beta_D * 500 * 400 / 600)
  expect_equal(rhs(0, y)[["L"]], expected_infection)
})

test_that("problems give friendly errors instead of crashing", {
  m <- model_of(tpl_SIR)

  bad <- m; bad$flows$expr[1] <- "beta * S * I /"
  expect_match(run_simulation(bad, m$params)$errors[1], "could not be read")

  bad <- m; bad$compartments$init[2] <- NA
  expect_match(run_simulation(bad, m$params)$errors[1], "no initial value")

  bad <- m; bad$compartments$init[1] <- -5
  expect_match(run_simulation(bad, m$params)$errors[1], "negative initial value")

  bad <- m; bad$flows$to[2] <- "deleted_box"
  expect_match(run_simulation(bad, m$params)$errors[1], "no longer exists")

  bad <- m; bad$compartments$in_N <- FALSE
  expect_match(run_simulation(bad, m$params)$errors[1], "counting towards N")

  expect_match(run_simulation(m, c(beta = 0.3))$errors[1], "Missing value")
  expect_match(run_simulation(m, c(beta = NA, gamma = 0.1))$errors[1], "valid number")
  expect_match(run_simulation(m, m$params, t_end = -1)$errors[1], "end time")
  expect_match(run_simulation(m, m$params, dt = 0)$errors[1], "time step")

  bad <- m; bad$compartments$init <- c(0, 0, 0)
  expect_match(run_simulation(bad, m$params)$errors[1], "division by zero")
})

test_that("useful warnings are given", {
  m <- model_of(tpl_SIR)
  m$compartments$init[2] <- 0  # no infectious animals
  expect_match(run_simulation(m, m$params)$warnings, "no infectious animals", all = FALSE)

  m <- model_of(tpl_SIR)
  m$flows$expr[2] <- "5"       # constant outflow empties I below zero
  expect_match(run_simulation(m, m$params)$warnings, "Negative values", all = FALSE)
})

test_that("parameters are detected from formulas", {
  m <- model_of(tpl_SLADR)
  expect_setequal(detect_parameters(m),
                  c("beta_A", "beta_D", "sigma", "mu", "c_A", "delta", "c_D"))
})
