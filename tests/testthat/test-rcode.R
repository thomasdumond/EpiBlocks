# Tests for the "R code" tab (R/rcode.R): the generated script must run on
# its own and give the same results as the app.

run_generated <- function(code) {
  env <- new.env()
  grDevices::pdf(NULL)                      # the script draws a plot
  on.exit(grDevices::dev.off())
  eval(parse(text = code), envir = env)
  env$out
}

test_that("the generated script reproduces the app for every template", {
  skip_if_not_installed("deSolve")
  for (name in names(TEMPLATES)) {
    s <- model_from_spec(TEMPLATES[[name]])
    t_end <- s$settings$t_end; dt <- s$settings$dt
    code <- model_r_code(s$model, s$model$params, t_end, dt)
    app <- run_simulation(s$model, s$model$params, t_end, dt)$data
    expect_equal(as.matrix(run_generated(code)), as.matrix(app),
                 tolerance = 1e-8, info = name)
  }
})

test_that("the script uses the parameter values chosen in the app", {
  s <- model_from_spec(tpl_SIR)
  code <- model_r_code(s$model, c(beta = 0.45, gamma = 0.125), 50, 1)
  expect_match(code, "beta  = 0.45", fixed = TRUE)
  expect_match(code, "gamma = 0.125", fixed = TRUE)
  expect_match(code, "times <- seq(0, 50, by = 1)", fixed = TRUE)
  expect_match(code, "dS <- -beta * S * I / N", fixed = TRUE)
  expect_match(code, "N <- S + I + R", fixed = TRUE)
})

test_that("open models (Source and Sink) are written correctly", {
  skip_if_not_installed("deSolve")
  spec <- list(
    compartments = data.frame(name = c("S", "I"), init = c(100, 5), in_N = TRUE),
    flows = data.frame(from = c("SOURCE", "S", "S", "I"), to = c("S", "I", "SINK", "SINK"),
                       type = c("constant", "infection_freq", "linear", "linear"),
                       expr = c("lambda", "beta * S * I / N", "mu * S", "(mu + alpha) * I"),
                       infectious = c("", "I", "", ""))
  )
  m <- model_of(spec)
  p <- c(lambda = 2, beta = 0.4, mu = 0.02, alpha = 0.1)
  code <- model_r_code(m, p, 60, 1)
  expect_match(code, "dS <- lambda - beta * S * I / N - mu * S", fixed = TRUE)
  expect_match(code, "dI <- beta * S * I / N - (mu + alpha) * I", fixed = TRUE)
  expect_equal(as.matrix(run_generated(code)),
               as.matrix(run_simulation(m, p, 60, 1)$data), tolerance = 1e-8)
})
