# Tests for flow types, renaming, save/load and LaTeX output

test_that("flow types pre-fill the expected formulas", {
  expect_equal(flow_formula("infection_freq", "S", "I", "beta"), "beta * S * I / N")
  expect_equal(flow_formula("infection_dens", "S", "I", "beta"), "beta * S * I")
  expect_equal(flow_formula("infection_freq", "S", c("A", "D"), "beta"), "beta * S * (A + D) / N")
  expect_equal(flow_formula("linear", "E", character(0), "sigma"), "sigma * E")
  expect_equal(flow_formula("constant", "Source", character(0), "lambda"), "lambda")
  expect_equal(unique_param_name("beta", c("beta", "beta2")), "beta3")
})

test_that("renaming a compartment updates every formula", {
  m <- model_of(tpl_SIR)
  m <- rename_compartment(m, m$compartments$id[2], "Inf1")
  expect_equal(m$compartments$name, c("S", "Inf1", "R"))
  expect_equal(m$flows$expr, c("beta * S * Inf1 / N", "gamma * Inf1"))
})

test_that("deleting a compartment removes its flows", {
  m <- model_of(tpl_SIR)
  m2 <- remove_elements(m, comp_ids = m$compartments$id[2])
  expect_equal(nrow(m2$compartments), 2)
  expect_equal(nrow(m2$flows), 0)
})

test_that("every template loads and simulates without errors", {
  for (name in names(TEMPLATES)) {
    s <- model_from_spec(TEMPLATES[[name]])
    res <- run_simulation(s$model, s$model$params, s$settings$t_end, s$settings$dt)
    expect_true(res$ok, info = name)
    expect_length(res$warnings, 0)
  }
})

test_that("a saved model opens again identically", {
  s <- model_from_spec(tpl_SLADR)
  spec <- spec_from_state(s$model, s$model$params, s$xy, s$settings)
  path <- tempfile(fileext = ".json")
  writeLines(spec_to_json(spec), path)

  s2 <- model_from_spec(spec_from_json_file(path))
  expect_equal(s2$model$compartments[, -1], s$model$compartments[, -1])
  expect_equal(s2$model$flows$expr, s$model$flows$expr)
  expect_equal(s2$model$params, s$model$params)

  r1 <- run_simulation(s$model, s$model$params, 40, 0.5)
  r2 <- run_simulation(s2$model, s2$model$params, 40, 0.5)
  expect_equal(r1$data, r2$data)
})

test_that("bad files give friendly errors", {
  path <- tempfile(fileext = ".json")
  writeLines("this is not json", path)
  expect_error(spec_from_json_file(path), "not a valid EpiBlocks model")

  writeLines('{"something": 1}', path)
  expect_error(spec_from_json_file(path), "no compartments")

  spec <- tpl_SIR
  spec$flows$to[1] <- "Zombie"
  expect_error(model_from_spec(spec), "does not exist: Zombie")
})

test_that("equations are written in LaTeX", {
  expect_equal(latex_name("beta"), "\\beta")
  expect_equal(latex_name("beta_A"), "\\beta_{A}")
  expect_equal(latex_name("I2"), "I_{2}")
  expect_equal(latex_name("Dead"), "\\mathrm{Dead}")
  expect_equal(expr_to_latex(parse_rate("beta * S * I / N")$expr), "\\frac{\\beta \\, S \\, I}{N}")

  tex <- model_equations_latex(model_of(tpl_SIR))
  expect_match(tex, "\\frac{dS}{dt} &= - \\frac{\\beta \\, S \\, I}{N}", fixed = TRUE)
  expect_match(tex, "N &= S + I + R", fixed = TRUE)
})
