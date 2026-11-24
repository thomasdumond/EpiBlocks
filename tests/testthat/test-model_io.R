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

test_that("models are closed by default; Source/Sink only when added", {
  expect_length(empty_model()$special, 0)
  # Templates are closed: no Source/Sink on the diagram
  for (name in names(TEMPLATES)) {
    m <- model_of(TEMPLATES[[name]])
    expect_length(m$special, 0)
    expect_false(any(c(SOURCE_ID, SINK_ID) %in% canvas_nodes(m, complete_positions(m, data.frame(id = character(0), x = numeric(0), y = numeric(0))))$id))
  }
  # A model whose flows use the Source and Sink shows them
  spec <- list(
    compartments = data.frame(name = "S", init = 100, in_N = TRUE),
    flows = data.frame(from = c("SOURCE", "S"), to = c("S", "SINK"),
                       type = c("constant", "linear"), expr = c("lambda", "mu * S"),
                       infectious = "")
  )
  m <- model_of(spec)
  expect_setequal(m$special, c(SOURCE_ID, SINK_ID))

  # Removing the Source also removes its arrows
  m2 <- remove_elements(m, SOURCE_ID)
  expect_equal(m2$special, SINK_ID)
  expect_equal(m2$flows$to, SINK_ID)
})

test_that("Sources and Sinks are numbered and named", {
  expect_equal(next_special_id(SOURCE_ID, character(0)), "SOURCE")
  expect_equal(next_special_id(SOURCE_ID, c("SOURCE", "SINK")), "SOURCE2")
  expect_equal(next_special_id(SINK_ID, c("SINK", "SINK2", "SOURCE")), "SINK3")
  expect_equal(next_special_id(SOURCE_ID, "SOURCE2"), "SOURCE")   # a removed one is reused
  expect_equal(special_name(c("SOURCE", "SOURCE2", "SINK", "SINK12")),
               c("Source", "Source 2", "Sink", "Sink 12"))
  expect_equal(sort_special(c("SINK2", "SOURCE3", "SINK", "SOURCE")),
               c("SOURCE", "SOURCE3", "SINK", "SINK2"))
  # Ids are never mistaken for compartments (compartment ids start with c_)
  expect_false(any(is_special_id(c("c_source", "S", "SOURCEX", "SINK_2"))))
  # ... and compartments cannot be called like them
  expect_match(check_name("SINK2"), "reserved")
  expect_null(check_name("Sink2"))
})

test_that("several Sources and Sinks: diagram, equations and save/load", {
  spec <- list(
    compartments = data.frame(name = c("S", "I"), init = c(100, 10), in_N = TRUE),
    flows = data.frame(
      from = c("SOURCE", "SOURCE2", "S", "S", "I"),
      to   = c("S", "I", "SINK", "SINK2", "SINK2"),
      type = c("constant", "constant", "linear", "linear", "linear"),
      expr = c("births", "imports", "mu * S", "cull * S", "cull * I"),
      infectious = ""
    )
  )
  s <- model_from_spec(spec)
  m <- s$model
  expect_equal(m$special, c("SOURCE", "SOURCE2", "SINK", "SINK2"))

  # All four blocks are drawn, at different places, with their own labels
  nodes <- canvas_nodes(m, s$xy)
  sp <- nodes[nodes$group == "special", ]
  expect_equal(sp$id, c("SOURCE", "SOURCE2", "SINK", "SINK2"))
  expect_equal(sub("\n.*", "", sp$label), c("Source", "Source 2", "Sink", "Sink 2"))
  expect_equal(nrow(unique(sp[, c("x", "y")])), 4)

  # Equations: each block only adds or removes
  p <- c(births = 5, imports = 1, mu = 0.1, cull = 0.2)
  rhs <- build_rhs(m, analyse_model(m), p)
  d <- rhs(0, c(S = 100, I = 10))
  expect_equal(d[["S"]], 5 - 0.1 * 100 - 0.2 * 100)
  expect_equal(d[["I"]], 1 - 0.2 * 10)
  expect_match(model_equations_latex(m), "\\frac{dI}{dt} &= \\mathrm{imports}", fixed = TRUE)

  # Save and open again: same blocks, same arrows
  path <- tempfile(fileext = ".json")
  writeLines(spec_to_json(spec_from_state(m, p, s$xy, list())), path)
  m2 <- model_from_spec(spec_from_json_file(path))$model
  expect_equal(m2$special, m$special)
  arrows <- function(m) paste(node_name(m, m$flows$from), node_name(m, m$flows$to), m$flows$expr)
  expect_equal(arrows(m2), arrows(m))   # compartments get new internal ids
  expect_equal(arrows(m)[2], "Source 2 I imports")

  # Removing Sink 2 removes only its arrows
  m3 <- remove_elements(m, "SINK2")
  expect_equal(m3$special, c("SOURCE", "SOURCE2", "SINK"))
  expect_equal(m3$flows$expr, c("births", "imports", "mu * S"))

  # A flow from any Source straight to any Sink is refused
  bad <- m; bad$flows$from[3] <- "SOURCE2"
  expect_match(analyse_model(bad)$errors, "directly from the Source to the Sink", all = FALSE)
})

test_that("an added but unused Source is remembered when saving", {
  s <- model_from_spec(tpl_SIR)
  s$model$special <- SOURCE_ID
  xy <- complete_positions(s$model, s$xy)
  path <- tempfile(fileext = ".json")
  writeLines(spec_to_json(spec_from_state(s$model, s$model$params, xy, list())), path)
  expect_equal(model_from_spec(spec_from_json_file(path))$model$special, SOURCE_ID)

  # and a closed model stays closed
  s$model$special <- character(0)
  writeLines(spec_to_json(spec_from_state(s$model, s$model$params, s$xy, list())), path)
  expect_length(model_from_spec(spec_from_json_file(path))$model$special, 0)
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
  s <- model_from_spec(spec_SLADR)
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
