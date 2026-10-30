# Tests for reading user formulas safely (R/expr_safe.R)

test_that("valid formulas are accepted and their names found", {
  p <- parse_rate("beta * S * I / N")
  expect_true(p$ok)
  expect_setequal(p$symbols, c("beta", "S", "I", "N"))

  expect_true(parse_rate("exp(-k * t) + sqrt(2) * min(a, b) - max(c, 1e-3)")$ok)
  expect_true(parse_rate("(delta + c_D) * D")$ok)
  expect_true(parse_rate("-mu * S")$ok)
  expect_true(parse_rate("a ^ 2")$ok)
})

test_that("dangerous or unknown functions are rejected", {
  for (bad in c("system('ls')", "file.remove('x')", "eval(parse(text = 'x'))",
                "Sys.time()", "get('x')", "beta * S[1]", "a$b", "x <- 1",
                "function(x) x", "S %% 2", "paste(S)")) {
    p <- parse_rate(bad)
    expect_false(p$ok, info = bad)
    expect_type(p$error, "character")
  }
})

test_that("syntax errors, empty formulas and odd values give friendly errors", {
  expect_match(parse_rate("beta * S +")$error, "could not be read")
  expect_match(parse_rate("2beta")$error, "could not be read")
  expect_match(parse_rate("")$error, "empty")
  expect_match(parse_rate("   ")$error, "empty")
  expect_match(parse_rate("a; b")$error, "single formula")
  expect_false(parse_rate("'text'")$ok)
  expect_false(parse_rate("TRUE * S")$ok)
  expect_false(parse_rate("min(a, )")$ok)
})

test_that("formulas are evaluated without access to the rest of R", {
  env <- safe_eval_env()
  expect_true(exists("exp", envir = env, inherits = FALSE))
  expect_false(exists("system", envir = env))
  expect_false(exists("eval", envir = env))
})

test_that("renaming replaces whole names only and keeps formatting", {
  expect_equal(rename_symbol("beta * S * I / N", "I", "Inf2"), "beta * S * Inf2 / N")
  expect_equal(rename_symbol("beta*S*I/N + Ia", "I", "J"), "beta*S*J/N + Ia")
  expect_equal(rename_symbol("exp(-exp)", "exp", "z"), "exp(-z)")   # function name untouched
  expect_equal(rename_symbol("beta * S +", "S", "X"), "beta * S +") # invalid: unchanged
})

test_that("compartment names are checked", {
  expect_null(check_name("S"))
  expect_null(check_name("Dead_2"))
  expect_match(check_name(""), "empty")
  expect_match(check_name("2S"), "not a valid name")
  expect_match(check_name("my box"), "not a valid name")
  expect_match(check_name("a.b"), "not a valid name")
  expect_match(check_name("N"), "reserved")
  expect_match(check_name("t"), "reserved")
  expect_match(check_name("exp"), "reserved")
  expect_match(check_name("Inf"), "not a valid name")
  expect_match(check_name("S", taken = "S"), "already used")
})
