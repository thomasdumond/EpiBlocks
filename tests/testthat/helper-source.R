# Load the app's logic (everything in R/) before the tests run.
# testthat runs tests from tests/testthat, so the app folder is two levels up.
app_dir <- normalizePath(file.path("..", ".."))
for (f in list.files(file.path(app_dir, "R"), pattern = "\\.R$", full.names = TRUE)) {
  source(f, local = FALSE)
}

# Small helper: build a model from a spec (same format as templates)
model_of <- function(spec) model_from_spec(spec)$model

# A model with features the menu templates do not have: two infectious boxes
# (A and D), and boxes that do not count towards N (D and R). It was the
# SLADR menu template (white spot disease in shrimp) before that was removed.
spec_SLADR <- list(
  title = "SLADR model: white spot disease in shrimp",
  description = paste(
    "White spot syndrome virus (WSSV) in a shrimp pond or tank.",
    "Susceptible shrimp become Latently infected, then Acutely ill, then Dead.",
    "Transmission comes from contact with acute shrimp (beta_A) and from eating dead shrimp",
    "(cannibalism, beta_D). Sick shrimp are eaten by others (c_A), and carcasses disappear",
    "by decay (delta) or by being eaten (c_D); both end in Removed.",
    "Only living shrimp (S, L, A) count towards N. Time is in days.",
    "Parameter values are illustrative: adapt them from the paper or your own data."
  ),
  reference = paste(
    "Adapted from Lotz JM, Soto MA (2002) Model of white spot syndrome virus (WSSV)",
    "epidemics in Litopenaeus vannamei. Dis Aquat Org 50:199-209."
  ),
  compartments = data.frame(
    name = c("S", "L", "A", "D", "R"),
    init = c(995, 0, 5, 0, 0),
    in_N = c(TRUE, TRUE, TRUE, FALSE, FALSE),
    x = c(0, 380, 580, 780, 680),
    y = c(0, 0, 0, 0, 200)
  ),
  flows = data.frame(
    from = c("S", "L", "A", "A", "D"),
    to   = c("L", "A", "D", "R", "R"),
    type = c("custom", "linear", "linear", "linear", "custom"),
    expr = c(
      "beta_A * S * A / N + beta_D * S * D / N",  # contact with acute + eating carcasses
      "sigma * L",                                # end of latent period
      "mu * A",                                   # death from disease
      "c_A * A",                                  # sick shrimp eaten by others
      "(delta + c_D) * D"                         # carcasses decay or are eaten
    ),
    infectious = c("A,D", "", "", "", "")
  ),
  parameters = list(beta_A = 0.3, beta_D = 1.5, sigma = 1, mu = 0.5,
                    c_A = 0.1, delta = 0.3, c_D = 0.5),
  settings = list(t_end = 40, dt = 0.1),
  special_positions = list(list(id = "SOURCE", x = -150, y = -150),
                           list(id = "SINK", x = 930, y = 200))
)
