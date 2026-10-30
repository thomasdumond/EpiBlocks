# =============================================================================
# templates.R -- ready-made models users can load and then modify
# =============================================================================
#
# Each template is a "spec" (see model_io.R). To add a template, copy one of
# the blocks below, change it, and add it to TEMPLATES at the bottom.
# Positions (x, y) are in pixels on the diagram; y grows downwards.
# =============================================================================

tpl_SI <- list(
  title = "SI model",
  description = paste(
    "Susceptible animals become Infected and stay infected for life",
    "(e.g. a persistent infection with no recovery). Eventually every animal is infected."
  ),
  reference = "Keeling MJ, Rohani P (2008) Modeling Infectious Diseases in Humans and Animals. Princeton University Press.",
  compartments = data.frame(name = c("S", "I"), init = c(999, 1), in_N = TRUE,
                            x = c(0, 250), y = c(0, 0)),
  flows = data.frame(from = "S", to = "I", type = "infection_freq",
                     expr = "beta * S * I / N", infectious = "I"),
  parameters = list(beta = 0.3),
  settings = list(t_end = 60, dt = 0.5),
  special_positions = list(list(id = "SOURCE", x = -150, y = -150),
                           list(id = "SINK", x = 400, y = 150))
)

tpl_SIR <- list(
  title = "SIR model",
  description = paste(
    "Susceptible animals become Infected, then Recover with lifelong immunity.",
    "The basic reproduction number is R0 = beta / gamma (here 3)."
  ),
  reference = "Kermack WO, McKendrick AG (1927) A contribution to the mathematical theory of epidemics. Proc R Soc Lond A 115:700-721.",
  compartments = data.frame(name = c("S", "I", "R"), init = c(999, 1, 0), in_N = TRUE,
                            x = c(0, 220, 440), y = c(0, 0, 0)),
  flows = data.frame(from = c("S", "I"), to = c("I", "R"),
                     type = c("infection_freq", "linear"),
                     expr = c("beta * S * I / N", "gamma * I"),
                     infectious = c("I", "")),
  parameters = list(beta = 0.3, gamma = 0.1),
  settings = list(t_end = 100, dt = 0.5),
  special_positions = list(list(id = "SOURCE", x = -150, y = -150),
                           list(id = "SINK", x = 590, y = 150))
)

tpl_SEIR <- list(
  title = "SEIR model",
  description = paste(
    "Like SIR, with an Exposed (latent) stage: infected animals are not yet infectious.",
    "Mean latent period = 1 / sigma, mean infectious period = 1 / gamma."
  ),
  reference = "Keeling MJ, Rohani P (2008) Modeling Infectious Diseases in Humans and Animals. Princeton University Press.",
  compartments = data.frame(name = c("S", "E", "I", "R"), init = c(999, 0, 1, 0), in_N = TRUE,
                            x = c(0, 200, 400, 600), y = 0),
  flows = data.frame(from = c("S", "E", "I"), to = c("E", "I", "R"),
                     type = c("infection_freq", "linear", "linear"),
                     expr = c("beta * S * I / N", "sigma * E", "gamma * I"),
                     infectious = c("I", "", "")),
  parameters = list(beta = 0.5, sigma = 0.2, gamma = 0.1),
  settings = list(t_end = 150, dt = 0.5),
  special_positions = list(list(id = "SOURCE", x = -150, y = -150),
                           list(id = "SINK", x = 750, y = 150))
)

tpl_SIS <- list(
  title = "SIS model",
  description = paste(
    "Infected animals recover but gain no immunity: they become Susceptible again.",
    "The infection settles at an endemic level when R0 = beta / gamma > 1."
  ),
  reference = "Keeling MJ, Rohani P (2008) Modeling Infectious Diseases in Humans and Animals. Princeton University Press.",
  compartments = data.frame(name = c("S", "I"), init = c(999, 1), in_N = TRUE,
                            x = c(0, 280), y = c(0, 0)),
  flows = data.frame(from = c("S", "I"), to = c("I", "S"),
                     type = c("infection_freq", "linear"),
                     expr = c("beta * S * I / N", "gamma * I"),
                     infectious = c("I", "")),
  parameters = list(beta = 0.3, gamma = 0.1),
  settings = list(t_end = 150, dt = 0.5),
  special_positions = list(list(id = "SOURCE", x = -150, y = -150),
                           list(id = "SINK", x = 430, y = 150))
)

tpl_SLADR <- list(
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

# Menu label = template. The first entry is loaded when the app starts.
TEMPLATES <- list(
  "SIR"   = tpl_SIR,
  "SI"    = tpl_SI,
  "SIS"   = tpl_SIS,
  "SEIR"  = tpl_SEIR,
  "SLADR (white spot disease, shrimp)" = tpl_SLADR
)
