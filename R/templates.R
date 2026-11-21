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

tpl_SEEEEIIIIR <- list(
  title = "SEEEEIIIIR model: latent and infectious periods in stages",
  description = paste(
    "Like SEIR, but the latent period is split into 4 stages (E1 to E4) and the",
    "infectious period into 4 stages (I1 to I4). All E stages share one rate, sigma,",
    "and all I stages share one rate, gamma. The periods then follow a gamma",
    "(Erlang) distribution instead of an exponential one: most animals stay close",
    "to the mean, which is more realistic. Mean latent period = 4 / sigma (here 5 days),",
    "mean infectious period = 4 / gamma (here 10 days), R0 = 4 * beta / gamma (here 5)."
  ),
  reference = paste(
    "Wearing HJ, Rohani P, Keeling MJ (2005) Appropriate models for the management",
    "of infectious diseases. PLoS Med 2(7):e174."
  ),
  compartments = data.frame(
    name = c("S", paste0("E", 1:4), paste0("I", 1:4), "R"),
    init = c(999, 0, 0, 0, 0, 1, 0, 0, 0, 0),
    in_N = TRUE,
    # Zigzag left to right: S, E1, I1 and R on top, the other stages below
    x = c(0, 320, 360, 560, 750, 920, 1030, 1230, 1450, 1550),
    y = c(0, 0, 200, 200, 200, 0, 200, 200, 200, 0)
  ),
  flows = data.frame(
    from = c("S", paste0("E", 1:4), paste0("I", 1:4)),
    to   = c(paste0("E", 1:4), paste0("I", 1:4), "R"),
    type = c("infection_freq", rep("linear", 8)),
    expr = c("beta * S * (I1 + I2 + I3 + I4) / N",
             paste0("sigma * E", 1:4),
             paste0("gamma * I", 1:4)),
    infectious = c("I1,I2,I3,I4", rep("", 8))
  ),
  parameters = list(beta = 0.5, sigma = 0.8, gamma = 0.4),
  settings = list(t_end = 150, dt = 0.5),
  special_positions = list(list(id = "SOURCE", x = -150, y = -150),
                           list(id = "SINK", x = 1700, y = 350))
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

# Menu label = template. The first entry is loaded when the app starts.
TEMPLATES <- list(
  "SIR"   = tpl_SIR,
  "SI"    = tpl_SI,
  "SIS"   = tpl_SIS,
  "SEIR"  = tpl_SEIR,
  "SEEEEIIIIR" = tpl_SEEEEIIIIR
)
