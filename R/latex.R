# =============================================================================
# latex.R -- writing the ODE system as LaTeX (shown with MathJax)
# =============================================================================

GREEK <- c("alpha", "beta", "gamma", "delta", "epsilon", "zeta", "eta", "theta",
           "iota", "kappa", "lambda", "mu", "nu", "xi", "pi", "rho", "sigma",
           "tau", "upsilon", "phi", "chi", "psi", "omega",
           "Gamma", "Delta", "Theta", "Lambda", "Xi", "Pi", "Sigma", "Phi", "Psi", "Omega")

#' LaTeX for a name: beta -> \beta, beta_A -> \beta_{A}, beta2 -> \beta_{2},
#' Dead -> \mathrm{Dead}, S -> S.
latex_name <- function(name) {
  parts <- regmatches(name, regexec("^([A-Za-z]+?)(_?)([0-9]+|_.*|)$", name))[[1]]
  if (length(parts) == 0) return(paste0("\\mathrm{", gsub("_", "\\\\_", name), "}"))
  base <- parts[2]
  sub <- sub("^_", "", parts[4])
  if (!nzchar(parts[3]) && !nzchar(parts[4])) sub <- ""
  base_tex <- if (base %in% GREEK) {
    paste0("\\", base)
  } else if (nchar(base) == 1) {
    base
  } else {
    paste0("\\mathrm{", base, "}")
  }
  if (nzchar(sub)) paste0(base_tex, "_{", gsub("_", ",", sub), "}") else base_tex
}

#' Is this (parsed) expression a sum or difference at the top level?
is_sum <- function(e) {
  is.call(e) && as.character(e[[1]]) %in% c("+", "-") && length(e) == 3
}

#' Convert a checked rate expression (from parse_rate) to LaTeX.
expr_to_latex <- function(e) {
  if (is.numeric(e)) return(format(e, scientific = FALSE, drop0trailing = TRUE))
  if (is.symbol(e)) return(latex_name(as.character(e)))

  op <- as.character(e[[1]])
  args <- as.list(e)[-1]
  tex <- lapply(args, expr_to_latex)
  # Wrap sums in brackets when they appear inside a product or power
  wrap <- function(i) {
    if (is_sum(args[[i]])) paste0("\\left(", tex[[i]], "\\right)") else tex[[i]]
  }

  switch(op,
    "+" = if (length(args) == 1) tex[[1]] else paste(tex[[1]], "+", tex[[2]]),
    "-" = if (length(args) == 1) paste0("-", wrap(1)) else paste(tex[[1]], "-", wrap(2)),
    "*" = {
      # Show a dot between two numbers or before a number, else just a space
      sep <- if (is.numeric(args[[2]])) " \\cdot " else " \\, "
      paste0(wrap(1), sep, wrap(2))
    },
    "/" = paste0("\\frac{", tex[[1]], "}{", tex[[2]], "}"),
    "^" = paste0("{", wrap(1), "}^{", tex[[2]], "}"),
    "(" = paste0("\\left(", tex[[1]], "\\right)"),
    "exp" = paste0("e^{", tex[[1]], "}"),
    "log" = paste0("\\ln\\left(", tex[[1]], "\\right)"),
    "sqrt" = paste0("\\sqrt{", tex[[1]], "}"),
    "min" = paste0("\\min\\left(", paste(tex, collapse = ", "), "\\right)"),
    "max" = paste0("\\max\\left(", paste(tex, collapse = ", "), "\\right)"),
    paste0("\\mathrm{", op, "}\\left(", paste(tex, collapse = ", "), "\\right)")
  )
}

#' The whole ODE system as one LaTeX "aligned" block, plus the definition
#' of N. Flows with an invalid formula are shown as "?".
model_equations_latex <- function(model) {
  comps <- model$compartments
  flows <- model$flows

  flow_tex <- vapply(flows$expr, function(x) {
    p <- parse_rate(x)
    if (p$ok) expr_to_latex(p$expr) else "\\text{?}"
  }, "", USE.NAMES = FALSE)
  flow_is_sum <- vapply(flows$expr, function(x) {
    p <- parse_rate(x)
    p$ok && is_sum(p$expr)
  }, TRUE, USE.NAMES = FALSE)

  lines <- vapply(seq_len(nrow(comps)), function(i) {
    id <- comps$id[i]
    terms <- character(0)
    for (j in seq_len(nrow(flows))) {
      if (flows$to[j] == id) terms <- c(terms, paste0("+ ", flow_tex[j]))
      if (flows$from[j] == id) {
        t <- if (flow_is_sum[j]) paste0("\\left(", flow_tex[j], "\\right)") else flow_tex[j]
        terms <- c(terms, paste0("- ", t))
      }
    }
    rhs <- if (length(terms)) sub("^\\+ ", "", paste(terms, collapse = " ")) else "0"
    paste0("\\frac{d", latex_name(comps$name[i]), "}{dt} &= ", rhs)
  }, "")

  in_N <- comps$name[comps$in_N]
  n_line <- if (length(in_N)) {
    paste0("N &= ", paste(vapply(in_N, latex_name, ""), collapse = " + "))
  }
  paste0("$$\\begin{aligned}", paste(c(lines, n_line), collapse = " \\\\[4pt] "), "\\end{aligned}$$")
}
