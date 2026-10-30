# =============================================================================
# expr_safe.R -- reading user-typed rate formulas SAFELY
# =============================================================================
#
# Users type rate formulas such as  beta * S * I / N.  We never run that text
# with eval() directly. Instead we:
#   1. turn the text into an R parse tree with parse() (this runs nothing),
#   2. walk the tree and reject anything that is not a number, a name, an
#      arithmetic operator or one of a few whitelisted maths functions,
#   3. later evaluate the checked tree inside an environment that ONLY
#      contains those whitelisted functions (see safe_eval_env()).
#
# So even a formula like  system("rm -rf /")  is rejected at step 2, and could
# not run at step 3 either, because system() does not exist in that
# environment.
# =============================================================================

# Maths functions users may call in a formula
ALLOWED_FUNCTIONS <- c("exp", "log", "sqrt", "min", "max")

# Operators. In R these are functions too, e.g. a + b is `+`(a, b)
ALLOWED_OPERATORS <- c("+", "-", "*", "/", "^", "(")

# Names with a fixed meaning: N = total living population, t = time
RESERVED_NAMES <- c("N", "t")

# Small helper: "a %||% b" returns b when a is NULL
`%||%` <- function(a, b) if (is.null(a)) b else a


#' Environment used to evaluate rate formulas.
#'
#' It contains only the allowed operators and functions, and its parent is the
#' empty environment, so nothing else in R can be reached from a formula.
safe_eval_env <- local({
  env <- new.env(parent = emptyenv())
  for (f in c(ALLOWED_OPERATORS, ALLOWED_FUNCTIONS)) {
    assign(f, get(f, envir = baseenv()), envir = env)
  }
  lockEnvironment(env, bindings = TRUE)
  function() env
})


#' Check a rate formula typed by the user.
#'
#' @param text The formula as text, e.g. "beta * S * I / N".
#' @return A list with
#'   ok      TRUE if the formula is allowed,
#'   expr    the parsed formula (an R "call" or name) if ok,
#'   symbols the names used in the formula (compartments, parameters, N, t),
#'   error   a friendly message if not ok.
parse_rate <- function(text) {
  fail <- function(msg) list(ok = FALSE, expr = NULL, symbols = character(0), error = msg)

  text <- trimws(paste(text %||% "", collapse = " "))
  if (!nzchar(text)) {
    return(fail("The rate is empty. Type a formula, for example: beta * S * I / N"))
  }

  parsed <- tryCatch(parse(text = text, keep.source = FALSE), error = function(e) e)
  if (inherits(parsed, "error")) {
    return(fail(paste0(
      "This formula could not be read. Check for missing brackets or ",
      "operators (e.g. write 2 * beta, not 2beta)."
    )))
  }
  if (length(parsed) != 1) {
    return(fail("Please enter a single formula (no ';' or several lines)."))
  }
  expr <- parsed[[1]]

  symbols  <- character(0)
  problems <- character(0)

  # Recursive walk through the parse tree
  walk <- function(e) {
    if (is.numeric(e)) {
      if (length(e) != 1 || !is.finite(e)) problems <<- c(problems, "contains an invalid number")
      return(invisible())
    }
    if (is.symbol(e)) {
      nm <- as.character(e)
      if (!nzchar(nm)) {
        problems <<- c(problems, "has an empty argument (e.g. a stray comma)")
      } else {
        symbols <<- c(symbols, nm)
      }
      return(invisible())
    }
    if (is.call(e)) {
      fn <- e[[1]]
      fname <- if (is.symbol(fn)) as.character(fn) else ""
      if (!fname %in% c(ALLOWED_OPERATORS, ALLOWED_FUNCTIONS)) {
        shown <- if (nzchar(fname)) sprintf("'%s'", fname) else "this construction"
        problems <<- c(problems, sprintf(
          "uses %s, which is not allowed. You can use numbers, names, + - * / ^ ( ) and the functions %s",
          shown, paste(ALLOWED_FUNCTIONS, collapse = ", ")
        ))
        return(invisible())
      }
      args <- as.list(e)[-1]
      for (i in seq_along(args)) {
        # An empty argument, e.g. min(a, ), cannot even be stored in a variable
        if (identical(args[[i]], quote(expr = ))) {
          problems <<- c(problems, "has an empty argument (e.g. a stray comma)")
        } else {
          walk(args[[i]])
        }
      }
      return(invisible())
    }
    # Anything else: text in quotes, TRUE/FALSE, NULL, ...
    problems <<- c(problems, "contains something that is not a number or a name (e.g. text in quotes or TRUE/FALSE)")
  }
  walk(expr)

  if (length(problems)) {
    return(fail(paste0("This formula ", problems[1], ".")))
  }
  list(ok = TRUE, expr = expr, symbols = unique(symbols), error = NULL)
}


#' Rename one name inside a formula, keeping the user's formatting.
#'
#' Used when a compartment is renamed, e.g. I -> Inf turns
#' "beta * S * I / N" into "beta * S * Inf / N". Only whole names are
#' replaced (renaming I does not touch "Ia"), and function names are left
#' alone. Formulas that cannot be parsed are returned unchanged.
rename_symbol <- function(text, old, new) {
  text <- gsub("\t", " ", text %||% "")  # tabs would shift column positions
  parsed <- tryCatch(parse(text = text, keep.source = TRUE), error = function(e) NULL)
  if (is.null(parsed)) return(text)

  pd <- utils::getParseData(parsed)
  hits <- pd[pd$token == "SYMBOL" & pd$text == old, , drop = FALSE]
  if (nrow(hits) == 0) return(text)

  lines <- strsplit(text, "\n", fixed = TRUE)[[1]]
  # Replace from the end backwards so earlier positions stay valid
  hits <- hits[order(hits$line1, hits$col1, decreasing = TRUE), , drop = FALSE]
  for (i in seq_len(nrow(hits))) {
    l <- hits$line1[i]
    lines[l] <- paste0(
      substr(lines[l], 1, hits$col1[i] - 1),
      new,
      substr(lines[l], hits$col2[i] + 1, nchar(lines[l]))
    )
  }
  paste(lines, collapse = "\n")
}


#' Is this a valid compartment or parameter name?
#'
#' @param name     The proposed name.
#' @param taken    Names already in use by something else.
#' @return NULL if fine, otherwise a friendly message.
check_name <- function(name, taken = character(0)) {
  name <- trimws(name %||% "")
  if (!nzchar(name)) return("The name cannot be empty.")
  if (name != make.names(name) || grepl(".", name, fixed = TRUE)) {
    return(paste0(
      "'", name, "' is not a valid name. Use letters, digits and _ only, ",
      "starting with a letter (e.g. S, I2, Dead)."
    ))
  }
  if (name %in% RESERVED_NAMES) {
    return(paste0("'", name, "' is reserved (N = total population, t = time). Please choose another name."))
  }
  if (name %in% c(ALLOWED_FUNCTIONS, "SOURCE", "SINK")) {
    return(paste0("'", name, "' is reserved. Please choose another name."))
  }
  if (name %in% taken) return(paste0("The name '", name, "' is already used."))
  NULL
}
