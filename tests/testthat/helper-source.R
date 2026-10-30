# Load the app's logic (everything in R/) before the tests run.
# testthat runs tests from tests/testthat, so the app folder is two levels up.
app_dir <- normalizePath(file.path("..", ".."))
for (f in list.files(file.path(app_dir, "R"), pattern = "\\.R$", full.names = TRUE)) {
  source(f, local = FALSE)
}

# Small helper: build a model from a spec (same format as templates)
model_of <- function(spec) model_from_spec(spec)$model
