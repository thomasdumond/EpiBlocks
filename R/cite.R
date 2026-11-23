# =============================================================================
# cite.R -- the "Cite" tab: how to cite EpiBlocks, version and contact
# =============================================================================
#
# Change the details here (e.g. add an author, or a DOI once there is one).
# The "accessed" date is the day the user opens the app.
# =============================================================================

APP_VERSION <- "1.0"
APP_YEAR    <- "2026"
APP_TITLE   <- "EpiBlocks: build compartmental epidemic models by drawing them"
APP_URL     <- "https://shiny.vet.unimelb.edu.au/epi/epi.blocks/"
APP_GITHUB  <- "https://github.com/thomasdumond/EpiBlocks"
APP_EMAIL   <- "thomas.dumond@adelaide.edu.au"

# Authors, in citation order. To add one, copy a block.
#   cite         as written in the citation ("Last Initials")
#   bib          for BibTeX ("Last, First")
#   institution  short form used in the citation
APP_AUTHORS <- list(
  list(name = "Thomas Dumond", cite = "Dumond T", bib = "Dumond, Thomas",
       affiliation = paste("School of Animal and Veterinary Sciences, Adelaide University,",
                           "Roseworthy Campus, Roseworthy, South Australia, Australia"),
       institution = "Adelaide University"),
  list(name = "Simin Lee", cite = "Lee S", bib = "Lee, Simin",
       affiliation = paste("Melbourne Veterinary School, Faculty of Science,",
                           "The University of Melbourne, Parkville, VIC, Australia"),
       institution = "The University of Melbourne")
)

author_field <- function(field) vapply(APP_AUTHORS, `[[`, "", field)

#' Institutions, each once, in author order: "A and B" (used in the citation).
cite_institutions <- function() {
  inst <- unique(author_field("institution"))
  if (length(inst) <= 1) return(inst)
  paste(paste(head(inst, -1), collapse = ", "), "and", tail(inst, 1))
}

#' Date as "22 November 2026" (no leading zero, English month names).
cite_date <- function(date = Sys.Date()) {
  paste(as.integer(format(date, "%d")), month.name[as.integer(format(date, "%m"))],
        format(date, "%Y"))
}

#' The citation as plain text.
cite_text <- function(date = Sys.Date()) {
  sprintf("%s (%s). %s (version %s). %s. %s (accessed %s).",
          paste(author_field("cite"), collapse = ", "), APP_YEAR, APP_TITLE,
          APP_VERSION, cite_institutions(),
          APP_URL, cite_date(date))
}

#' The citation as a BibTeX entry (@misc works in BibTeX, biblatex and Zotero).
cite_bibtex <- function(date = Sys.Date()) {
  paste0(
    "@misc{epiblocks,\n",
    "  author       = {", paste(author_field("bib"), collapse = " and "), "},\n",
    "  title        = {{EpiBlocks}: build compartmental epidemic models by drawing them},\n",
    "  year         = {", APP_YEAR, "},\n",
    "  version      = {", APP_VERSION, "},\n",
    "  organization = {", cite_institutions(), "},\n",
    "  howpublished = {\\url{", APP_URL, "}},\n",
    "  note         = {Version ", APP_VERSION, ". Accessed ", cite_date(date), "}\n",
    "}"
  )
}

#' A block of citation text with a Copy button above it.
cite_block <- function(id, label, text) {
  div(
    class = "mb-3",
    div(class = "d-flex align-items-center gap-2 mb-1",
        strong(label),
        tags$button(id = paste0("copy_", id), type = "button",
                    class = "btn btn-sm btn-outline-secondary py-0",
                    onclick = sprintf("epiCopy('%s', this)", id), icon("copy"), " Copy")),
    tags$pre(id = id, class = "cite-text", text)
  )
}

#' The contents of the Cite tab (built per session, so the date is today's).
cite_ui <- function(date = Sys.Date()) {
  div(
    class = "help-text",
    layout_columns(
      col_widths = c(7, 5),
      div(
        h5("How to cite EpiBlocks"),
        p("If you use EpiBlocks in teaching, a workshop or a publication, please cite it as:"),
        cite_block("cite_plain", "Text", cite_text(date)),
        cite_block("cite_bib", "BibTeX", cite_bibtex(date)),
        p(class = "text-muted small",
          "EpiBlocks has no paper or DOI yet. This page will be updated when it does.")
      ),
      div(
        h5(if (length(APP_AUTHORS) > 1) "Authors" else "Author"),
        lapply(APP_AUTHORS, function(a) p(strong(a$name), br(), a$affiliation)),
        h5("Contact and feedback"),
        p("Questions, bugs or ideas for new templates are very welcome."),
        tags$ul(
          tags$li(icon("envelope"), " ", tags$a(href = paste0("mailto:", APP_EMAIL), APP_EMAIL)),
          tags$li(icon("github"), " ",
                  tags$a(href = APP_GITHUB, target = "_blank", rel = "noopener",
                         "Source code on GitHub"))
        ),
        h5("Version"),
        p(paste0("EpiBlocks ", APP_VERSION, "."))
      )
    )
  )
}
