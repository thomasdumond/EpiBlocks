# =============================================================================
# help.R -- the in-app Help tab
# =============================================================================

help_ui <- function() {
  div(
    class = "help-text",
    layout_columns(
      col_widths = c(6, 6),
      div(
        h5("1. Start from a template or from scratch"),
        p("Pick a model in the ", strong("Template"), " menu and press ", strong("Load"),
          ", or press ", strong("New"), " to start from an empty diagram."),

        h5("2. Add a compartment (box)"),
        p("Click ", strong("Add compartment"), " above the diagram, then click on an empty space.",
          " The new box is called C1, C2, … Click it to change its ", strong("name"),
          " (press ", strong("Rename"), "), its ", strong("initial value"),
          " and whether it ", strong("counts towards N"), "."),
        p("Names may contain letters, digits and _ and must start with a letter.",
          " N (total living population) and t (time) are reserved."),

        h5("3. Draw a flow (arrow)"),
        p("Click ", strong("Draw flow"), ", then press on a box and drag to another box.",
          " Draw from the ", strong("Source"), " for births or inflow,",
          " and into the ", strong("Sink"), " for deaths or removals."),

        h5("4. Set the rate of a flow"),
        p("Click an arrow. Choose its ", strong("type"), " and the formula is filled in for you:"),
        tags$ul(
          tags$li(tags$code("beta * S * I / N"), " infection, frequency dependent"),
          tags$li(tags$code("beta * S * I"), " infection, density dependent"),
          tags$li(tags$code("gamma * I"), " progression, recovery or death (rate × origin box)"),
          tags$li(tags$code("lambda"), " constant inflow"),
          tags$li(strong("Custom"), ": type any formula")
        ),
        p("You can always edit the formula. Any new name you type (e.g. ", tags$code("theta"),
          ") becomes a parameter and appears under ", strong("Parameters"), ".")
      ),
      div(
        h5("What can a formula contain?"),
        tags$ul(
          tags$li("numbers, e.g. ", tags$code("0.5"), " or ", tags$code("1e-3")),
          tags$li("compartment names, parameter names, ", tags$code("N"), " and ", tags$code("t")),
          tags$li(tags$code("+ - * / ^"), " and brackets"),
          tags$li(tags$code("exp(), log(), sqrt(), min(), max()"))
        ),

        h5("5. Explore"),
        p("Change parameter values and the epicurve updates straight away.",
          " Tick or untick compartments above the plot to show or hide them.",
          " The ", strong("Equations"), " tab shows the model as differential equations,",
          " and the ", strong("Table"), " tab lets you download the results as CSV."),

        h5("6. Keep your work"),
        p(strong("Save"), " downloads your model as a .json file.",
          " ", strong("Open"), " loads it again later."),

        h5("Tips"),
        tags$ul(
          tags$li("An infection needs at least one infectious animal at time 0."),
          tags$li("Make outflows proportional to their origin box (e.g. ",
                  tags$code("mu * S"), "), otherwise numbers can go negative."),
          tags$li("To delete something, select it and press ", strong("Delete selected"),
                  " or use the button in the panel on the right."),
          tags$li("Scroll to zoom the diagram; drag the background to move it.")
        )
      )
    )
  )
}
