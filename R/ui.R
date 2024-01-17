setup_ui <- function(self) {
  chs <- c("Make selection" = "", data.tree::GetAttribute(self$DT, "dt") |> colnames() |> setdiff("ID"))
  shiny::fluidPage(
    shinyjs::useShinyjs(),
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        shinyTree::shinyTree("tree", multiple = FALSE),
        shiny::actionButton("addButton", "Add gate"),
        shiny::actionButton("applyButton", "Apply gates"),
        shiny::actionButton("clrButton", "Clear gates"),
        shiny::textInput("inTxt", label = NULL),
        shiny::actionButton("rmButton", "Remove selected subtree"),
        shiny::actionButton("calcButton", "Recalculate scores"),
        shiny::verbatimTextOutput("msg", placeholder = TRUE),
        shiny::selectizeInput(inputId = "tbl_grp", label = "Select grouping", choices = chs),
        shiny::tableOutput("tbl")
      ),
      shiny::mainPanel(
        shiny::fluidRow(
          shiny::column(3, shiny::selectizeInput(inputId = "X1", label = "Select X1", choices = chs)),
          shiny::column(3, shiny::selectizeInput(inputId = "Y1", label = "Select Y1", choices = chs)),
          shiny::column(3, shiny::selectizeInput(inputId = "Z1", label = "Select Z1", choices = chs)),
          shiny::column(3, shiny::selectizeInput(inputId = "P1", label = "Select Plot1",
              choices = c("Make selection" = "", "scatter", "contour", "violin", "density", "hide")))
        ),
        shiny::fluidRow(
          shiny::column(3, shiny::selectizeInput(inputId = "X2", label = "Select X2", choices = chs)),
          shiny::column(3, shiny::selectizeInput(inputId = "Y2", label = "Select Y2", choices = chs)),
          shiny::column(3, shiny::selectizeInput(inputId = "Z2", label = "Select Z2", choices = chs)),
          shiny::column(3, shiny::selectizeInput(inputId = "P2", label = "Select Plot2",
              choices = c("Make selection" = "", "scatter", "contour", "violin", "density", "hide")))
        ),
        shiny::fluidRow(
          shiny::column(6, shiny::textOutput("t1")),
          shiny::column(6, shiny::textOutput("t2"))
        ),
        shiny::uiOutput("plotArea")
      )
    )
  )
}