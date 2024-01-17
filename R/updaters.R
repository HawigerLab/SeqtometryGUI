update_ui <- function(input, output) {
  output$plotArea <- shiny::renderUI({
    if (input$P2 == "hide") {
      shiny::fluidRow(
        shiny::column(12,
          plotly::plotlyOutput(outputId = "p1", height = "100%")),
          plotly::plotlyOutput(outputId = "p2", height = "100%"))
    } else {
      shiny::fluidRow(
        shiny::column(6, plotly::plotlyOutput(outputId = "p1", height = "100%")),
        shiny::column(6, plotly::plotlyOutput(outputId = "p2", height = "100%")))
    }
  })
}

update_plots <- function(self, input, output) {
  update_ui(input, output)
  dt <- data.tree::GetAttribute(self$AP, "dt")
  t1_dat <- shiny::reactive(check_params(dt, input$P1, input$X1, input$Y1, input$Z1))
  t2_dat <- shiny::reactive(check_params(dt, input$P2, input$X2, input$Y2, input$Z2))
  output$t1 <- shiny::renderText(t1_dat())
  output$t2 <- shiny::renderText(t2_dat())
  output$p1 <- plotly::renderPlotly(gen_plot(dt, input$P1, input$X1, input$Y1, input$Z1, t1_dat()))
  output$p2 <- plotly::renderPlotly(gen_plot(dt, input$P2, input$X2, input$Y2, input$Z2, t2_dat()))
}

update_tree <- function(self, output) {
  output$tree <- shinyTree::renderTree(.tree_to_rjson(self$DT))
}

update_table <- function(self, input, output) {
  output$tbl <- shiny::renderTable({
    dt <- data.tree::GetAttribute(self$AP, "dt")
    cl <- input$tbl_grp
    if (cl == "" || is.numeric(dt[[cl]])) {
      data.frame(Group = "", Count = "", Percent = "")
    } else {
      dt[, .(Count = .N, Percent = .N / nrow(dt) * 100), cl][order(Count, decreasing = TRUE)]
    }
  })
}