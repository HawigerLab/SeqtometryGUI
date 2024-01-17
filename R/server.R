setup_server <- function(self) {
  function(input, output, session) {
    session$onSessionEnded(shiny::stopApp)

    # UI setup
    shiny::observe(shinyjs::runjs("$('.jstree').trigger('ready.jstree')"))
    update_tree(self, output)
    update_table(self, input, output)
    update_plots(self, input, output)

    # Click handlers
    shiny::observeEvent(input$tree, on_tree_click(self, input, output))
    shiny::observeEvent(input$calcButton, on_calc_click(self, input, output))
    shiny::observeEvent(input$addButton, on_add_click(self, input, output, session))
    shiny::observeEvent(input$clrButton, on_clr_click(self, input, output))
    shiny::observeEvent(input$applyButton, on_apply_click(self, input, output, session))
    shiny::observeEvent(input$rmButton, on_rm_click(self, input, output))
  }
}