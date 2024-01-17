.tree_to_rjson <- function(root) {
  ret <- list()
  children <- data.tree::GetAttribute(root, "children")
  ret[[data.tree::GetAttribute(root, "name")]] <-
    if (is.na(children) || rlang::is_empty(children)) list()
    else purrr::map(children, .tree_to_rjson) |> purrr::flatten()
  structure(ret, stopened = TRUE)
}

on_tree_click <- function(self, input, output) {
  l <- shinyTree::get_selected(input$tree)
  if (rlang::is_empty(l)) return() else l <- l[[1]]
  self$AP <-
    if (l == "Root") {
      self$DT
    } else {
      attr(l, "ancestry")[-1] |>
      c(l[1]) |>
      purrr::reduce(~ data.tree::GetAttribute(.x, "children")[[.y]], .init = self$DT)
    }
  update_table(self, input, output)
  update_plots(self, input, output)
}

on_calc_click <- function(self, input, output) {
  output$msg <- shiny::renderText("Calculating scores...please wait")
  dt <- data.tree::GetAttribute(self$AP, "dt")
  sm <- Seurat::GetAssayData(self$SO, assay = self$IA) %>%
    .[, dt$ID] %>%
    .[Matrix::rowSums(.) != 0, ] |>
    Seqtometry::score(self$GS) |>
    data.table::as.data.table()
  dt[, colnames(sm) := sm]
  output$msg <- shiny::renderText("Calculating scores: done")
  update_plots(self, input, output)
}

.get_cell_ids <- function(dt) {
  i <- plotly::event_data("plotly_selected")
  if (is.null(i) || methods::is(i, "list")) return(NULL)
  i %<>% dplyr::filter(!is.na(key))
  if (stringr::str_detect(i[1, "key"], "Histogram")) {
    k <- stringr::str_split(i[1, "key"], "-", simplify = TRUE)
    x <- k[1, 2]
    r <- range(i$x)
    z <- k[1, 3]
    l <- unique(k[, 4])
    dt[dt[[z]] %in% l & dplyr::between(dt[[x]], r[1], r[2])]$ID
  } else {
    i$key
  }
}

on_add_click <- function(self, input, output, session) {
  if (input$inTxt == "") {
    output$msg <- shiny::renderText("Enter gate name")
  } else {
    dt <- data.tree::GetAttribute(self$AP, "dt")
    dt[ID %in% .get_cell_ids(dt), Gate := input$inTxt]
    output$msg <- shiny::renderText("Gate added")
    shiny::updateTextInput(session, "inTxt", value = "")
    if (input$tbl_grp == "Gate") update_table(self, input, output)
    if (input$Z1 == "Gate" || input$Z2 == "Gate") update_plots(self, input, output)
  }
}

on_clr_click <- function(self, input, output) {
  data.tree::GetAttribute(self$AP, "dt")[, Gate := "Unassigned"]
  output$msg <- shiny::renderText("All gates reset to Unassigned")
  if (input$tbl_grp == "Gate") update_table(self, input, output)
  if (input$Z1 == "Gate" || input$Z2 == "Gate") update_plots(self, input, output)
}

on_apply_click <- function(self, input, output, session) {
  if (input$inTxt == "") {
    output$msg <- shiny::renderText("Enter new node name")
  } else {
    dt <- data.tree::GetAttribute(self$AP, "dt") |> data.table::copy() %>% .[Gate != "Unassigned"]
    self$AP$AddChild(input$inTxt, dt = dt)
    update_tree(self, output)
    output$msg <- shiny::renderText("Applied gates")
    shiny::updateTextInput(session, "inTxt", value = "")
  }
}

on_rm_click <- function(self, input, output) {
  if (data.tree::GetAttribute(self$AP, "name") == "Root") {
    output$msg <- shiny::renderText("Cannot remove root node")
  } else {
    ps <- self$AP$pathString
    data.tree::Prune(self$DT, \(x) x$pathString != ps)
    self$AP <- self$DT
    output$msg <- shiny::renderText("Pruned subtree")
    update_tree(self, output)
    update_table(self, input, output)
    update_plots(self, input, output)
  }
}