check_params <- function(dt, ip, ix, iy, iz) {
  if (ip == "hide") return("")
  if (ip == "") return("Please select a plot type")
  if (ix == "") return("Please select an x-axis")
  if (iy == "" && ip != "density") return("Please select a y-axis")

  nx <- is.numeric(dt[[ix]])
  ny <- is.numeric(dt[[iy]])
  nxy <- nx && ny
  dplyr::case_when(
    ip %in% c("scatter", "contour") && !nxy ~ "x and y-axes for plot must both be numeric",
    ip == "violin" && nxy ~ "One axis needs to be categorical and the other numeric",
    ip == "density" && !nx ~ "x-axis must be numeric",
    ip != "scatter" && is.numeric(dt[[iz]]) ~ "z-axis needs to be categorical",
    .default = "")
}

gen_plot <- function(dt, ip, ix, iy, iz, tx) {
  if (tx != "" || ip == "hide") return(NULL)

  if (iz == "") iz <- "Empty"
  cls <- if (iz == "Empty") "#000000" else if (is.numeric(dt[[iz]])) c("#0000FF", "#FFFFFF", "#FF0000") else "Set1"
  p <- plotly::plot_ly(dt,
    x = stats::as.formula(paste0("~", ix)),
    y = stats::as.formula(paste0("~", iy)),
    color = stats::as.formula(paste0("~", iz))) |>
    plotly::layout(
      legend = list(itemsizing = "constant"),
      xaxis = list(zeroline = FALSE),
      yaxis = list(zeroline = FALSE))
  if (ip == "violin") {
    plotly::add_trace(p, type = "violin", colors = cls)
  } else if (ip == "density") {
    p <- plotly::plot_ly() |> plotly::layout(
      xaxis = list(title = ix, zeroline = FALSE),
      yaxis = list(title = "Normalized density", zeroline = FALSE))
    purrr::walk(unique(dt[[iz]]), function(l) {
      d <- stats::density(dt[[ix]][dt[[iz]] == l], bw = "SJ")
      r <- range(d$y)
      p <<- plotly::add_trace(p,
        x = d$x, y = (d$y - r[[1]]) / (r[[2]] - r[[1]]),
        colors = cls,
        type = "scatter",
        mode = "lines+markers",
        marker = list(opacity = 0),
        key = paste("Histogram", ix, iz, l, sep = "-"),
        name = l)
    })
    p
  } else {
    p <- p |>
      plotly::add_markers(
        key = dt[["ID"]],
        marker = list(size = 2),
        colors = cls,
        legendgroup = dt[[iz]],
        opacity = if (ip == "scatter") 1 else 0,
        showlegend = ip == "scatter") |>
      plotly::add_markers(
        x = range(dt[[ix]]),
        y = range(dt[[iy]]),
        color = dt[[iz]][1],
        key = NA,
        legendgroup = NA,
        opacity = 0,
        showlegend = FALSE,
        hoverinfo = "none")
    if (ip == "contour") {
      purrr::walk(unique(dt[[iz]]), function(l) {
        i <- which(dt[[iz]] == l)
        p <<- plotly::add_histogram2dcontour(p,
          x = dt[[ix]][i], y = dt[[iy]][i], color = l,
          legendgroup = l, colors = cls, contours = list(coloring = "none"))
      })
    }
    p
  }
}