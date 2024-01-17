#' Seqtometry GUI
#' @description
#' An R6 object encapsulating all data for passing and modification (by reference) across Shiny app components
#' @importFrom R6 R6Class
#' @export
SeqtometryGUI <- R6::R6Class("SeqtometryGUI", list(
  #' @field SO A Seurat object containing a gene expression matrix that has passed QC
  SO = NULL,
  #' @field GS A named list of gene signatures
  GS = NULL,
  #' @field IA A string denoting the name of the imputed assay
  IA = NULL,
  #' @field DT Root node of data.tree for all data.
  #' Each node contains a data.table with signature scores, gates, as well as Seurat object metadata and embeddings
  DT = NULL,
  #' @field AP Node that represents the current level (subtree) of the gating hierarchy being inspected
  AP = NULL,
  #' @description Initialize fields
  #' @param seurat_object Seurat object: contains the data to be analyzed
  #' @param gene_signatures list of character: gene signatures to be used for scoring
  #' @param assay string: the name of the assay in the Seurat object to be used for analysis
  #' @param impute boolean: whether to perform MAGIC imputation on parameter assay
  #' @param reduction string: the name of the reduction in the Seurat object to be used for fitting the diffusion operator
  #' @param approximate boolean: whether to perform approximate MAGIC imputation
  #' @returns R6 object with all fields initialized
  #' @importFrom magrittr `%>%` `%<>%`
  #' @importFrom methods is
  #' @importFrom BiocSingular LowRankMatrix
  #' @importFrom reticulate import py_module_available
  #' @import Seurat
  #' @import data.table
  initialize = function(seurat_object, gene_signatures, assay = "RNA",
                        impute = TRUE, reduction = NULL, approximate = FALSE) {
    if (!is(seurat_object, "Seurat"))
      stop("Please input a Seurat object")
    if (!is(gene_signatures, "list") || purrr::some(gene_signatures, ~ purrr::vec_depth(.) != 1))
      stop("Please input a list of signatures")
    if (!is.null(assay) && !assay %in% SeuratObject::Assays(seurat_object))
      stop("Specified assay not found in Seurat object")

    self$SO <- seurat_object
    self$GS <- gene_signatures
    if (is.null(assay)) assay <- self$SO@active.assay
    if (impute) {
      if (!py_module_available("magic"))
        stop("magic python module not detected")
      self$IA <- paste0("MAGIC_", assay)
      magic <-
        if (is.null(reduction)) {
          print("Log10K normalization", quote = FALSE)
          self$SO@active.assay <- assay
          self$SO <- self$SO |>
            GetAssayData(assay, "counts") %>%
            .[Matrix::rowSums(.) > 0, ] |> # Remove unexpressed genes
            SetAssayData(self$SO, "counts", new.data = _)
          self$SO %<>% NormalizeData(assay)
          self$SO |>
            GetAssayData(assay) |>
            Matrix::t() |>
            import("magic")$MAGIC(t = "auto", n_jobs = -1L)$fit()
        } else {
          if (!reduction %in% Reductions(self$SO))
            stop("Specified reduction not found in Seurat object")
          r <- Reductions(self$SO, reduction)
          if (is.null(r@cell.embeddings))
            stop("Reduction is missing cell embeddings")
          print("Calculating diffusion operator on supplied reduced dimensions", quote = FALSE)
          r@cell.embeddings |> import("magic")$MAGIC(t = "auto", n_jobs = -1L)$fit()
        }
      imat <-
        if (approximate) {
          if (is.null(reduction)) {
            # From SVD: Principal components = U * S, Rotation = Vt
            LowRankMatrix(
              rotation = t(magic$graph$data_pca$components_),
              components = sweep(magic$transform(magic$graph$data_nu), 2, magic$graph$data_pca$singular_values_, "*"))
          } else {
            r <- Reductions(self$SO, reduction)
            if (is.null(r@feature.loadings))
              stop("Reduction is missing feature loadings")
            if (nrow(r@feature.loadings) != nrow(self$SO))
              stop("Reduction was not performed on all genes")
            LowRankMatrix(
              rotation = r@feature.loadings,
              components = magic$transform(r@cell.embeddings))
          }
        } else {
          magic$transform()
        }
      imat %<>% Matrix::t()
      dimnames(imat) <- dimnames(self$SO)
      self$SO[[self$IA]] <- CreateAssayObject(data = imat)
    } else {
      print("Warning: skipping MAGIC imputation (this is not recommended unless the given assay was already imputed).", quote = FALSE)
      self$IA <- assay
    }
    print("Calculating scores", quote = FALSE)
    options(future.globals.maxSize = 16 * 1024^3)
    scores <- GetAssayData(self$SO, assay = self$IA) |> Seqtometry::score(self$GS)

    root <- as.data.table(self$SO@meta.data, keep.rownames = "ID")
    embeddings <- lapply(self$SO@reductions, \(x) x@cell.embeddings[, 1:2]) %>% do.call(cbind, .)
    root %<>% cbind(embeddings, scores)
    root[, `:=`(Gate = "Unassigned", Empty = "")]
    self$AP <- self$DT <- data.tree::Node$new("Root", dt = root)
    print("All fields initialized, you may now invoke this object's run_gui method.", quote = FALSE)
  },
  #' @description run a Shiny app using the data within this object's fields
  run_gui = function() {
    shiny::shinyApp(ui = setup_ui(self), server = setup_server(self))
  }
))