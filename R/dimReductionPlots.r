# ============================================================================
# PCA PLOTS
# ============================================================================

#' Create PCA Plot
#'
#' Creates a Principal Component Analysis plot using ggplot2.
#'
#' @param data A numeric data frame or matrix. Rows are samples, columns are features
#' @param feature_list A named list. First element is used for coloring, second for shape
#' @param title Character string. Plot title (default: "")
#' @param scale_data Logical. Scale data before PCA (default: TRUE)
#' @param show_labels Logical. Show sample labels (default: FALSE)
#' @param x_lims Optional. Numeric vector of length 2 for x-axis limits
#' @param y_lims Optional. Numeric vector of length 2 for y-axis limits
#' @param point_size Numeric. Size of points (default: 3)
#'
#' @return A ggplot2 object
#' @export
#'
#' @examples
#' \dontrun{
#' plot_pca(data = expression_matrix,
#'          feature_list = list(group = sample_groups, batch = sample_batches))
#' }
plot_pca <- function(data, feature_list, title = "", scale_data = TRUE,
                     show_labels = FALSE, x_lims = NULL, y_lims = NULL,
                     point_size = 3) {
  # Input validation
  if (!is.data.frame(data) && !is.matrix(data)) {
    stop("'data' must be a data frame or matrix")
  }
  if (!is.list(feature_list) || length(feature_list) == 0) {
    stop("'feature_list' must be a non-empty list")
  }

  # Convert to data frame and handle missing values
  data <- as.data.frame(data)
  data[is.na(data)] <- 0

  # Scale if requested
  if (scale_data) {
    data <- as.data.frame(scale(data))
  }

  # Perform PCA
  pca_result <- stats::prcomp(data, center = FALSE, scale = FALSE)
  pca_df <- as.data.frame(pca_result$x)

  # Add feature information
  feature_names <- names(feature_list)
  pca_df$Class <- as.factor(feature_list[[1]])
  pca_df$Name <- rownames(pca_df)

  if (length(feature_names) > 1) {
    pca_df$Shape <- as.factor(feature_list[[2]])
  }

  # Calculate variance explained
  variance_explained <- round(pca_result$sdev^2 / sum(pca_result$sdev^2) * 100, 2)
  x_label <- paste0("PC1 (", variance_explained[1], "%)")
  y_label <- paste0("PC2 (", variance_explained[2], "%)")

  # Create plot
  p <- ggplot2::ggplot(pca_df,
                       ggplot2::aes(x = PC1, y = PC2, color = Class, label = Name)) +
    ggplot2::xlab(x_label) +
    ggplot2::ylab(y_label) +
    ggpubr::theme_pubr() +
    ggplot2::ggtitle(title) +
    ggplot2::theme(legend.position = "right")

  # Add points or labels
  if (show_labels && length(feature_names) == 1) {
    p <- p + ggplot2::geom_text()
  } else {
    if (length(feature_names) > 1) {
      p <- p + ggplot2::aes(shape = Shape) +
        ggplot2::geom_point(size = point_size) +
        ggplot2::scale_shape_discrete(name = feature_names[2])
    } else {
      p <- p + ggplot2::geom_point(size = point_size)
    }
  }

  # Add axis limits if provided
  if (!is.null(x_lims)) {
    p <- p + ggplot2::xlim(x_lims)
  }
  if (!is.null(y_lims)) {
    p <- p + ggplot2::ylim(y_lims)
  }

  # Add color scale
  p <- p + ggplot2::scale_color_discrete(name = feature_names[1])

  return(p)
}

# ============================================================================
# TSNE plots
# ============================================================================
plot_tsne <- function(dataframe,title="",feature){
  library(M3C)
  tsnePlot<-tsne(dataframe,labels=feature,axistextsize = 6,dotsize = 2,text = colnames(dataframe),legendtitle = title,textlabelsize = 3)
  tsnePlot
}