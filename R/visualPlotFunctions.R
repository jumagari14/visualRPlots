# ============================================================================
# visualPlotFunctions.R
# ============================================================================
# Consolidated ggplot2-based visualization functions for R package
# Optimized and renamed for better usability
# ============================================================================

# ============================================================================
# SANKEY/ALLUVIAL PLOTS
# ============================================================================

#' Create Sankey/Alluvial Plot
#'
#' Creates a Sankey (alluvial) diagram for visualizing flow of data across
#' categorical variables, commonly used for clinical data flow visualization.
#'
#' @param data A data frame containing the data to plot
#' @param alluvium Character string. Column name for the alluvium variable (flow identifier)
#' @param x Character string. Column name for the x-axis variable (time/sequence)
#' @param stratum Character string. Column name for the stratum variable (categories)
#' @param palette Optional. Either a character vector of color values or a single
#'   character string for a ColorBrewer palette name
#' @param text_size Numeric. Size of text labels (default: 2.5)
#' @param na_label Character string. Label for missing values (default: "Lost")
#'
#' @return A ggplot2 object
#' @export
#'
#' @examples
#' \dontrun{
#' plot_sankey(data = my_data, alluvium = "patient_id",
#'             x = "timepoint", stratum = "outcome")
#' }
plot_sankey <- function(data, alluvium = "alluvium", x = "x",
                         stratum = "stratum", palette = NULL,
                         text_size = 2.5, na_label = "Lost") {
  # Input validation
  if (!is.data.frame(data)) {
    stop("'data' must be a data frame")
  }
  if (!all(c(alluvium, x, stratum) %in% colnames(data))) {
    stop("All specified column names must exist in 'data'")
  }

  # Prepare data
  data_prep <- data %>%
    dplyr::rename("stratum" = !!stratum, "alluvium" = !!alluvium, "x" = !!x) %>%
    dplyr::mutate_all(.funs = function(x) ifelse(is.na(x), na_label, x))

  # Create base plot
  p <- ggplot2::ggplot(data_prep,
                        ggplot2::aes(x = x, stratum = stratum, alluvium = alluvium,
                                     label = stratum, fill = stratum)) +
    ggalluvial::geom_flow(stat = "flow", aes.flow = "backward", color = "darkgray") +
    ggalluvial::geom_stratum(width = 0.35) +
    ggalluvial::geom_text(stat = "stratum",
                          ggplot2::aes(label = paste0(after_stat(stratum))),
                          size = text_size) +
    ggpubr::theme_pubr() +
    ggplot2::guides(fill = "none") +
    ggplot2::theme(axis.title = ggplot2::element_blank(),
                   axis.text.x = ggplot2::element_text(size = 10),
                   axis.text.y = ggplot2::element_blank(),
                   axis.ticks = ggplot2::element_blank(),
                   axis.line = ggplot2::element_blank())

  # Add color palette
  if (!is.null(palette)) {
    if (length(palette) > 1) {
      p <- p + ggplot2::scale_fill_manual(name = "", values = palette)
    } else {
      p <- p + ggplot2::scale_fill_brewer(name = palette)
    }
  }

  return(p)
}

# ============================================================================
# BOXPLOTS WITH STATISTICAL ANNOTATIONS
# ============================================================================

#' Create Boxplot with Statistical Tests
#'
#' Creates a boxplot comparing groups with automatic statistical testing
#' (ANOVA/Kruskal-Wallis for multiple groups, t-test/Wilcoxon for two groups).
#'
#' @param data A data frame containing the data to plot
#' @param x_var Character string. Column name for the grouping variable (x-axis)
#' @param y_var Character string. Column name for the numeric variable (y-axis)
#' @param parametric Logical or NULL. If NULL, automatically determines based on
#'   normality tests. If TRUE/FALSE, forces parametric/non-parametric tests
#' @param add_points Logical. Add jittered points to the plot (default: TRUE)
#' @param color_palette Character vector or string. Color palette for groups
#' @param show_symbols Logical. Show significance symbols instead of p-values (default: FALSE)
#' @param show_posthoc Logical. Show post-hoc pairwise comparisons (default: FALSE)
#' @param step_increase Numeric. Step increase for p-value brackets (default: 0.03)
#' @param point_alpha Numeric. Transparency of points (default: 0.7)
#' @param point_size Numeric. Size of points (default: 1)
#'
#' @return A list containing:
#'   \item{plot}{The ggplot2 object}
#'   \item{test}{Statistical test results}
#'   \item{summary}{Descriptive statistics}
#' @export
#'
#' @examples
#' \dontrun{
#' result <- plot_boxplot_stats(data = my_data, x_var = "group",
#'                               y_var = "value", show_posthoc = TRUE)
#' print(result$plot)
#' }
plot_boxplot_stats <- function(data, x_var, y_var, parametric = NULL,
                                add_points = TRUE, color_palette = "Set1",
                                show_symbols = FALSE, show_posthoc = FALSE,
                                step_increase = 0.03, point_alpha = 0.7,
                                point_size = 1) {
  # Input validation
  if (!is.data.frame(data)) {
    stop("'data' must be a data frame")
  }
  if (!all(c(x_var, y_var) %in% colnames(data))) {
    stop("Both 'x_var' and 'y_var' must be column names in 'data'")
  }

  # Prepare variable names
  x_var_clean <- make.names(x_var)
  y_var_clean <- make.names(y_var)

  data_prep <- data %>%
    dplyr::rename(!!x_var_clean := !!x_var, !!y_var_clean := !!y_var)

  data_prep[[x_var_clean]] <- as.factor(data_prep[[x_var_clean]])
  data_prep[[y_var_clean]] <- as.numeric(data_prep[[y_var_clean]])

  # Remove missing values
  data_prep <- data_prep[!is.na(data_prep[[y_var_clean]]), ]

  if (nrow(data_prep) == 0) {
    stop("No valid data after removing missing values")
  }

  # Determine if multivariate analysis
  n_groups <- length(unique(data_prep[[x_var_clean]]))
  is_multivariate <- n_groups > 2

  # Auto-detect parametric if needed
  if (is.null(parametric)) {
    parametric <- .check_normality(data_prep, x_var_clean, y_var_clean)
  }

  # Perform statistical tests
  test_results <- .perform_statistical_test(data_prep, x_var_clean, y_var_clean,
                                            parametric, is_multivariate)

  if (is.null(test_results)) {
    return(NULL)
  }

  # Create boxplot
  if (add_points) {
    if (length(color_palette) > 1 && is.character(color_palette)) {
      color_points <- colorspace::darken(color_palette, 0.2)
      p <- ggpubr::ggboxplot(data_prep, x = x_var_clean, y = y_var_clean,
                             palette = color_palette, add = "jitter",
                             fill = x_var_clean, ylab = y_var,
                             add.params = list(color = x_var_clean, alpha = point_alpha)) +
        ggpubr::theme_pubr() +
        ggplot2::theme(legend.position = "none") +
        ggplot2::scale_color_manual(values = color_points, guide = "none")
    } else {
      p <- ggpubr::ggboxplot(data_prep, x = x_var_clean, y = y_var_clean,
                             palette = color_palette, add = "jitter",
                             fill = x_var_clean, ylab = y_var,
                             add.params = list(color = x_var_clean, alpha = point_alpha)) +
        ggpubr::theme_pubr() +
        ggplot2::theme(legend.position = "none") +
        ggplot2::scale_color_brewer(palette = color_palette, guide = "none")
    }
  } else {
    p <- ggpubr::ggboxplot(data_prep, x = x_var_clean, y = y_var_clean,
                           palette = color_palette, fill = x_var_clean,
                           ylab = y_var) +
      ggpubr::theme_pubr() +
      ggplot2::theme(legend.position = "none")
  }

  # Add statistical annotations
  if (show_posthoc && is_multivariate && !is.null(test_results$posthoc)) {
    p <- add_posthoc_annotations(p, data_prep, x_var_clean, y_var_clean,
                                   test_results$posthoc, parametric,
                                   show_symbols, step_increase)
  } else if (!show_posthoc) {
    label_format <- if (show_symbols) "p.signif" else "p.format"
    symnum_args <- if (show_symbols) {
      list(cutpoints = c(0, 0.01, 0.05, Inf), symbols = c("**", "*", ""))
    } else NULL

    p <- p + ggpubr::stat_compare_means(
      mapping = ggplot2::aes_string(group = x_var_clean),
      label = label_format,
      method = test_results$test_name,
      paired = FALSE,
      symnum.args = symnum_args,
      label.x.npc = if (show_symbols) "center" else "left",
      tip.length = 0.01,
      step.increase = step_increase
    )
  }

  # Calculate summary statistics
  summary_stats <- .calculate_summary_stats(data_prep, x_var_clean, y_var_clean)

  return(list(
    plot = p,
    test = test_results,
    summary = summary_stats
  ))
}

#' Add Post-hoc Statistical Annotations to Plot
#'
#' Adds post-hoc pairwise comparison annotations to an existing ggplot object.
#'
#' @param plot A ggplot2 object
#' @param data A data frame containing the data
#' @param x_var Character string. Column name for the grouping variable
#' @param y_var Character string. Column name for the numeric variable
#' @param posthoc_results Data frame. Results from post-hoc tests with columns:
#'   'group1', 'group2', and a p-value column
#' @param pval_column Character string. Name of the p-value column in posthoc_results
#' @param group_var Optional. Character string. Column name for grouping within x_var
#' @param step_increase Numeric. Step increase for bracket positioning (default: 0.03)
#' @param tip_length Numeric. Length of bracket tips (default: 0.01)
#' @param label_format Character. Format for labels: "symbol" or "number" (default: "symbol")
#' @param significance_cutoffs Numeric vector. Cutoffs for significance symbols
#' @param significance_symbols Character vector. Symbols corresponding to cutoffs
#'
#' @return A ggplot2 object with annotations added
#' @export
add_posthoc_annotations <- function(plot, data, x_var, y_var, posthoc_results,
                                    pval_column = "p.adj", group_var = NULL,
                                    step_increase = 0.03, tip_length = 0.01,
                                    label_format = "symbol",
                                    significance_cutoffs = c(0, 0.001, 0.01, 0.05, Inf),
                                    significance_symbols = c("***", "**", "*", "ns")) {
  # Input validation
  if (!inherits(plot, "ggplot")) {
    stop("'plot' must be a ggplot2 object")
  }
  if (!is.data.frame(data)) {
    stop("'data' must be a data frame")
  }
  if (!is.data.frame(posthoc_results)) {
    stop("'posthoc_results' must be a data frame")
  }

  return(.add_posthoc_annotations_internal(plot, data, x_var, y_var,
                                           posthoc_results, pval_column,
                                           group_var, step_increase,
                                           tip_length, label_format,
                                           significance_cutoffs,
                                           significance_symbols))
}

# ============================================================================
# STACKED BARPLOTS
# ============================================================================

#' Create Stacked Barplot
#'
#' Creates a stacked barplot showing proportions of categories within groups.
#'
#' @param data A data frame containing the data
#' @param x_var Character string. Column name for the x-axis grouping variable
#' @param fill_var Character string. Column name for the fill variable (categories)
#' @param color_palette Character vector. Named vector of colors for categories
#' @param show_labels Logical. Show count labels on bars (default: TRUE)
#' @param label_size Numeric. Size of labels (default: 2.5)
#'
#' @return A ggplot2 object
#' @export
#'
#' @examples
#' \dontrun{
#' plot_stacked_bar(data = my_data, x_var = "population",
#'                  fill_var = "subtype",
#'                  color_palette = c("Type1" = "#345995", "Type2" = "#03CEA4"))
#' }
plot_stacked_bar <- function(data, x_var, fill_var,
                              color_palette = NULL, show_labels = TRUE,
                              label_size = 2.5) {
  # Input validation
  if (!is.data.frame(data)) {
    stop("'data' must be a data frame")
  }
  if (!all(c(x_var, fill_var) %in% colnames(data))) {
    stop("Both 'x_var' and 'fill_var' must be column names in 'data'")
  }

  # Prepare data
  data_prep <- data %>%
    dplyr::rename("x_group" = !!x_var, "fill_group" = !!fill_var) %>%
    dplyr::group_by(x_group) %>%
    dplyr::count(fill_group) %>%
    dplyr::mutate(percentage = n / sum(n) * 100,
                  label = paste0(fill_group, "\n(", n, ")"))

  # Create plot
  p <- ggplot2::ggplot(data_prep,
                       ggplot2::aes(x = x_group, y = percentage, fill = fill_group)) +
    ggplot2::geom_bar(stat = "identity", width = 0.5) +
    ggplot2::scale_y_continuous(labels = scales::percent_format(scale = 1)) +
    ggpubr::theme_pubr() +
    ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                   axis.ticks.x = ggplot2::element_blank(),
                   axis.title.y = ggplot2::element_blank(),
                   axis.line.x = ggplot2::element_blank())

  # Add labels
  if (show_labels) {
    p <- p + ggplot2::geom_text(ggplot2::aes(label = label),
                                 position = ggplot2::position_stack(vjust = 0.5),
                                 color = "black", size = label_size,
                                 fontface = "bold")
  }

  # Add color palette
  if (!is.null(color_palette)) {
    p <- p + ggplot2::scale_fill_manual(values = color_palette)
  }

  return(p)
}

# ============================================================================
# VOLCANO PLOTS
# ============================================================================

#' Create Volcano Plot
#'
#' Creates a volcano plot for visualizing differential expression or similar
#' log-fold change vs significance analyses.
#'
#' @param data A data frame with columns for p-values, log fold changes, and gene/feature names
#' @param pval_col Character string. Column name for p-values
#' @param logfc_col Character string. Column name for log fold changes
#' @param gene_col Character string. Column name for gene/feature names
#' @param pval_cutoff Numeric. P-value cutoff for significance (default: 0.05)
#' @param logfc_cutoff Numeric. Log fold change cutoff (default: 0)
#' @param n_labels Numeric. Number of top genes to label (default: 10)
#' @param color_palette Named character vector. Colors for "Up", "Down", "Non-significant"
#' @param point_size Numeric. Size of points (default: 2)
#' @param text_size Numeric. Size of text labels (default: 3)
#'
#' @return A ggplot2 object
#' @export
#'
#' @examples
#' \dontrun{
#' plot_volcano(data = de_results, pval_col = "pvalue",
#'               logfc_col = "log2FoldChange", gene_col = "gene_name")
#' }
plot_volcano <- function(data, pval_col = "pval", logfc_col = "log2fc",
                         gene_col = "gene", pval_cutoff = 0.05,
                         logfc_cutoff = 0, n_labels = 10,
                         color_palette = c("Down" = "#00AFBB",
                                          "Non-significant" = "grey",
                                          "Up" = "#bb0c00"),
                         point_size = 2, text_size = 3) {
  # Input validation
  if (!is.data.frame(data)) {
    stop("'data' must be a data frame")
  }
  required_cols <- c(pval_col, logfc_col, gene_col)
  if (!all(required_cols %in% colnames(data))) {
    stop("All specified column names must exist in 'data'")
  }

  # Prepare data
  data_prep <- data %>%
    dplyr::rename("pval" = !!pval_col, "log2fc" = !!logfc_col, "gene" = !!gene_col) %>%
    dplyr::filter(!is.na(pval) & !is.na(log2fc))

  if (nrow(data_prep) == 0) {
    stop("No valid data after removing missing values")
  }

  # Define significance
  data_prep$significance <- "Non-significant"
  data_prep$significance[data_prep$pval < pval_cutoff &
                         data_prep$log2fc > logfc_cutoff] <- "Up"
  data_prep$significance[data_prep$pval < pval_cutoff &
                         data_prep$log2fc < -logfc_cutoff] <- "Down"

  # Label top genes
  top_genes <- data_prep %>%
    dplyr::arrange(pval) %>%
    dplyr::filter(significance != "Non-significant") %>%
    dplyr::slice_head(n = n_labels) %>%
    dplyr::pull(gene)

  data_prep$label <- ifelse(data_prep$gene %in% top_genes,
                            data_prep$gene, NA)

  # Create plot
  p <- ggplot2::ggplot(data_prep,
                        ggplot2::aes(x = log2fc, y = -log10(pval),
                                     col = significance, label = label)) +
    ggplot2::geom_vline(xintercept = c(-logfc_cutoff, logfc_cutoff),
                        col = "gray", linetype = "dashed") +
    ggplot2::geom_hline(yintercept = -log10(pval_cutoff),
                        col = "gray", linetype = "dashed") +
    ggplot2::geom_point(size = point_size) +
    ggplot2::scale_color_manual(values = color_palette) +
    ggplot2::coord_cartesian(
      ylim = c(0, max(-log10(data_prep$pval), na.rm = TRUE) + 1),
      xlim = c(min(data_prep$log2fc, na.rm = TRUE) - 1,
               max(data_prep$log2fc, na.rm = TRUE) + 1)
    ) +
    ggplot2::labs(color = "Significance",
                  x = expression("log"[2]*"FC"),
                  y = expression("-log"[10]*" p-value")) +
    ggrepel::geom_text_repel(max.overlaps = Inf, size = text_size) +
    ggpubr::theme_pubr(legend = "right") +
    ggplot2::guides(label = ggplot2::guide_legend("none")) +
    ggplot2::theme(text = ggplot2::element_text(size = 10),
                   title = ggplot2::element_text(size = 10))

  return(p)
}

# ============================================================================
# FOREST PLOTS
# ============================================================================

#' Create Forest Plot from Linear Models
#'
#' Creates a forest plot from linear model results, useful for visualizing
#' effect sizes and confidence intervals across multiple models or traits.
#'
#' @param model_list A list of linear model objects (lm, glm, etc.) or a data frame
#'   of tidied model results
#' @param adjust_pvals Logical. Adjust p-values using Bonferroni correction (default: FALSE)
#' @param pval_cutoff Numeric. P-value cutoff for significance (default: 0.05)
#' @param remove_intercept Logical. Remove intercept term from plot (default: TRUE)
#'
#' @return A ggplot2 object
#' @export
#'
#' @examples
#' \dontrun{
#' models <- list(model1 = lm(y ~ x1, data = df1),
#'                model2 = lm(y ~ x2, data = df2))
#' plot_forest(models, adjust_pvals = TRUE)
#' }
plot_forest <- function(model_list, adjust_pvals = FALSE, pval_cutoff = 0.05,
                        remove_intercept = TRUE) {
  # Input validation
  if (is.data.frame(model_list)) {
    tidy_results <- model_list
  } else if (is.list(model_list)) {
    tidy_results <- do.call(rbind, lapply(model_list, broom::tidy))
  } else {
    stop("'model_list' must be a list of model objects or a data frame")
  }

  if (!is.data.frame(tidy_results)) {
    stop("Could not convert model_list to data frame")
  }

  # Adjust p-values if requested
  if (adjust_pvals) {
    tidy_results$p.value <- p.adjust(tidy_results$p.value, method = "bonferroni")
  }

  # Extract trait names from row names
  tidy_results$trait <- gsub("\\..+", "", rownames(tidy_results))

  # Remove intercept if requested
  if (remove_intercept) {
    tidy_results <- tidy_results[tidy_results$term != "(Intercept)", ]
  }

  # Clean term names
  tidy_results$term <- gsub("`", "", tidy_results$term)

  # Create forest plot using ggplot2 only (replaces ggforestplot::forestplot)
  # - compute 95% confidence intervals if not provided
  if (!all(c("ci.low", "ci.high") %in% colnames(tidy_results))) {
    if (all(c("estimate", "std.error") %in% colnames(tidy_results))) {
      z <- stats::qnorm(0.975)
      tidy_results$ci.low <- tidy_results$estimate - z * tidy_results$std.error
      tidy_results$ci.high <- tidy_results$estimate + z * tidy_results$std.error
    } else if (all(c("conf.low", "conf.high") %in% colnames(tidy_results))) {
      tidy_results$ci.low <- tidy_results$conf.low
      tidy_results$ci.high <- tidy_results$conf.high
      if (!"estimate" %in% colnames(tidy_results)) {
        tidy_results$estimate <- (tidy_results$ci.low + tidy_results$ci.high) / 2
      }
    } else {
      stop("'tidy_results' must contain 'estimate' and 'std.error' or 'conf.low' and 'conf.high'")
    }
  }

  # significance flag and formatted p-value for display
  tidy_results$signif_flag <- ifelse(is.na(tidy_results$p.value), NA,
                                     ifelse(tidy_results$p.value < pval_cutoff, "sig", "ns"))
  tidy_results$p.format <- ifelse(is.na(tidy_results$p.value), NA,
                                  ifelse(tidy_results$p.value < 0.001, "<0.001",
                                         format(round(tidy_results$p.value, 3), nsmall = 3)))

  # create a unique row label (term + trait) so multiple traits/rows do not overlap
  tidy_results$row_label <- paste0(tidy_results$term,
                                   ifelse(is.na(tidy_results$trait) | tidy_results$trait == "", "",
                                          paste0(" (", tidy_results$trait, ")")))
  tidy_results$row_label <- factor(tidy_results$row_label, levels = rev(unique(tidy_results$row_label)))

  # compute x-range and a position for p-value text to the right of the CIs
  x_max <- max(tidy_results$ci.high, tidy_results$estimate, na.rm = TRUE)
  x_min <- min(tidy_results$ci.low, tidy_results$estimate, na.rm = TRUE)
  x_range <- ifelse(is.finite(x_max - x_min) && (x_max - x_min) > 0, x_max - x_min, 1)
  pval_x <- x_max + 0.06 * x_range

  p <- ggplot2::ggplot(tidy_results, ggplot2::aes(x = estimate, y = row_label, colour = trait)) +
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed", colour = "gray60") +
    ggplot2::geom_errorbarh(ggplot2::aes(xmin = ci.low, xmax = ci.high), height = 0.25, size = 0.8) +
    ggplot2::geom_point(ggplot2::aes(shape = signif_flag), size = 3, stroke = 1) +
    ggplot2::scale_shape_manual(values = c("sig" = 19, "ns" = 1), na.value = 1, guide = "none") +
    ggplot2::geom_text(ggplot2::aes(x = pval_x, label = p.format), hjust = 0, colour = "black", size = 3, show.legend = FALSE) +
    ggplot2::labs(x = "Estimate", y = NULL, colour = "Trait") +
    ggplot2::coord_cartesian(xlim = c(x_min - 0.06 * x_range, pval_x + 0.06 * x_range)) +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.y = ggplot2::element_text(size = 10),
                   legend.position = "right")

  return(p)
}

# ============================================================================
# CORRELATION PLOTS
# ============================================================================

#' Create Correlation Scatter Plot
#'
#' Creates scatter plots with correlation statistics for pairs of variables.
#'
#' @param data A data frame containing numeric variables
#' @param x_var Character string or vector. Column name(s) for x-axis variable(s)
#' @param y_var Character string or vector. Column name(s) for y-axis variable(s)
#' @param cor_method Character. Correlation method: "pearson" or "spearman" (default: "spearman")
#' @param font_size Numeric. Font size for text (default: 10)
#' @param show_cor Logical. Show correlation coefficient on plot (default: TRUE)
#' @param smooth_method Character. Smoothing method: "loess" or "lm" (default: "loess")
#'
#' @return A ggplot2 object or list of ggplot2 objects
#' @export
#'
#' @examples
#' \dontrun{
#' plot_correlation(data = my_data, x_var = "var1", y_var = "var2")
#' }
plot_correlation <- function(data, x_var, y_var, cor_method = "spearman",
                             font_size = 10, show_cor = TRUE,
                             smooth_method = "loess") {

  # Input validation
  if (!is.data.frame(data)) {
    stop("'data' must be a data frame")
  }

  all_vars <- unique(c(x_var, y_var))
  if (!all(all_vars %in% colnames(data))) {
    stop("All specified variables must exist in 'data'")
  }

  # Determine correlation parameters
  if (cor_method == "spearman") {
    add_method <- "loess"
    cor_symbol <- "rho"
  } else if (cor_method == "pearson") {
    add_method <- "reg.line"
    cor_symbol <- "R"
  } else {
    stop("'cor_method' must be 'pearson' or 'spearman'")
  }

  colnames(data) <- make.names(colnames(data))

  if (show_cor) {
    p <- ggpubr::ggscatter(data,
                            x = make.names(y_var),
                            y = make.names(x_var),
                            add = add_method,
                            add.params = list(color = "blue"),
                            cor.coef = TRUE,
                            cor.method = cor_method,
                            cor.coeff.args = list(
                              label.x.npc = "center",
                              label.y.npc = "top",
                              label.sep = "\n",
                              r.accuracy = 0.01,
                              p.accuracy = 0.01,
                              cor.coef.name = cor_symbol
                            ))
  } else {
    p <- ggpubr::ggscatter(plot_data,
                            x = make.names(y),
                            y = make.names(x),
                            add = add_method,
                            add.params = list(color = "blue"),
                            cor.coef = FALSE)
  }

  p <- p + ggplot2::theme(
    axis.title = ggplot2::element_text(size = font_size),
    axis.text = ggplot2::element_text(size = font_size),
    plot.title = ggplot2::element_text(size = font_size + 2, face = "bold")
  ) +
    ggplot2::xlab(y) +
    ggplot2::ylab(x)

  return (p)
}

# ============================================================================
# ENRICHMENT DOT PLOTS
# ============================================================================

#' Create Enrichment Dot Plot
#'
#' Creates a dot plot for enrichment analysis results (e.g., GO, KEGG, Reactome).
#'
#' @param enrichment_data A data frame with enrichment results
#' @param x_var Character string. Column name for x-axis variable (default: "Cluster")
#' @param y_var Character string. Column name for y-axis (term names, default: "Description")
#' @param color_var Character string. Column name for color variable (default: "p.adjust")
#' @param size_var Character string. Column name for size variable (default: "count")
#' @param show_category Numeric. Number of categories to show (default: 5)
#' @param title Character string. Plot title (default: "")
#' @param font_size Numeric. Font size (default: 12)
#' @param label_wrap Numeric. Character width for label wrapping (default: 30)
#' @param facet_var Optional. Character string. Column name for faceting
#'
#' @return A ggplot2 object
#' @export
plot_enrichment_dot <- function(enrichment_data, x_var = "Cluster",
                                 y_var = "Description", color_var = "p.adjust",
                                 size_var = "count", show_category = 5,
                                 title = "", font_size = 12, label_wrap = 30,
                                 facet_var = NULL) {
  # Input validation
  if (!is.data.frame(enrichment_data)) {
    stop("'enrichment_data' must be a data frame")
  }

  required_cols <- c(x_var, y_var, color_var, size_var)
  if (!all(required_cols %in% colnames(enrichment_data))) {
    stop("All specified column names must exist in 'enrichment_data'")
  }

  # Prepare data
  data_prep <- enrichment_data %>%
    dplyr::rename("x_group" = !!x_var,
                  "Description" = !!y_var,
                  "p.adjust" = !!color_var,
                  "count" = !!size_var) %>%
    dplyr::filter(!is.na(Description))

  # Limit to top categories
  if (is.numeric(show_category) && show_category < nrow(data_prep)) {
    data_prep <- data_prep %>%
      dplyr::arrange(p.adjust) %>%
      dplyr::slice_head(n = show_category)
  }

  # Factorize description for proper ordering
  data_prep$Description <- factor(data_prep$Description,
                                   levels = rev(unique(data_prep$Description)))

  # Create plot
  p <- ggplot2::ggplot(data_prep,
                        ggplot2::aes_string(x = "x_group", y = "Description",
                                            size = "count", fill = "p.adjust")) +
    ggplot2::geom_point(shape = 21) +
    ggplot2::scale_size_continuous(range = c(3, 8)) +
    enrichplot::set_enrichplot_color(type = "fill") +
    ggplot2::ylab(NULL) +
    ggplot2::ggtitle(title) +
    DOSE::theme_dose(font_size)

  # Add faceting if requested
  if (!is.null(facet_var) && facet_var %in% colnames(enrichment_data)) {
    p <- p + ggplot2::facet_grid(~ .data[[facet_var]],
                                  scales = "free_x",
                                  space = "free_x",
                                  switch = "x",
                                  labeller = ggplot2::label_wrap_gen(width = label_wrap)) +
      ggplot2::theme(strip.text = ggplot2::element_text(size = 14))
  }

  return(p)
}

# ============================================================================
# MULTI-PANEL FIGURES
# ============================================================================

#' Arrange Multiple Plots into a Single Figure
#'
#' Arranges multiple ggplot objects into a grid layout and optionally saves to file.
#'
#' @param plot_list A list of ggplot objects or a list of lists containing plots
#' @param ncol Numeric. Number of columns (default: 4)
#' @param nrow Numeric. Number of rows (default: 6)
#' @param labels Character. Labels for plots: "AUTO" for automatic, or a vector
#' @param filename Optional. File path to save the figure
#' @param width Numeric. Figure width in pixels (default: 2125)
#' @param height Numeric. Figure height in pixels (default: 3075)
#' @param dpi Numeric. Resolution (default: 150)
#' @param remove_indices Optional. Numeric vector. Indices of plots to remove
#'
#' @return A ggplot2 object (arranged plots) or NULL if saved to file
#' @export
#'
#' @examples
#' \dontrun{
#' plots <- list(plot1, plot2, plot3)
#' arrange_plots(plots, ncol = 2, nrow = 2, filename = "figure.png")
#' }
arrange_plots <- function(plot_list, ncol = 4, nrow = 6, labels = "AUTO",
                           filename = NULL, width = 2125, height = 3075,
                           dpi = 150, remove_indices = NULL) {
  # Input validation
  if (!is.list(plot_list)) {
    stop("'plot_list' must be a list")
  }

  # Extract plots from nested lists if needed
  if (length(plot_list) > 0) {
    if (is.list(plot_list[[1]]) && !inherits(plot_list[[1]], "ggplot")) {
      plot_list <- lapply(plot_list, function(x) {
        if ("plot" %in% names(x)) x$plot else x[[1]]
      })
    }
  }

  # Remove specified plots
  if (!is.null(remove_indices)) {
    plot_list[remove_indices] <- NULL
  }

  # Arrange plots
  combined <- ggpubr::ggarrange(plotlist = plot_list,
                                 ncol = ncol,
                                 nrow = nrow,
                                 labels = labels,
                                 font.label = list(size = 16))

  # Save if filename provided
  if (!is.null(filename)) {
    ggplot2::ggsave(filename = filename, plot = combined,
                     width = width, height = height, dpi = dpi, units = "px")
    return(invisible(combined))
  }

  return(combined)
}

# ============================================================================
# CLUSTERED HEATMAPS
# ============================================================================

#' Create Clustered Heatmap
#'
#' Builds a clustered heatmap using pheatmap. The input can be either a numeric
#' matrix/data.frame or a path to a tab-delimited file with row names in the
#' first column. Optional annotation columns and annotation color mappings are
#' supported.
#'
#' @param inFile A numeric matrix/data.frame or a character path to a tab-delimited file
#'   (first column used as row names).
#' @param figureName Character. Base name used when saving the figure (default: "clusteredHeatmap").
#' @param annotationColumns Optional. Either a data.frame with rownames matching the
#'   columns of the matrix, or a path to a tab-delimited file containing annotation
#'   columns (header present, rownames in first column).
#' @param annotationColors Optional. Either a list compatible with pheatmap's
#'   `annotation_colors` (named lists), or a path to a tab-delimited file with
#'   three columns: group, name, color (used to construct the named vectors).
#' @param showColNames Logical. Show column names on the heatmap (default: FALSE).
#' @param showRowNames Logical. Show row names on the heatmap (default: FALSE).
#' @param clusterCols Logical. Whether to cluster columns (default: TRUE).
#' @param clusterRows Logical. Whether to cluster rows (default: TRUE).
#' @param cellColors Character vector of colors to interpolate for the heatmap
#'   palette (default: c("#1E90FF","#FFFFE0","#DC143C")).
#' @param scaleRows Logical. If TRUE, scale rows (default: FALSE).
#' @param showAnnotColNames Logical. Show annotation column names (default: FALSE).
#' @param figureWidth Numeric. Width in pixels when saving (default: 1125).
#' @param figureHeight Numeric. Height in pixels when saving (default: 625).
#' @param dpi Numeric. Resolution used when saving (default: 150).
#' @param filename Optional. If provided, save the heatmap to this file. If NULL,
#'   a file named paste0(figureName, ".png") will be written when saving.
#'
#' @return A list invisibly containing pheatmap output (gtable and components).
#' @export
#' @examples
#'
#'
#' # From a matrix
#' \dontrun{
#' m <- matrix(rnorm(100), nrow=10)
#' rownames(m) <- paste0("gene", 1:10)
#' hm <- getClusteredHeatmap(m)
#' }
#'
getClusteredHeatmap <- function(inFile,
                                 figureName = "clusteredHeatmap",
                                 annotationColumns = NULL,
                                 annotationColors = NULL,
                                 showColNames = FALSE,
                                 showRowNames = FALSE,
                                 clusterCols = TRUE,
                                 clusterRows = TRUE,
                                 cellColors = c("#1E90FF", "#FFFFE0", "#DC143C"),
                                 scaleRows = FALSE,
                                 showAnnotColNames = FALSE,
                                 figureWidth = 1125,
                                 figureHeight = 625,
                                 dpi = 150,
                                 filename = NULL) {

  if (!requireNamespace("pheatmap", quietly = TRUE)) {
    stop("Package 'pheatmap' is required for getClusteredHeatmap()")
  }
  if (!requireNamespace("grid", quietly = TRUE)) {
    stop("Package 'grid' is required for getClusteredHeatmap()")
  }

  # Load/validate matrix
  if (is.data.frame(inFile) || is.matrix(inFile)) {
    valMatrix <- as.matrix(inFile)
  } else if (is.character(inFile) && length(inFile) == 1 && file.exists(inFile)) {
    valMatrix <- as.matrix(utils::read.table(inFile, sep = "\t", header = TRUE,
                                             row.names = 1, check.names = FALSE, stringsAsFactors = FALSE))
  } else {
    stop("'inFile' must be a matrix/data.frame or a path to an existing file")
  }

  if (!is.numeric(valMatrix)) {
    valMatrix <- apply(valMatrix, 2, as.numeric)
    rownames(valMatrix) <- rownames(inFile)
  }

  # Scaling option
  scaling <- if (isTRUE(scaleRows)) "row" else "none"

  # Prepare annotation columns (if provided)
  annotColumns <- NULL
  if (!is.null(annotationColumns)) {
    if (is.character(annotationColumns) && length(annotationColumns) == 1 && file.exists(annotationColumns)) {
      annotColumns <- utils::read.table(annotationColumns, sep = "\t", header = TRUE,
                                        row.names = 1, check.names = FALSE, stringsAsFactors = FALSE)
    } else if (is.data.frame(annotationColumns)) {
      annotColumns <- annotationColumns
    } else {
      stop("'annotationColumns' must be a data.frame or a path to a table")
    }
  }

  # Prepare annotation colors mapping
  annotColors <- NULL
  if (!is.null(annotationColors)) {
    if (is.character(annotationColors) && length(annotationColors) == 1 && file.exists(annotationColors)) {
      ac <- utils::read.delim(annotationColors, sep = "\t", header = TRUE, stringsAsFactors = FALSE, check.names = FALSE)
      # Expecting three columns: group, name, color
      if (ncol(ac) < 3) stop("annotationColors file must have at least 3 columns: group, name, color")
      colnames(ac)[1:3] <- c("group", "name", "color")
      ac$color[is.na(ac$color)] <- "NA"
      groups <- unique(ac$group)
      deColors <- list()
      for (g in groups) {
        subset_g <- ac[ac$group == g, , drop = FALSE]
        vec <- stats::setNames(as.character(subset_g$color), as.character(subset_g$name))
        deColors[[as.character(g)]] <- vec
      }
      annotColors <- deColors
    } else if (is.list(annotationColors)) {
      annotColors <- annotationColors
    } else if (is.data.frame(annotationColors)) {
      # If supplied as data.frame already, try same conversion as above
      ac <- annotationColors
      if (ncol(ac) >= 3) {
        colnames(ac)[1:3] <- c("group", "name", "color")
        groups <- unique(ac$group)
        deColors <- list()
        for (g in groups) {
          subset_g <- ac[ac$group == g, , drop = FALSE]
          vec <- stats::setNames(as.character(subset_g$color), as.character(subset_g$name))
          deColors[[as.character(g)]] <- vec
        }
        annotColors <- deColors
      } else {
        stop("annotationColors data.frame must have at least 3 columns: group, name, color")
      }
    } else {
      stop("'annotationColors' must be a list, data.frame, or a path to a file")
    }
  }

  # Build color palette
  if (!is.character(cellColors) || length(cellColors) < 2) {
    stop("'cellColors' must be a character vector of at least two colors")
  }
  myColor <- grDevices::colorRampPalette(cellColors)(101)

  # Call pheatmap
  heatmapFigure <- pheatmap::pheatmap(mat = valMatrix,
                                      color = myColor,
                                      scale = scaling,
                                      treeheight_row = 100,
                                      border_color = NA,
                                      clustering_method = "complete",
                                      margins = c(5, 5),
                                      cluster_rows = clusterRows,
                                      cluster_cols = clusterCols,
                                      clustering_distance_rows = "euclidean",
                                      clustering_distance_cols = "euclidean",
                                      annotation_names_row = FALSE,
                                      annotation_names_col = showAnnotColNames,
                                      show_rownames = showRowNames,
                                      show_colnames = showColNames,
                                      annotation_col = annotColumns,
                                      annotation_colors = annotColors,
                                      fontsize = 7,
                                      silent = TRUE)

  # Save output if requested
  if (!is.null(filename) || !is.null(figureName)) {
    out_file <- filename
    if (is.null(out_file)) out_file <- paste0(figureName, ".png")
    grDevices::png(filename = out_file, width = figureWidth, height = figureHeight, units = "px", res = dpi)
    grid::grid.newpage()
    grid::grid.draw(heatmapFigure$gtable)
    grDevices::dev.off()
  }

  invisible(heatmapFigure)
}

# ============================================================================
# HELPER FUNCTIONS (Internal)
# ============================================================================

# Internal function to check normality
.check_normality <- function(data, x_var, y_var) {
  norm_pvals <- tapply(seq(nrow(data)), data[[x_var]], function(idx) {
    values <- data[[y_var]][idx]
    values <- values[!is.na(values)]

    if (length(values) < 3 || length(unique(values)) == 1) {
      return(0)
    }

    tryCatch({
      shapiro.test(values)$p.value
    }, error = function(e) {
      return(0)
    })
  })

  if (any(is.na(norm_pvals))) {
    return(FALSE)
  }

  return(all(norm_pvals > 0.05))
}

# Internal function to perform statistical tests
.perform_statistical_test <- function(data, x_var, y_var, parametric, is_multivariate) {
  if (is_multivariate) {
    if (parametric) {
      model <- aov(data[[y_var]] ~ data[[x_var]])
      pval <- summary(model)[[1]][["Pr(>F)"]][1]
      test_name <- "ANOVA"

      # Post-hoc
      tukey <- TukeyHSD(model)
      posthoc <- as.data.frame(tukey[[1]])
      posthoc$Comparison <- rownames(posthoc)
      posthoc <- posthoc[, c("Comparison", "diff", "p adj")]
      colnames(posthoc) <- c("Comparison", "Difference", "p.adj")

      return(list(
        test_name = "anova",
        p_value = pval,
        model = model,
        posthoc = posthoc
      ))
    } else {
      kruskal_result <- kruskal.test(data[[y_var]] ~ data[[x_var]])
      pval <- kruskal_result$p.value
      test_name <- "Kruskal-Wallis"

      # Post-hoc using FSA if available
      if (requireNamespace("FSA", quietly = TRUE)) {
        dunn_result <- FSA::dunnTest(data[[y_var]] ~ data[[x_var]],
                                      method = "bh", two.sided = TRUE)
        posthoc <- dunn_result$res[, c("Comparison", "Z", "P.adj")]
        colnames(posthoc) <- c("Comparison", "Difference", "p.adj")
      } else {
        posthoc <- NULL
      }

      return(list(
        test_name = "kruskal.test",
        p_value = pval,
        model = kruskal_result,
        posthoc = posthoc
      ))
    }
  } else {
    groups <- unique(data[[x_var]])
    if (length(groups) != 2) {
      return(NULL)
    }

    group1_data <- data[[y_var]][data[[x_var]] == groups[1]]
    group2_data <- data[[y_var]][data[[x_var]] == groups[2]]

    if (parametric) {
      test_result <- t.test(group1_data, group2_data, paired = FALSE,
                            var.equal = FALSE, alternative = "two.sided")
      test_name <- "Welch T-test"
    } else {
      test_result <- wilcox.test(group1_data, group2_data, paired = FALSE,
                                  alternative = "two.sided")
      test_name <- "Wilcoxon rank-sum test"
    }

    return(list(
      test_name = if (parametric) "t.test" else "wilcox.test",
      p_value = test_result$p.value,
      model = test_result,
      posthoc = NULL
    ))
  }
}

# Internal function to calculate summary statistics
.calculate_summary_stats <- function(data, x_var, y_var) {
  summary_stats <- data %>%
    dplyr::group_by(!!sym(x_var)) %>%
    dplyr::summarise(
      Mean = mean(!!sym(y_var), na.rm = TRUE),
      Median = median(!!sym(y_var), na.rm = TRUE),
      SD = sd(!!sym(y_var), na.rm = TRUE),
      Q1 = quantile(!!sym(y_var), 0.25, na.rm = TRUE, type = 5),
      Q3 = quantile(!!sym(y_var), 0.75, na.rm = TRUE, type = 5),
      N = dplyr::n(),
      .groups = "drop"
    )

  return(summary_stats)
}

# Internal function to add post-hoc annotations
.add_posthoc_annotations_internal <- function(plot, data, x_var, y_var,
                                              posthoc_results, pval_column,
                                              group_var, step_increase,
                                              tip_length, label_format,
                                              significance_cutoffs,
                                              significance_symbols) {
  # Prepare posthoc data
  posthoc_prep <- posthoc_results %>%
    tidyr::separate(!!sym("Comparison"), c("group1", "group2"),
                    sep = " - ", remove = FALSE) %>%
    dplyr::rename(p.adj = !!sym(pval_column), comparison = Comparison) %>%
    rstatix::add_significance(p.col = "p.adj",
                              cutpoints = significance_cutoffs,
                              symbols = significance_symbols) %>%
    dplyr::filter(p.adj.signif != "ns")

  if (nrow(posthoc_prep) == 0) {
    return(plot)
  }

  # Get y positions
  if (is.null(group_var)) {
    y_pos <- rstatix::get_y_position(
      data = data,
      formula = as.formula(paste(y_var, "~", x_var)),
      step.increase = step_increase
    )
  } else {
    y_pos <- data %>%
      dplyr::rename("xVar" = !!sym(x_var)) %>%
      dplyr::group_by(xVar) %>%
      rstatix::get_y_position(
        formula = as.formula(paste(y_var, "~", group_var)),
        step.increase = step_increase
      )
  }

  # Merge positions
  posthoc_annot <- posthoc_prep %>%
    dplyr::inner_join(y_pos, by = c("group1", "group2"))

  # Determine label column
  if (label_format == "symbol") {
    label_col <- "p.adj.signif"
    label_size <- 3.88
  } else {
    posthoc_annot$p.adj <- format_pvalue(posthoc_annot$p.adj)
    label_col <- "p.adj"
    label_size <- 2.8
  }

  # Add annotations
  plot <- plot + ggpubr::stat_pvalue_manual(
    posthoc_annot,
    label = label_col,
    hide.ns = TRUE,
    tip.length = tip_length,
    step.increase = step_increase,
    size = label_size
  )

  return(plot)
}

# ============================================================================
# UTILITY FUNCTIONS
# ============================================================================

#' Add Sample Size Information to Axis Labels
#'
#' Adds sample size (N) information to categorical axis labels.
#'
#' @param data A data frame
#' @param x_var Character string. Column name for the categorical variable
#'
#' @return A data frame with updated labels
#' @export
add_sample_size_labels <- function(data, x_var) {
  if (!is.data.frame(data)) {
    stop("'data' must be a data frame")
  }
  if (!x_var %in% colnames(data)) {
    stop("'x_var' must be a column name in 'data'")
  }

  sample_sizes <- data %>%
    dplyr::rename("Old_var" = !!sym(x_var)) %>%
    dplyr::group_by(Old_var) %>%
    dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
    dplyr::mutate(New_var = paste0(Old_var, "\n(N=", n, ")"))

  data <- data %>%
    dplyr::left_join(sample_sizes, by = setNames("Old_var", x_var)) %>%
    dplyr::mutate(!!sym(x_var) := NULL) %>%
    dplyr::rename(!!sym(x_var) := New_var)

  return(data)
}

#' Format P-values for Display
#'
#' Formats p-values for statistical tables and plots.
#'
#' @param x Numeric vector of p-values
#' @param digits Numeric. Number of digits (1, 2, or 3, default: 3)
#' @param prepend_p Logical. Prepend "p=" or "p" to values (default: FALSE)
#'
#' @return Character vector of formatted p-values
#' @export
format_pvalue <- function(x, digits = 3, prepend_p = FALSE) {
  if (!digits %in% c(1, 2, 3)) {
    stop("'digits' must be 1, 2, or 3")
  }

  # Format p-values
  p_fmt <- dplyr::case_when(
    x > 1 + 1e-15 ~ NA_character_,
    x < 0 - 1e-15 ~ NA_character_,
    x >= 0.1 ~ format(round(x, digits), nsmall = digits),
    x >= 0.001 ~ format(round(x, digits), nsmall = digits),
    x < 0.001 ~ format(signif(x, 3), scientific = TRUE),
    is.na(x) ~ NA_character_
  )

  if (prepend_p) {
    p_fmt <- dplyr::case_when(
      is.na(p_fmt) ~ NA_character_,
      grepl("<|>", p_fmt) ~ paste0("p", p_fmt),
      TRUE ~ paste0("p=", p_fmt)
    )
  }

  return(p_fmt)
}
