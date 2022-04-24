#' @include package-functions.R
#' @include filehandler-class.R
#' @include afterflow-class.R

#' @title Plot Dimensional Reduction
#'
#' @description
#' Makes sure the passed object is a FileHandler object
#'
#' @param fh FileHandler object
#' @param af AfterFlow object
#' @param color What to color each dot by in the plot, default "cluster.id"
#' @param af AfterFlow object
#' @param af AfterFlow object
#'
#' @importFrom ggplot2 ggplot aes_string geom_point
#'
#' @return Nothing
#'
#' @examples
#' plot_dimred(fh, af) -> Saves the default dimensional reduction plot

plot_dimred <- function(fh, af, color = "cluster.id",
                        prefix = character(), verbose = TRUE) {
  afterflow_check(af)
  dimred_names <- c("umap" = "UMAP", "tsne" = "tSNE")
  if (af@dimred == character())
    show_message("AfterFlow object has not had any dimensional reduction methods run",
                 final = "stop")
  dim_xy <- dimred_names[af@dimred]
  if (!(color %in% colnames(af@metadata)))
    show_message("Can't color UMAP by given variable", color,
                 final = "stop")

  aes_dimred <- aes_string(x = paste0(dim_xy, "_1"), x = paste0(dim_xy, "_2"),
                           color = color)
  filehandler_save_figure(
    fh,
    ggplot(af@metadata, aes_dimred) + geom_point(),
    paste(
      c(
        prefix,
        af@transform_method,
        sprintf("%s_DimRed_by_%s", af@dimred, color)
      ),
      collapse = "_"
    ),
    verbose = verbose,
    width = 10,
    height = 10
  )
  return()
}

#' @title Plot Dimensional Reduction
#'
#' @description
#' Makes sure the passed object is a FileHandler object
#'
#' @param fh FileHandler object
#' @param af AfterFlow object
#' @param color What to color each dot by in the plot, default "cluster.id"
#' @param af AfterFlow object
#' @param af AfterFlow object
#'
#' @importFrom ggplot2 ggplot aes_string geom_tile scale_fill_gradient2
#' @importFrom dplyr across all_of group_by summarise ungroup mutate gather
#'
#' @return Nothing
#'
#' @examples
#' plot_heatmap(fh, af) -> Saves the default scaled heatmap

plot_heatmap <- function(fh, af, group = "cluster.id", scale = TRUE,
                         prefix = character(), verbose = TRUE) {
  scale_vector <- function(vec, rabs = c(2, 4), rsca = c(-1, 1)) {
    if (min(vec) == max(vec))
      return(vec)
    vec_min <- max(rabs[1], min(vec))
    vec_max <- min(rabs[2], max(vec))
    scale_value <- function(val) {
      return(
        (
          (max(vec_min, min(vec_max, val)) - vec_min) /
            (vec_max - vec_min)
        ) *
          (rsca[2] - rsca[1]) +
          rsca[1]
      )
    }
    return(sapply(vec, scale_value))
  }
  afterflow_check(af)
  markers <- afterflow_markers(af, TRUE, verbose)
  data <- group_by(data, af@metadata, across(all_of(group)))
  data <- ungroup(summarise(data, across(all_of(names(markers))), mean))
  if (scale)
    data <- mutate(data, across(all_of(names(markers))), scale_vector)
  data <- gather(data, "Marker", "Logicle-MFI", names(markers))
  data[["Marker"]] <- markers[data[["Marker"]]]
  aes_hm <- aes_string(
    x = sprintf("`%s`", group), y = "Marker", fill = "Logicle-MFI"
  )
  filehandler_save_figure(
    fh,
    ggplot(data, aes_hm) + geom_tile(color = "black") +
      scale_fill_gradient2(
        low = "#075AFF", mid = "#FFFFCC", high = "#FF0000",
        midpoint = ifelse(scale, 0, 2.5)
      ) +
      theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
            axis.title.x = element_blank(),
            legend.text = element_blank()),
    paste(
      c(
        prefix,
        af@transform_method,
        sprintf("Heatmap_by_%s", group)
      ),
      collapse = "_"
    ),
    verbose = verbose,
    width = 15,
    height = 15
  )
  return()
}


#'
#'
#' #' Density plot
#' #'
#' #' Makes a ridge-density plot
#' #'
#' #' @param df Data.frame with data
#' #' @param markers Marker list
#' #' @param filename Filename
#' #' @param yvar Y variable
#' #' @param colorvar Color variable
#' #' @param dataset_names Dataset names
#' #' @param ncol Number of columns
#' #' @param covar Covariable
#' #'
#' #' @return ggplot
#' #' @export
#' #'
#' #' @examples
#' #' ggplot
#' plot_density <- function(df, markers, filename = NULL, yvar = "batch",
#'                          colorvar = "Type", dataset_names = NULL, ncol = 6,
#'                          covar = NULL) {
#'   if ("AfterFlow" %in% class(df))
#'     df <- df@fcs
#'   df <- dplyr::select(df, dplyr::all_of(c(markers, yvar, colorvar)))
#'   p <- lapply(
#'     markers,
#'     function(marker)
#'       ggplot2::ggplot(
#'         df,
#'         ggplot2::aes_string(
#'           x = paste0("`", marker, "`"),
#'           y = yvar
#'         )
#'       ) +
#'       ggridges::geom_density_ridges(
#'         ggplot2::aes_string(color = paste0("`", colorvar, "`"), fill = paste0("`", colorvar, "`")),
#'         alpha = 0.4
#'       ) +
#'       ggplot2::labs(y = yvar) +
#'       ggplot2::theme_bw()
#'   )
#'   legend <- cowplot::get_legend(p[[1]] +
#'                                   ggplot2::theme(
#'                                     legend.box.margin = ggplot2::margin(0, 0, 0, 12)
#'                                   ))
#'   p <- lapply(p, function(pl) pl + ggplot2::theme(legend.position = "none"))
#'   p <- cowplot::plot_grid(
#'     cowplot::plot_grid(plotlist = p, ncol = ncol),
#'     legend,
#'     rel_widths = c(2 * ncol, 1)
#'   )
#'   if (is.null(covar))
#'   #   covar <- "none"
#'   # title <-
#'   #   p <- cowplot::plot_grid(
#'   #     cowplot::ggdraw() +
#'   #       cowplot::draw_label(
#'   #         sprintf("Densities by %s (Covar: %s)", yvar, covar),
#'   #         fontface = 'bold',
#'   #         x = 0,
#'   #         hjust = 0
#'   #       ) +
#'   #       ggplot2::theme(
#'   #         plot.margin = margin(0, 0, 0, 7)
#'   #       ), p,
#'   #     ncol = 1,
#'   #     rel_heights = c(0.1, ceiling(length(markers) / ncol))
#'   #   )
#'
#'   if (!is.null(filename))
#'     cowplot::save_plot(filename, p, base_width = 20, base_height = length(markers)/1.3)
#'   else
#'     return(p)
#' }
