#' Titles for Echarts grid
#' 
#' @description Adds titles to echart subplots built using a grid (for example with
#' [echarts4r::e_facet()]. **Note**: This function will work with grids specified entirely in
#' percent or entirely in pixels only. It will not work for grids using a mix of pixels and percent.
#' TODO: need to make this work for pixels.
#'
#' @param e `echart` Chart to be modified.
#' @param titles `vector / list` Names to be used. **Note**: [e_grid_titles()] will use the order
#'   in `titles` to assign titles along the grid (left to right, top to bottom). So, if you made a
#'   facet plot using a charcter grouping variable, you will probably need to sort `titles`
#'   alphabetically for the order to match the grid.
#' @param top_space `int` How far above the plot the title should be placed. The units of
#'   `top_space` (percent vs pixels) will be determined automatically to match the units used in 
#'   the specification of the grid. Default value is 2.
#'
#' @importFrom echarts4r e_title
#' @importFrom dplyr %>%
#'
#' @export
#'
#' @examples
#' groups <- 6
#' observations <- 10
#' df <- data.frame(x = rep(1:observations,
#'                          times = groups),
#'                  y = runif(groups * observations, 0, 1),
#'                  group = rep(paste('Group', 1:6), each = observations))
#' 
#' df %>%
#'   dplyr::group_by(group) %>%
#'   echarts4r::e_charts(x) %>%
#'   echarts4r::e_bar(y,
#'                    name = 'Outcome') %>%
#'   echarts4r::e_facet(rows = 2,
#'                      cols = 3,
#'                      legend_space = 5) %>%
#'   e_grid_titles(titles = unique(df$group))
e_grid_titles <- function(e, titles, top_space = 2) { 
  grid <- e$x$opts$grid

  if (is.null(grid)) {
    stop('there is no subplot grid present, use e_title() instead')
  }

  if (length(grid) != length(titles)) {
    stop('number of titles does not match the number of subplots')
  }

  # check that grid uses percents only (adapt later for pixels)
  if (!all(grepl('%', unlist(grid)))) {
    stop('sorry, grid must be specified using percents only')
  }

  for (i in 1:length(titles)) {
    #percents <- grepl('%',
                      #grid[[i]])

    subplot <- lapply(grid[[i]],
                      function(x) as.numeric(sub('%', '', x)))

    left <- subplot$left + subplot$width / 2
    top <- subplot$top - 2

    e <- e %>%
           echarts4r::e_title(titles[[i]],
                              textAlign = 'center',
                              left = paste0(left, '%'),
                              top = paste0(top, '%'))
  }

  return(e)
}

