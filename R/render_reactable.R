#' Render Reactable
#' 
#' @description Function to create a general reactable table with sensible defaults.
#'
#' @param df `dataframe` Data to be tablized.
#' @param columns `list` Column definitions, should follow the standard reactable format. Default is
#'   NULL.
#' @param column_groups `list` Convenience arggument to simplify setting column groups. Should be a
#'   named list where keys are the group names and values are vectors of the column names within
#'   the corresponding group. Default is NULL.
#' @param digits `int` Default number of digits to show for numeric columns. Default is 0.
#' @param elementId `chr` Table id, used primarily for JS functions. Default is `'my-select'`.
#' @param fullWidth `bool` Whether tables should be expanded to fill a window by default. Default is
#'   FALSE.
#' @param searchable `bool` Whether tables should be searchable by default. Default is TRUE.
#' @param page_size `int` Number of observations to show per page. Default is 25.
#' @param sort_by `chr` Name of column to sort the table by. Default is NULL.
#' @param sort_order `chr` Whether to sort the `sort_by` column in ascending or descending order.
#'   Default is `'desc'`.
#'
#' @export
render_reactable <- function(df, columns = NULL, column_groups = NULL, digits = 0,
                             elementId = 'my-select',
                             fullWidth = FALSE,
                             searchable = TRUE, page_size = 25,
                             sort_by = NULL, sort_order = 'desc', ...) {

  # header groups
  if (!is.null(column_groups)) {
    colGroups <- list()
    for (i in 1:length(column_groups)) {
      group_name <- names(column_groups)[[i]]
      colGroups[[i]] <- colGroup(name = group_name,
                                 columns = column_groups[[group_name]])
    }
  } else {
    colGroups <- NULL
  }


  # sorting
  if (!is.null(sort_by)) {
    sorting <- list()
    sorting[[sort_by]] = sort_order
  } else {
    sorting <- NULL
  }

  out <- reactable(data = df,
                   style = list(fontFamily = 'Arial',
                                fontSize = '12px'),
                   resizable = TRUE,
                   highlight = TRUE,
                   searchable = searchable,
                   fullWidth = fullWidth,
                   defaultPageSize = page_size,
                   defaultSorted = sorting,
                   defaultColDef = colDef(format = colFormat(digits = digits,
                                                             separators = TRUE),
                                          sortNALast = TRUE,
                                          align = 'left'),
                   columns = columns,
                   columnGroups = colGroups,
                   elementId = element_id,
                   ...)
  
  return(out)
}

