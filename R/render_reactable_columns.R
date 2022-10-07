#' Reactable bar column
#' 
#' @description Creates a [reactable::colDef] object with bar formatting (horizontal bars are added
#' to the background of each cell proprtional to their value scaled by the maximum of the column).
#' Useful for columns of numeric data.
#'
#' @param df `dataframe` Dataframe of data used in the table.
#' @param col `chr` Column name of the data (in `df`).
#' @param name `chr` Display name of the column to be used as the header. Default is `col`.
#' @param tooltip `chr` Tooltip text to be added to the header. Default is NULL.
#' @param color `chr` CSS appropriate string indicating the bar color.
#' @param digits `int` Number of decimal digits to display. Default is 0.
#' @param percent `bool` Whether to format the column as a percent. Default is FALSE.
#' @param min_width `int` Minimum width for the column.
#' @param border_right `bool / chr` Right side border to be added. Default is NULL.
#' @param ... Additional arguments to be passed to [reactable::reactable].
#' 
#' @importFrom reactable colDef colFormat
#' @importFrom htmlwidgets JS
#' @export
col_bar <- function(df, col, name = col, tooltip = NULL, color = '#c5c5c5',
                    digits = 0, percent = FALSE, ...,
                    min_width = 75, border_right = NULL) {
  out <- reactable::colDef(
	  header = render_reactable_header(name, tooltip),
    format = reactable::colFormat(digits = digits,
                                  percent = percent,
                                  separators = TRUE),
    style = function(value) {
      if (is.na(value)) {
      render_reactable_bar(width = 0 / max(df[[col]], na.rm = TRUE),
                           fill = color,
                           border_right = border_right)
      } else {
      render_reactable_bar(width = value / max(df[[col]], na.rm = TRUE),
                           fill = color,
                           border_right = border_right)
      }
    },
    minWidth = min_width,
    filterable = TRUE,
    filterMethod = htmlwidgets::JS('filterMinValue'),
    filterInput = htmlwidgets::JS('rangeFilter'),
    ...)

  return(out)
}


#' Reactable base column
#' 
#' @description Creates a [reactable::colDef] object.
#'
#' @param name `chr` Display name of the column to be used as the header. 
#' @param tooltip `chr` Tooltip text to be added to the header. Default is NULL.
#' @param color `chr` CSS appropriate string indicating the font color.
#' @param digits `int` Number of decimal digits to display. Default is 0.
#' @param percent `bool` Whether to format the column as a percent. Default is FALSE.
#' @param min_width `int` Minimum width for the column.
#' @param border_right `bool / chr` Right side border to be added. Default is NULL.
#' @param ... Additional arguments to be passed to [reactable::reactable].
#' 
#' @importFrom reactable colDef colFormat
#' @export
col_base <- function(name, tooltip = NULL, color = '#000000', digits = 0, percent = FALSE, ...,
                     min_width = 75, sticky = NULL, border_right = NULL) {

  out <- reactable::colDef(header = render_reactable_header(name, tooltip),
                           sticky = sticky,
                           format = reactable::colFormat(digits = digits,
                                                         percent = percent,
                                                         separators = TRUE),
                           minWidth = min_width,
                           style = list(color = color,
                                        borderRight = render_reactable_border(border_right)),
													 filterable = TRUE,
                           ...)

  return(out)
}

#' Reactable badge column
#' 
#' @description Creates a [reactable::colDef] object with colored ovals in the background. Useful
#' for columns of categorical data.
#'
#' @param name `chr` Display name of the column to be used as the header.
#' @param tooltip `chr` Tooltip text to be added to the header. Default is NULL.
#' @param colors `list` Named list of CSS appropriate strings indicating the badge colors. Keys
#'   should be the data values associated with each color.
#' @param min_width `int` Minimum width for the column.
#' @param border_right `bool / chr` Right side border to be added. Default is NULL.
#' @param ... Additional arguments to be passed to [reactable::reactable].
#' 
#' @importFrom reactable colDef
#' @export
col_badge <- function(name, tooltip = NULL, colors, ..., 
                      min_width = 75, border_right = NULL) {
  out <- reactable::colDef(header = render_reactable_header(name, tooltip),
                           align = 'center',
                           style = list(color = 'black',
                                        borderRight = render_reactable_border(border_right),
                                        fontWeight = 'bold'),
                           cell = function(value) {
                             if (is.na(value)) {
                               color <- 'rgba(0, 0, 0, 0)'
                             } else {
                               color <- colors[[value]]
                             }

                             css <- paste('display: inline-block;',
                                          'padding: 0.3rem 0.75rem;',
                                          'border-radius: 15px;',
                                          'font-weight: 600;',
                                          'font-size: 0.99rem;',
                                          'background:', color, ';')

                             div(style = css, value)
                           },
													 filterable = TRUE,
                           ...)

  return(out)
}

  
#' Reactable sparkline column
#' 
#' @description Creates a [reactable::colDef] object with sparklines.
#'
#' @param data `vct` Vector of values to be plotted in the sparkline.
#' @param name `chr` Display name of the column to be used as the header. Default is `'Spark'`.
#' @param tooltip `chr` Tooltip text to be added to the header. Default is NULL.
#' @param color `chr` CSS appropriate string indicating the sparkline bar color.
#' @param min_width `int` Minimum width for the column.
#' @param border_right `bool / chr` Right side border to be added. Default is NULL.
#' @param ... Additional arguments to be passed to [reactable::reactable].
#' 
#' @export
col_sparkline <- function(data, name = 'Spark', tooltip = NULL, color = '#c5c5c5', ...,
                          border_right = NULL, max_width = 60) {
  out <- reactable::colDef(header = render_reactable_header(name, tooltip),
                           filterable = FALSE,
                           align = 'center',
                           style = list(borderRight = render_reactable_border(border_right)),
                           maxWidth = max_width,
                           cell = function(value, index) {sparkline::sparkline(data[[index]],
                                                   type = 'bar',
                                                   barColor = color)})

  return(out)
}

