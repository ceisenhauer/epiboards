#' Reactable bar cell
#' 
#' @description Adds bar styling to a reactable cell through embedded CSS.
#' 
#' @param width `num` Proportion (not percent) of cell to fill with the bar (width wise). 
#' @param fill `chr` CSS appropriate string indicating bar fill.
#' @param color `chr` CSS appropriate sting indicating text color.
#' @param font_weight `chr` Font weight.
#' @param border_right `bool / chr` Border to be used on the right side of the cell. If `TRUE` a 
#'   default border style of a thin grey line is used. The user can also pass a CSS style string.
#'
#' @export
render_reactable_bar <- function(width, fill = "#e6e6e6", color = '#000000', font_weight = 'bold',
                                 border_right = NULL) {
  width <- paste0(width * 100, "%")
  bar <- sprintf("linear-gradient(90deg, %1$s %2$s, transparent %2$s)", fill, width)

  out <- list(backgroundImage = bar,
              backgroundSize = paste('100%', '75%'),
              backgroundRepeat = 'no-repeat',
              backgroundPosition = 'center',
              color = color,
              fontWeight = font_weight,
              borderRight = render_reactable_border(border_right))

  return(out)
}

#' Reactable cell border
#' 
#' @description Convenience function to add a cell border to a reactable.
#' 
#' @param border `bool / chr` A user can either pass `TRUE` to add a default border (thin and grey)
#'   or they can pass their own CSS style string. Anything else will result in no border. Default
#'   is NULL.
#'
#' export
render_reactable_border <- function(border = NULL) {
  out <- if (is.null(border)) {
           return(NULL)
         } else if (border) {
           return("1px solid rgba(0, 0, 0, 0.1)")
         } else if (is(border, 'character')) {
           return(border)
         } else {
           warning('border must be TRUE or a valid style string')
           return(NULL)
         }
}

#' Reactable headers (with tooltips)
#'
#' @description Convenience function to facilitate adding tooltips to reactable headers.
#'
#' @param name `chr` Column name.
#' @param tooltip `chr` Tooltip to be added to the header text. Default is NULL (no tooltip).
#' 
#' @importFrom htmltools div
#' @importFrom tippy tippy
#' @export
render_reactable_header <- function(name, tooltip = NULL) {
  if (is.null(tooltip)) {
    out <- name
  } else {
    out <- htmltools::div(style = paste('text-decoration: underline;',
                                        'text-decoration-style: dotted;',
                                        'cursor: help'),
                          tippy::tippy(name,
                                       paste0('<span style="font-size:16px;">',
                                              tooltip,
                                              '</span>'),
                                       allowHTML = TRUE))
  }
  
  return(out)
}

