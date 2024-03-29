% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/render_reactable_columns.R
\name{col_bar}
\alias{col_bar}
\title{Reactable bar column}
\usage{
col_bar(
  df,
  col,
  name = col,
  tooltip = NULL,
  color = "#c5c5c5",
  digits = 0,
  percent = FALSE,
  ...,
  min_width = 75,
  border_right = NULL
)
}
\arguments{
\item{df}{\code{dataframe} Dataframe of data used in the table.}

\item{col}{\code{chr} Column name of the data (in \code{df}).}

\item{name}{\code{chr} Display name of the column to be used as the header. Default is \code{col}.}

\item{tooltip}{\code{chr} Tooltip text to be added to the header. Default is NULL.}

\item{color}{\code{chr} CSS appropriate string indicating the bar color.}

\item{digits}{\code{int} Number of decimal digits to display. Default is 0.}

\item{percent}{\code{bool} Whether to format the column as a percent. Default is FALSE.}

\item{...}{Additional arguments to be passed to \link[reactable:reactable]{reactable::reactable}.}

\item{min_width}{\code{int} Minimum width for the column.}

\item{border_right}{\code{bool / chr} Right side border to be added. Default is NULL.}
}
\description{
Creates a \link[reactable:colDef]{reactable::colDef} object with bar formatting (horizontal bars are added
to the background of each cell proprtional to their value scaled by the maximum of the column).
Useful for columns of numeric data.
}
