#' Load csv data
#'
#' @param data.path Path to the csv file
#' @param fn Name of the csv file
#' @param header csv file has a header, TRUE/FALSE
#'
#' @return Data frame of the csv
#' @export
#'
#' @examples
#' \donttest
load.csv <- function(data.path,
                      fn,
                      header = TRUE){

  read.csv(file.path(data.path, fn),
           comment.char = "#",
           header = header,
           stringsAsFactors = FALSE)
}
