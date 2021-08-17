#' @title Help function for logfc_cols()
#' @description 
#' This function checks whether the input exists in the df:
#' - for strings checks the column names
#' - for numbers checks whether it is smaller than the number of columns
#'
#' @param data input df
#' @param input column number or column to check 
#' @return error or pass
#' @export
#' @keyword Internal
test_input_type <- function(data, input) {
    if (is.numeric(input)) {
        if (ncol(data) < input) {
            stop("One of the column numbers exceeds the amount of dataframe columns")
        } else if (input < 0) {
            stop("One of the column numbers < 0")
        }
    } else if (is.character(input)) {
        if (identical(input %in% colnames(data), FALSE)) {
            stop("Column with the input name does not exist")
        }
    } 
}