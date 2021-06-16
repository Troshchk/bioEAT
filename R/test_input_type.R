#' CONVERTING IDS USING orgDB
#' This function returns a dataframe with the translated IDs
#'
#' @examples
#' @param input string or character or variable
#' @param from input ID type
#' @param to output ID type or list of output ID types
#' @param db annotation database
#' @import clusterProfiler
#' @return dataframe with the translated IDs
#' @export
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