#' @title Calculating logFC between two columns of a dataframe
#' @description
#' This function returns dataframe with 3 columns col1, col2, log2FC. 
#' Mind the column names order: the logFC is always calculated between the first anf the second input.
#' @details 
#' As an example, when this function is useful is lack of data. In order to find the 
#' differentially expressed genes more samples are needed. In case of fewer samples an 
#' alternative approach is to manually calculate the expected behaviour of the desired
#' output genes. 
#' For example, we expect a gene to have lfc > 3 for samples 1vs2 and 5vs6. In this case
#' the logfc_cols() function could be used to calculate and order individual logFold changes,
#' which could be further used to select the genes with the desired begaviour.
#' @examples
#' logfc_cols(dataNorm_df, "column_name", "5")
#' logfc_cols(dataNorm_df, "column_name", 5)
#' logfc_cols(dataNorm_df, 1, 5)
#' @param df dataframe with normalized counts
#' @param col1 name or number of the column in df
#' @param col2 name or number the column in df
#' @return dataframe with 3 columns col1, col2, log2FC
#' @import gtools
#' @export
logfc_cols <- function(data, col1, col2) {
    
    formalNames <- names(formals())

    if (do.call(missing, list(formalNames[1])) || length(data) == 0 || is.na(data) || is.null(data)) {
        stop("The input dataframe is empty or missing")
    }

    if (do.call(missing, list(formalNames[2])) || do.call(missing, list(formalNames[3])) || is.na(col1) || is.null(col1) || is.na(col2) || is.null(col2)) {
        stop("The input for column or columns is missing")
    }

    tryCatch(
        {
            test_input_type(data, col1)
            test_input_type(data, col2)
        },
        error = function(msg) {
            message("Column input name or names do not exist in the input dataframe")
        }
    )

    f <- foldchange(data[, col1], data[, col2])
    dataFrame <- cbind(rownames(data), data[, col1], data[, col2], f)
    rownames(dataFrame) <- dataFrame[, 1] # Assigning row names from 1st column
    dataFrame <- dataFrame[, c(2, 3, 4)]
    dataFrame <- as.data.frame(dataFrame)
    dataFrame[1:3] <- lapply(dataFrame[1:3], as.character)
    dataFrame[1:3] <- lapply(dataFrame[1:3], as.numeric)


    dataFrame <- dataFrame[complete.cases(dataFrame), ]
    dataFrame <- dataFrame[is.finite(rowSums(dataFrame)), ]
    dataFrame[, 3] <- foldchange2logratio(dataFrame[, 3], base = 2)
    colnames(dataFrame) <- c(col1, col2, "log2FC")

    dataFrame <- dataFrame[complete.cases(dataFrame), ]
    dataFrame <- dataFrame[is.finite(rowSums(dataFrame)), ]

    dataFrame <- dataFrame[
        order(abs(dataFrame[, 3])),
    ]


    return(dataFrame)
}