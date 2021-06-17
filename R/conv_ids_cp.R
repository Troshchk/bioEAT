#' @title Converting gene IDS using orgDB
#' @description
#' This function returns a dataframe with the translated IDs.
#'@details
#' This function takes a vector of IDs and translated them from 
#' the input format to the output format using the selected orgDB.
#' In case the orgDB is not installed before launching the function,
#' the function will exit with error.
#' @examples
#' conv_ids_cp(rownames(df), "ENSEMBL", c("ENTREZID", "GENENAME"), "org.Mmu.eg.db")
#' conv_ids_cp(c("IFNA1", "IFNA13", "SLC2A3"), "SYMBOL", "ENSEMBL", "org.Mmu.eg.db")
#' @param input vector of the IDs to convert
#' @param from input ID type
#' @param to output ID type or vector of output ID types
#' @param db annotation database
#' @import clusterProfiler
#' @return dataframe with the translated IDs
#' @export
conv_ids_cp <- function(input, from, to, db) {

    if (length(input) == 0 || is.na(input) || is.null(input)) {
        stop("The input is empty")
    }

    if (!requireNamespace(db, quietly = TRUE)) {
        stop(paste("Package", db, " needed for this function to work. Please install it."),
            call. = FALSE
        )
    }

    my_genes <- input

    tryCatch(
        {
            bitr(my_genes, fromType = from, toType = to, OrgDb = db)
        },
        error = function(msg) {
            message("Error while executing, exiting with NA output. Please, check the inputs.")
            return(NA)
        }
    )

    return(bitr(my_genes, fromType = from, toType = to, OrgDb = db))
}