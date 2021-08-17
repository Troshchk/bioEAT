#' @title Converting gene IDS using maRt
#' @description
#' This function returns a dataframe with the translated IDs.
#' #'@details
#' This function takes a vector of IDs and translated them from 
#' the input format to the output format using the selected maRt. 
#' @examples
#' conv_ids_mart(c("CDY2A","IGHM", "IGKC"), "mmulatta_gene_ensembl", "external_gene_name", 
#' c("entrezgene_id", "ensembl_gene_id"))
#' @param input vector of IDs
#' @param mart annotation maRt
#' @param from input ID type
#' @param to output ID type or vector of output ID types
#' @return dataframe with translated IDs
#' @importFrom biomaRt getBM
#' @importFrom biomaRt useMart
#' @importFrom biomaRt useDataset
#' @export
conv_ids_mart <- function(input, mart, from, to) {
    
    if (length(input) == 0 || any(is.na(input)) || is.null(input)) {
        stop("The input is empty")
    }

    ensembl <- useMart("ensembl")
    ensembl_used <- useDataset(mart, mart = ensembl)


        tryCatch(
        {
        getBM(
        attributes = to,
        filters = from,
        values = input,
        mart = ensembl_used
    )
        },
        error = function(msg) {
            message("Error while executing, exiting with NA output. Please, check the inputs.")
            return(NA)
        }
    )


    return(getBM(
        attributes = to,
        filters = from,
        values = input,
        mart = ensembl_used
    ))
}