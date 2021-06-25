#' @title Converting gene IDS using orgDB and maRt
#' @description
#' This function returns a dataframe with translated IDs and NA row for the not anootated genes.
#'@details
#' This function takes a vector of IDs and translated them from 
#' the input format to the output format using the selected orgDB. 
#' The IDs not found by OrgDB are further annotated by maRt.
#' If the ID was not found it will be at the end of the output dataframe with NAs.
#' This function works only with the IDs: ENSEMBL ID, SYMBOL, ENTREZID
#' In case the orgDB is not installed before launching the function,
#' the function will exit with error.
#' @examples
#' conv_ids_full(rownames(dataNorm_df), "org.Mmu.eg.db", "ENSEMBL",
#' "mmulatta_gene_ensembl", "ensembl_gene_id")
#' conv_ids_full(c("IFNA13", "SLC2A3", "CD45RA", "CDY2A", "IGHM", "IGKC"), "org.Mmu.eg.db",  
#' "SYMBOL", "mmulatta_gene_ensembl", "external_gene_name")
#' @param input vector of IDs
#' @param db_cluster_profiler annotation clusterProfiler database
#' @param from_cluster_profiler input clusterProfiler ID type: ENSEMBL, ENTREZID, SYMBOL
#' @param mart annotation mart
#' @param from_mart input maRt ID type: ensembl_gene_id, external_gene_name, entrezgene_id
#' @import clusterProfiler
#' @import biomaRt
#' @return dataframe with the ENSEMBL ID, SYMBOL, ENTREZID, GENENAME(description); NAs are not dropped
#' @export
#' 
conv_ids_full <- function(input, db_cluster_profiler, from_cluster_profiler, mart, from_mart) {
    ann1 <- NULL # output dataframe from clusterProfiler
    ann2 <- NULL # output dataframe from maRtP
    ann3 <- NULL # output merged dataframe from clusterProfiler and maRt; possibly with not translated IDs
    not_annot <- NULL # vector of genes not found by clusterProfiler
    not_found <- NULL # vector of genes not found by clusterProfiler or by maRt
    column_to_compare <- from_cluster_profiler # renamed for the code visibility

    # Searching genes with clusterProfiler; genes not annotated by clusterProfiler are written to not_annot variable


    if (length(input) == 0 || is.na(input) || is.null(input)) {
        stop("The input is empty")
    }

    calculate_cp <- FALSE

    tryCatch(
        {
            conv_ids_cp(input, from_cluster_profiler, c("SYMBOL", "ENTREZID", "GENENAME", "ENSEMBL"), db_cluster_profiler)
        },
        error = function(msg) { 
            calculate_cp <- TRUE
        }
    )
  
    if(calculate_cp) { 
        message("None of IDs found using clusterProfiler.")
        ann1 <- NA
    } else {
        ann1 <- conv_ids_cp(input, from_cluster_profiler, c("SYMBOL", "ENTREZID", "GENENAME", "ENSEMBL"), db_cluster_profiler)
        ann1 <- ann1[c("ENSEMBL", "SYMBOL", "ENTREZID", "GENENAME")]
    }

    if (is.na(ann1)) {
        ann1 <- NULL
        not_annot <- input
    } else {
        not_annot <- outersect(ann1[, column_to_compare], input)
    }

    # If some genes were not annotated by clusterProfiler, they are searched using maRt
    if (is.null(not_annot)) {
        return(ann1)
    } else {
        ann2 <- conv_ids_mart(not_annot, mart, from_mart, c("ensembl_gene_id", "external_gene_name", "entrezgene_id", "description"))
        if (is.null(ann2)) {
            return(ann1)
        } else {
            ann2 <- ann2[c("ensembl_gene_id", "external_gene_name", "entrezgene_id", "description")]
            colnames(ann2) <- c("ENSEMBL", "SYMBOL", "ENTREZID", "GENENAME")
            ann3 <- rbind(ann1, ann2)
        }
    }


    # Adding to the output dataframe genes which are not found in any of previous steps

    not_found <- outersect(ann3[, column_to_compare], input)

    if (is.null(not_found)) {
        return(ann3)
    } else {
        for (i in not_found) {
            if (from_cluster_profiler == "ENSEMBL"){
                        ann3 <- rbind(ann3, c(i, rep(NA, each = ncol(ann3) - 1)))
            } else if (from_cluster_profiler == "SYMBOL") {
                        ann3 <- rbind(ann3, c(NA, i, rep(NA, each = ncol(ann3) - 2)))
            } else if (from_cluster_profiler == "ENTREZID") {
                        ann3 <- rbind(ann3, c(NA, NA, i, NA))
            }
        }
        colnames(ann3) <- c("ENSEMBL", "SYMBOL", "ENTREZID", "GENENAME")
    }
    return(ann3)
}