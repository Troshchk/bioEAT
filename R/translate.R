#' @title Finding gene homologs
#' @description
#' This function returns a dataframe with homolog IDs and NA row for the genes with no homologs.
#' @details
#' This function takes a vector of IDs. If the IDs are not ENTREZ IDs,
#' they are converted to the ENTREZ ID format. Next, the homologs of the IDs are found using
#' the homologeneFile. The IDs which were not coverted  and the IDs without homologs are returned.
#' @examples
#' translate(rownames(dataNorm_df), 9606, homologeneFile = file)
#' translate(rownames(dataNorm_df), 9606,
#'     from_id = "ENSEMBL", homologeneFile = file,
#'     db_cluster_profiler = "org.Mmu.eg.db", mart = "mmulatta_gene_ensembl",
#'     from_mart = "ensembl_gene_id"
#' )
#' @param input vector of IDs
#' @param taxid taxonomy ID of the target species -- https://www.ncbi.nlm.nih.gov/Taxonomy/Browser
#' @param from_id input ID type with default "ENTREZID", other possible: "ENSEMBL", "SYMBOL"
#' @param homologeneFile homology table -- https://ftp.ncbi.nih.gov/pub/HomoloGene/current/homologene.data
#' @param db_cluster_profiler annotation clusterProfiler database
#' @param mart annotation martL
#' @param from_mart input maRt ID type: ensembl_gene_id, external_gene_name, entrezgene_id
#' @import clusterProfiler
#' @import biomaRt
#' @import annotationTools
#' @return dataframe with the ENSEMBL ID, SYMBOL, ENTREZID, GENENAME(description); NAs are not dropped
#' @export
translate <- function(input, taxid, from_id = "ENTREZID", homologeneFile, db_cluster_profiler = NULL, from_mart = NULL, mart = NULL) {
    myGenes_ent <- NULL #  dataframe with converted input IDs from input type to ENSEMBL ID, SYMBOL, ENTREZID, GENENAME(description)
    myGenes_translated <- NULL # dataframe with input IDs, input ENTREZIDs and the output ENTREZIDs

    # test whether input for type conversion is svailable
    if ((from_id != "ENTREZID") & (is.null(db_cluster_profiler) | is.null(mart) | is.null(from_mart))) {
        stop("The input type is not ENTREZID and one or more necessary inputs for the function are not supplied:
        db_cluster_profiler, mart, from_mart")
    }

    # Checking whether the input type is correct
    if (!(from_id %in% c("ENSEMBL", "SYMBOL", "ENTREZID"))) {
        stop("The input ID tyoe is not suitable for this function")
    }

    if (from_id == "ENTREZID") {

        # finding homologs and connecting to the homolog ENTREZids
        myGenes_translated <- cbind(input, getHOMOLOG(input, taxid, homologeneFile))
        myGenes_translated <- as.data.frame(myGenes_translated)
        myGenes_translated[, 1] <- as.character(myGenes_translated[, 1])
        myGenes_translated[, 2] <- as.character(myGenes_translated[, 2])

        # setting colnames to beautify
        colnames(myGenes_translated) <- c("ENTREZID_input", "ENTREZID_output")
    } else {

        # translating my type of identificators to entrezID to proceed with finding homologs
        myGenes_ent <- conv_ids_full(input, db_cluster_profiler, from_id, mart, from_mart)

        # finding homologs and connecting to the homolog ENTREZids
        myGenes_translated <- cbind(myGenes_ent$ENTREZID, getHOMOLOG(myGenes_ent$ENTREZID, taxid, homologeneFile))
        myGenes_translated <- as.data.frame(myGenes_translated)
        myGenes_translated$V1 <- as.character(myGenes_translated$V1)
        myGenes_translated$V2 <- as.character(myGenes_translated$V2)

        myGenes_translated <- merge(myGenes_ent[c(from_id, "ENTREZID")], myGenes_translated, by.x = "ENTREZID", by.y = "V1")


        # setting colnames to beautify
        colnames(myGenes_translated) <- c("ENTREZID_input", paste(from_id, "_input"), "ENTREZID_output")
    }

    return(myGenes_translated)
}