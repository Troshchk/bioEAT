#' @title GO analysis and plots for BP, CC, MF
#' @description
#' This function returns table for GO results for BP, CC, MF with gene symbols.
#' The function may also create excel with the GO result tables and the PDFs with the barplots of the results.
#' @examples
#' groupGO_all_groups(entrez_ids_list, "org.Mmu.eg.db", "out_name", mf = 1, xls = FALSE, pdf = FALSE)
#' @param gene_entrezIDs vector of entrezIDs
#' @param orgDB db to use for GO
#' @param name name of the output file
#' @param bp level of the biological process with default 2
#' @param mf level of the molecular function with default 3
#' @param cc level of the cellular component with default 3
#' @param xls logical parameter to create excel with with default TRUE
#' @param pdf logical parameter to create pdf with with default TRUE
#' @return table with groupGO result arranged as list of dataframes
#' @return excel file with groupGO result and the pdf with the groupGO result barplot
#' per biological process, cellular component and molecular function
#' @import clusterProfiler
#' @import openxlsx
#' @export
groupGO_all_groups <- function(gene_entrezIDs, orgDB, name = "", bp = 2, cc = 3, mf = 3, xls = TRUE, pdf = TRUE) {
    
    if (length(gene_entrezIDs) == 0 || is.na(gene_entrezIDs) || is.null(gene_entrezIDs)) {
        stop("The input gene names input is empty")
    }

    if (!requireNamespace(orgDB, quietly = TRUE)) {
        stop(paste("Package", orgDB, " needed for this function to work. Please install it."),
            call. = FALSE
        )
    }

    tryCatch(
        {
            ggo2 <- groupGO(
                gene = gene_entrezIDs,
                OrgDb = orgDB,
                ont = "BP",
                level = 2,
                readable = TRUE
            )
        },
        error = function(msg) {
            stop("Error while executing, exiting with NA output. Please, check the inputs. \n Tip: are the inputs ENTREZ IDs?")
            return(NA)
        }
    )

    out_list <- list()

    for (i in c("BP", "CC", "MF")) {
        if (i == "BP") {
            level <- bp
        } else if (i == "CC") {
            level <- cc
        } else {
            level <- mf
        }


        ggo2 <- groupGO(
            gene = gene_entrezIDs,
            OrgDb = orgDB,
            ont = i,
            level = level,
            readable = TRUE
        )


        out_list[[i]] <- as.data.frame(ggo2)

        if (xls == TRUE) {
            write.xlsx(as.data.frame(ggo2), paste(name, i, ".xlsx", sep = ""), row.names = FALSE)
        }

        if (pdf == TRUE) {
            pdf(paste(name, i, ".pdf", sep = ""))
            print(barplot(ggo2, main = ""))
            dev.off()
        }
    }
    return(out_list)
}