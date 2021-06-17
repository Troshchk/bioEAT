#' @title Getting the main n PCA components
#' @description
#' This function returns a vector of top n contributors
#' @examples
#' get_main_pca(df, 3)
#' @param df dataframe with normalized counts (column = sample, row = transcript)
#' @param n number of top contributors
#' @import factoextra
#' @return vector of top n contributors
#' @export
get_main_pca <- function(df, n) {
    
    if (length(df) == 0 || is.na(df) || is.null(df)) {
        stop("The input dataframe is empty")
    }

    formalNames <- names(formals())
    if (!is.numeric(n) || do.call(missing, list(formalNames[2])) || length(n) == 0 || is.na(n) || is.null(n)) {
        stop("The input number of top contributors is empty or not numeric")
    }

    if (identical(n %% 1 == 0 & n > 0, FALSE)) {
        stop("The input number of top contributors is not integer or < 0")
    }

    res.pca <- prcomp(t(df))
    res.pca_df <- get_pca(res.pca)$contrib
    res.pca_df <- as.data.frame(res.pca_df)
    res.pca_df_names <- rownames(head(res.pca_df[order(-res.pca_df$"Dim.1"), ], n = n))
    return(res.pca_df_names)
}