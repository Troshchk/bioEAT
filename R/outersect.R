#' @title Outersect
#'@description
#' This function returns the non-overlapping values from the two input vectors.
#' @details 
#' It checks for values from the first inout not appearing in the second input
#' and for the values from the second inout not appearing in the first input.
#' @examples
#' outersect(df_1[,2],df_2[,2])
#' outersect(1:5, 4:7)
#' @param x vector 
#' @param y vector
#' @return sorted vector of non-overlapping values from lists x and y 
#' @export
outersect <- function(x, y) {
    
	out_sect <- sort(c(setdiff(x, y), setdiff(y, x)))
    if (length(out_sect)==0){
	            out_sect <-  NULL
        } 
        return(out_sect)
}
