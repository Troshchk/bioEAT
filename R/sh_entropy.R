#' @title Calculating Shannon entropy of each sample, Shannnon entropy
#' values and transcript frequency visualization
#' @description
#' This function returns a list, containing dataframe with the entropy
#' values per sample,
#' entropy plot and transcript frequency plot
#' @details
#' This function takes a dataframe with transcripts in rows and samples in
#' columns as an input and calculates the Shannon entropy for each sample
#' and the frequency of the transcript in the sample. Further it visualizes
#' the entropy levels and the frequencies. The transcripts are reordered by
#' their frequencies.
#' Function can also print the plots to pdf if pdf parameter is TRUE.
#' @examples
#' sh_entropy(df, "name", relevel = TRUE, levels = c(seq(1, ncol(df), 1)))
#' sh_entropy(df)
#' sh_entropy(df, pdf = FALSE)
#' @param df input dataframe with transcripts in rows and samples in columns
#' @param name name of the output pdf with defult "Shannon entropy", used when pdf is TRUE
#' @param pdf logical parameter to create pdf with with default TRUE
#' @param relevel logical parameter to reorder the samples order in the plots with default FALSE
#' @param levels vector with the correct order of the samples with default NULL; will be used only if relevel is TRUE 
#' @import plyr
#' @import reshape2
#' @import ggplot2
#' @return list with the entropy levels and plots
#' @export
sh_entropy <- function(df, name = "Shannon entropy", pdf = TRUE, relevel = FALSE, levels = NULL) {

    if (length(df) == 0 || is.na(df) || is.null(df)) {
        stop("The input dataframe is empty")
    }

    for (i in 1:nrow(df)) {
        if (!is.double(df[, i])) {
            stop("The input dataframe contains non-numeric data")
        }
    }

    if (relevel == TRUE & ncol(df) != length(unique(levels))) {
        stop("The levels vector contains less unique elements than the  amount of columns in the input dataframe")
    }

    # Making sure the samples will not be releveled
    if (relevel == FALSE){
        levels <- NULL
    }

    out_list <- list()

    # make a frequency dataframe from counts
    df_freq <- apply(df, 2, function(x) {
        x / sum(x)
    })

    # calculate entropy value per sample
    total_entropy <- -colSums(apply(df_freq, 2, function(x) {
        x * log(x)
    }), na.rm = TRUE)
    df_total_entropy <- setNames(ldply(total_entropy, data.frame), c("sample", "df_total_entropy"))
    df_total_entropy <- transform(df_total_entropy,
        sample = as.factor(sample)
    )



    # create long dataframe with transcript frequencies
    df_ent_melt <- setNames(melt(as.matrix(df_freq), na.rm = FALSE), c("Transcript", "Sample", "Value"))

    # assign column data types
    df_ent_melt <- transform(df_ent_melt,
        Transcript = as.factor(Transcript),
        Sample = as.factor(Sample),
        Value = as.numeric(Value)
    )

    # Top entropy plot
    # relevel factor to display in the correct order, if TRUE
    p1 <- ggplot(
        data = df_total_entropy,
        aes(x = forcats::fct_relevel(sample, as.character(levels)), y = df_total_entropy)) +
        geom_bar(stat = "identity", fill = "#7570B3") +
        theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
        labs(y = "Entropy", x = "") +
        theme(legend.position = "none")

    # Create color pallete
    values <- rep(rainbow(32), times = 400)
    values <- sample(values)

    # Bottom entropy plot
    # no legend
    # transcripts ordered from highest to lowest values
    p2 <- ggplot(
        df_ent_melt,
        aes(fill = reorder(Transcript, Value), y = Value, x = Sample)) +
        geom_bar(position = "fill", stat = "identity") +
        theme(legend.position = "none") +
        scale_fill_manual(values = values) +
        theme(axis.text.x = element_text(angle = 90)) +
        labs(y = "Percentage of transcript", x = "")


    out_list[["Entropy_values"]] <- df_total_entropy
    out_list[["top"]] <- p1
    out_list[["bottom"]] <- p2

    return(out_list)
    # Print to pdf
    if (pdf == TRUE) {
        pdf(paste(name, ".pdf", sep = ""))
        par(mfrow = c(1, 2))

        print(p1)
        print(p2)

        dev.off()
    }
}