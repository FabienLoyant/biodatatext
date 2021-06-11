#' Number of documents per patient counting
#'
#' @description Count the number of documents (texts) per patient and return a table or a plot representing the result. This works with a data table previously imported with the “import_data” function.
#'
#' @param data the name of the data table
#' @param plot if TRUE, allows to return the result as an histogram
#'
#' @import ggplot2
#' @import tibble
#' @import dplyr
#'
#' @return
#' @export
#'
#' @examples
check_text <- function(data, plot = FALSE){
  # number of documents per patient counting
  counting <- as_tibble(data$ID_patient) %>%
    count(data$ID_patient, sort = TRUE)
  names(counting) <- c("ID_patient", "num")
  if (plot == TRUE){
    # return an histogram representing the counting
    return(ggplot(counting, aes(reorder(ID_patient, num), num)) +
             geom_bar(stat = "identity", fill = "#97BADA") +
             geom_text(aes(label= as.character(num)), check_overlap = TRUE, size = 4) +
             coord_flip() +
             xlab("ID_patient") +
             ylab("Number of documents") +
             labs(title = "Number of documents per patient") +
             theme(plot.title=element_text(margin=margin(0,0,10,0), size=15, hjust = 0.5),
                   panel.background = element_rect(fill = "white"),
                   panel.grid.major = element_line(colour = "grey"),
                   plot.margin = margin(20,20,20,20)))
  }else{
    # return the counting table
    return(counting)
  }
}
