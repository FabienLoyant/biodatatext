#' Number of patients concerned about searched words
#'
#' @description Count for each word how many patients have at least one document containing this word. The words must be as a list previously imported with the ‘import_words” function or made with the “make_words” function.
#'
#' @param data table of data imported with the “import_data” function
#' @param word_list a list of words imported with the “import_words” function or made with the “make_words” function
#' @param rank if TRUE, allows to return all results in descending order
#' @param plot if TRUE, allows to return results as an histogram
#'
#' @import ggplot2
#' @import dplyr
#' @import tibble
#'
#' @return
#' @export
#'
#' @examples
word_hm_p <- function(data, word_list, rank = FALSE, plot = FALSE){
  per_patient <- word_hm_dp(data, word_list)
  result <- tibble()
  # filling the result table with the words
  for (i in 1:length(word_list)){
    result[i,1] <- names(word_list)[i]
    counter <- i
  }
  # initialization of each word with a 0 value
  result[,2] <- 0
  # the value of each word is incremented by 1 if a patient is concerned by the searched word
  for (i in 1:counter){
    for (j in 1:length(per_patient)){
      if (per_patient[[j]]$n_docs[i] != 0){
        result[i,2] <- result[i,2] + 1
      }
    }
  }
  names(result) <- c("word", "n_patients")
  if (rank == TRUE){
    result <- arrange(result, desc(n_patients))
  }
  # result as an histogram
  if(plot == TRUE){
    return(ggplot(result, aes(reorder(word, n_patients), n_patients)) +
             geom_bar(stat = "identity", fill = "#97BADA") +
             geom_text(aes(label= as.character(n_patients)), check_overlap = TRUE, size = 4) +
             coord_flip() +
             xlab("word") +
             ylab("Number of patients") +
             labs(title = "Number of patients associated to each word") +
             theme(plot.title=element_text(margin=margin(0,0,10,0), size=15, hjust = 0.5),
                   panel.background = element_rect(fill = "white"),
                   panel.grid.major = element_line(colour = "grey"),
                   plot.margin = margin(20,20,20,20)))
    # result as a table (tibble)
  }else{
    return(result)
  }
}
