#' Number of documents having searched words
#'
#' @description Count for each word how many documents have this word. The words must be as a list previously imported with the ‘import_words” function or made with the “make_words” function.
#'
#' @param text_list a vector containing all texts to analyse
#' @param word_list a list of words imported with the “import_words” function or made with the “make_words” function
#' @param rank if TRUE, allows to return all results in descending order
#' @param count if TRUE, allows to count the searched words in each document instead of only indicate if it’s present or not
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
word_hm <- function(text_list, word_list, rank = FALSE, count = FALSE, plot = FALSE){
  analysis <- word_here(text_list, word_list, count = count)
  result <- tibble()
  # each word is associated with the number of "TRUE" values in the result of the "word_here" function
  # (or with the sum of the values if we go for "count = TRUE")
  for (i in 1:length(word_list)){
    result[i,1] <- names(word_list)[i]
    result[i,2] <- sum(analysis[[i]])
  }
  if(count == TRUE){
    names(result) <- c("word", "n")
  }else{
    names(result) <- c("word", "n_docs")
  }
  if (rank == TRUE){
    if (count == TRUE){
      result <- arrange(result, desc(n))
    }else{
      result <- arrange(result, desc(n_docs))
    }
  }
  # result as an histogram
  if(plot == TRUE){
    if(count == TRUE){
      return(ggplot(result, aes(reorder(word, n), n)) +
               geom_bar(stat = "identity", fill = "#97BADA") +
               geom_text(aes(label= as.character(n)), check_overlap = TRUE, size = 4) +
               coord_flip() +
               xlab("word") +
               ylab("Number of words") +
               labs(title = "Total number of each word") +
               theme(plot.title=element_text(margin=margin(0,0,10,0), size=15, hjust = 0.5),
                     panel.background = element_rect(fill = "white"),
                     panel.grid.major = element_line(colour = "grey"),
                     plot.margin = margin(20,20,20,20)))
    }else{
      return(ggplot(result, aes(reorder(word, n_docs), n_docs)) +
               geom_bar(stat = "identity", fill = "#97BADA") +
               geom_text(aes(label= as.character(n_docs)), check_overlap = TRUE, size = 4) +
               coord_flip() +
               xlab("word") +
               ylab("Number of documents") +
               labs(title = "Number of documents associated to each word") +
               theme(plot.title=element_text(margin=margin(0,0,10,0), size=15, hjust = 0.5),
                     panel.background = element_rect(fill = "white"),
                     panel.grid.major = element_line(colour = "grey"),
                     plot.margin = margin(20,20,20,20)))
    }
    # result as a table (tibble)
  }else{
    return(result)
  }
}
