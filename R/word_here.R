#' Detect words in a list of documents
#'
#' @description For a list of words imported with the “import_words” function or made with the ‘make_words” function, the “word_here” function searches these words in all given texts.
#'
#' @param text_list a vector containing all texts to analyse
#' @param word_list a list of words imported with the “import_words” function or made with the “make_words” function
#' @param table if TRUE, allows to return the result as a table instead of a list
#' @param group if TRUE, allows to group the “grouped words” into one result instead of giving a result to each one
#' @param count if TRUE, allows to count the searched words in each document instead of only indicate if it’s present or not
#'
#' @import tibble
#'
#' @return
#' @export
#'
#' @examples
word_here <- function(text_list, word_list, table = FALSE, group = TRUE, count = FALSE){
  result <- list()
  # "name" is initialized with the first word
  name <- word_list[[1]][[1]][1]
  # OPTION 1: have a result with grouped words
  if (group == TRUE){
    for (i in 1:length(word_list)){
      # obtain a "normal" word
      if (length(word_list[[i]][[1]]) == 1){
        mot <- word_list[[i]][1]
        # obtain a list of "grouped" word
      }else{
        for (j in 1:length(word_list[[i]])){
          if (j == 1){
            mot <- word_list[[i]][[1]][1]
          }else{
            mot <- paste0(mot, "|", word_list[[i]][[j]][1])
          }
        }
      }
      # the "normal" word or the list of "grouped" words are searched within the documents
      result[[i]] <- one_word_here(text_list, mot, count = count)
      # the columns of the result are named with the different words
      # (about the grouped words, only the first word of a list of grouped words is used)
      if (i != 1){
        name <- c(name, word_list[[i]][[1]][1])
      }
    }
    # OPTION 2: have a result without grouped words
  }else{
    counter <- 0
    for (i in 1:length(word_list)){
      # obtain a "normal" word
      if (length(word_list[[i]][[1]]) == 1){
        counter <- counter + 1
        mot <- word_list[[i]][1]
        # the word is searched within the documents
        result[[counter]] <- one_word_here(text_list, mot, count = count)
        # the columns of the result are named with the different words
        if (i != 1){
          name <- c(name, word_list[[i]][[1]][1])
        }
        # obtain each "grouped" word
      }else{
        for (j in 1:length(word_list[[i]])){
          counter <- counter + 1
          mot <- word_list[[i]][[j]][1]
          # the word is searched within the documents
          result[[counter]] <- one_word_here(text_list, mot, count = count)
          # the columns of the result are named with the different words
          if (i != 1){
            name <- c(name, word_list[[i]][[j]][1])
          }
        }
      }
    }
  }
  names(result) <- name
  if(table == TRUE){
    result <- as_tibble(result)
  }
  return(result)
}
