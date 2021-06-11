#' Simplify a words list
#'
#' @description Simplify a words list imported with the “import_words” function. The simplification concerns the “grouped words” which are put into an only word instead of being a list of several grouped words which have a difficult structure composed of nested lists.
#' The function can also allow to collect fuzzy words.
#'
#' @param word_list the list of searched words imported with the “import_words” function
#' @param data data imported with the “import_data” function (option allowing to collect fuzzy words)
#' @param stopword if TRUE, allows to delete stopwords for the fuzzy words research (option)
#'
#' @import dplyr
#' @import stringdist
#' @import tibble
#' @import tidytext
#'
#' @return
#' @export
#'
#' @examples
easier_words <- function(word_list, data = NULL, stopword = FALSE){
  if (is.data.frame(data) == TRUE){
    text <- tibble(text = data$TEXTE)
    if (stopword == TRUE){
      my_stopwords <- tibble(word = stopwords("fr"))
      text <- text %>%
        unnest_tokens(word, text) %>%
        anti_join(my_stopwords, by = "word") %>%
        count(word, sort = TRUE)
    }else{
      text <- text %>%
        unnest_tokens(word, text) %>%
        count(word, sort = TRUE)
    }
    for (i in 1:length(word_list)){
      if (length(word_list[[i]][[1]]) == 1){
        if (word_list[[i]][[4]] != "global"){
          fuzzywords <- NULL
          for (a in 1:length(text$word)){
            if(stringdist(word_list[[i]][[1]], text$word[a])< 2 & text$word[a] != word_list[[i]][[1]]){
              fuzzywords <- paste0(fuzzywords, "|(", text$word[a], ")")
            }
          }
          word_list[[i]][[1]] <- paste0("(", word_list[[i]][[1]], ")", fuzzywords)
        }
      }else{
        for (j in 2:length(word_list[[i]])){
          if (word_list[[i]][[j]][[1]] != "global"){
            fuzzywords <- NULL
            for (a in 1:length(text$word)){
              if(stringdist(word_list[[i]][[j]][[1]], text$word[a])< 2 & text$word[a] != word_list[[i]][[j]][[1]]){
                fuzzywords <- paste0(fuzzywords, "|(", text$word[a], ")")
              }
            }
            word_list[[i]][[j]][[1]] <- paste0("(", word_list[[i]][[j]][[1]], ")", fuzzywords)
          }
        }
      }
    }
  }
  result <- list()
  for (i in 1:length(word_list)){
    if (length(word_list[[i]][[1]]) == 1){
      result[[i]] <- word_list[[i]]
    }else{
      result[[i]] <- word_list[[i]][[1]]
      result[[i]][[1]] <- paste0("(", word_list[[i]][[1]][[1]], ")")
      for (j in 2:length(word_list[[i]])){
        result[[i]][[1]] <- paste0(result[[i]][[1]], "|(", word_list[[i]][[j]][[1]], ")")
      }
    }
    result[[i]][[1]] <- paste0("(", result[[i]][[1]], ")")
  }
  names(result) <- names(word_list)
  return(result)
}
