#' Detection of documents containing searched words
#'
#' @description Allows for each word to give the list of documents containing this word. This is done from the data table imported with the “import_data” function and from the words list imported with the “import_words” function or made with the “make_words” function.
#'
#' @param data table of data imported with the “import_data” function
#' @param word_list a list of words imported with the “import_words” function or made with the “make_words” function
#' @param group if TRUE, allows to group the “grouped words” into one result instead of giving a result to each one
#' @param IDt if TRUE, allows to give results with the text ID instead of the text row numbers
#'
#' @return
#' @export
#'
#' @examples
where_is_word <- function(data, word_list, group = TRUE, IDt = FALSE){
  word_here_tab <- word_here(data$TEXTE, word_list, table = TRUE, group)
  if (IDt == TRUE){
    word_here_tab$doc <- data$ID_TEXTE
  }else{
    word_here_tab$doc <- 1:length(data$ID_patient)
  }
  result <- list()
  if (length(word_list[[1]][[1]]) == 1){
    name <- names(word_list)[1]
  }else{
    name <- names(word_list[[1]])[1]
  }
  # OPTION 1: have a result with all "grouped" words alone
  if (group == FALSE){
    counter <- 0
    for (i in 1:length(word_list)){
      # obtain the result for a "normal" word
      if (length(word_list[[i]][[1]]) == 1){
        counter <- counter + 1
        result[[counter]] <- word_here_tab$doc[word_here_tab[,counter] == TRUE]
        if (i != 1){
          name <- c(name, names(word_list)[i])
        }
        # obtain the result for a "grouped" word
      }else{
        for (j in 1:length(word_list[[i]])){
          counter <- counter + 1
          result[[counter]] <- word_here_tab$doc[word_here_tab[,counter] == TRUE]
          if (i != 1){
            name <- c(name, names(word_list[[i]])[j])
          }
        }
      }
    }
    names(result) <- name
    return(result)
    # OPTION 2: have a result with all "grouped" words grouped
  }else{
    for (i in 1:length(word_list)){
      result[[i]] <- word_here_tab$doc[word_here_tab[,i] == TRUE]
      if (i != 1){
        name <- c(name, names(word_list)[i])
      }
    }
    names(result) <- name
    return(result)
  }
}
