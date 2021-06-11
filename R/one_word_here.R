#' Detect a word in a list of documents
#'
#' @description For a given word, the function searches this word in all given texts.
#'
#' @param text_list a vector containing all texts to analyse
#' @param word the word we want to search in the texts
#' @param count if TRUE, allows to count the searched word in each document instead of only indicate if it’s present or not
#'
#' @import stringr
#'
#' @return
#' @export
#'
#' @examples
one_word_here <- function(text_list, word, count = FALSE){
  if(count == TRUE){
    test <- str_count(tolower(text_list), paste0("[^a-z0-9](", word, ")[^a-z0-9]"))
  }else{
    test <- str_detect(tolower(text_list), paste0("[^a-z0-9](", word, ")[^a-z0-9]"))
  }
  return(test)
}
