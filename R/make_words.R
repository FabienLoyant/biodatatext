#' Build a words list
#'
#' @param mywords a vector in which we can write the wanted words
#' @param precision “specific” or “global” (put “specific” or “global” for the precision mode of each word. It’s then used as a tool to know if we do a fuzzy analysis on the word)
#'
#' @import stringr
#'
#' @return
#' @export
#'
#' @examples
make_words <- function(mywords, precision = "specific"){
  if(is.vector(mywords) == TRUE){
    result <- list()
    name1 <- NULL
    name2 <- c("word", "category", "unit", "precision")
    for(i in 1:length(mywords)){
      result[[i]] <- list()
      result[[i]][[1]] <- mywords[i] %>%
        tolower() %>%
        str_replace_all("[éèê]", "e") %>%
        str_replace_all("ô", "o") %>%
        str_replace_all("ç", "c") %>%
        str_replace_all("ù", "u") %>%
        str_replace_all("[îï]", "i") %>%
        str_replace_all("[+]", "[+]") %>%
        str_replace_all("[*]", "[*]") %>%
        str_replace_all("[{]", "[{]") %>%
        str_replace_all("[}]", "[}]") %>%
        str_replace_all("[(]", "[(]") %>%
        str_replace_all("[)]", "[)]")
      result[[i]][[2]] <- "nocat"
      result[[i]][[3]] <- "nounit"
      if(precision == "global"){
        result[[i]][[4]] <- "global"
      }else{
        result[[i]][[4]] <- "specific"
      }
      names(result[[i]]) <- name2
      name1 <- c(name1, mywords[i])
    }
    names(result) <- name1
    return(result)
  }else{
    stop("Argument is not correct. It must be a vector containing wanted words")
  }
}
