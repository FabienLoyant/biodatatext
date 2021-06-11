#' Words importation
#'
#' @description Import the wanted words written in a csv or pdf file build with a particular semantics.
#'
#' @param file the file pathway
#' @param simplify if TRUE, allows to directly apply the “easier_words” function (option)
#'
#' @import pdftools
#' @import stringr
#'
#' @return
#' @export
#'
#' @examples
import_words <- function(file, simplify = FALSE){
  if (grepl(".pdf", tolower(file)) == TRUE | grepl(".csv", tolower(file)) == TRUE){
    words_file <- NULL
    if (grepl(".pdf", tolower(file)) == TRUE){
      # file importation for pdf
      loading <- pdf_text(file)
      for (i in 1:length(loading)){
        words_file <- paste(words_file, loading[i], sep = "\r\n")
      }
    }else if(grepl(".csv", tolower(file)) == TRUE){
      # file importation for csv
      loading <- read.csv2(file, header = FALSE)
      for (i in 1:length(loading)){
        for (j in 1: length(loading[,i])){
          words_file <- paste(words_file, loading[j,i], sep = "\r\n")
        }
      }
    }
    # text transformation
    initial_name <- words_file
    words_file <- words_file %>%
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
    words_file <- str_extract_all(words_file[[1]], "[^\r\n]+")
    initial_name <- str_extract_all(initial_name[[1]], "[^\r\n]+")
    # categories
    category <- "nocat"
    # units
    myunit <- "nounit"
    avunits <- c("unit", "nbof", "score", "percent", "cat", "hmany", "bool")
    # x: index of the result list (words_list)
    # y: list used as a tool to manage the "<<<" semantics (group words)
    # z: index of the y list
    x <- 1
    y <- NULL
    z <- 1
    all_names <- NULL
    group_names <- NULL
    words_list <- list()
    for (i in 1:length(words_file[[1]])){
      # manage a category
      if (str_detect(words_file[[1]][i], "#.*#") == TRUE){
        if (i < length(words_file[[1]])){
          if (str_detect(words_file[[1]][i+1], "<<<") == FALSE){
            if (words_file[[1]][i] == "##"){
              category <- "nocat"
            }else{
              category <- str_extract(words_file[[1]][i], "[^#]+")
            }
          }else{
            stop("Semantics is incorrect, please check the contents of the file containing the words.")
          }
        }
        # manage a unit
      }else if(str_detect(words_file[[1]][i], "@.*@") == TRUE){
        if(str_extract(words_file[[1]][i], "[^@]+") %in% avunits | str_detect(words_file[[1]][i], "^@@$") == TRUE){
          if (i < length(words_file[[1]])){
            if (str_detect(words_file[[1]][i+1], "<<<") == FALSE){
              if (words_file[[1]][i] == "@@"){
                myunit <- "nounit"
              }else{
                myunit <- str_extract_all(words_file[[1]][i], "[^@]+")
                myunit <- myunit[[1]]
              }
            }else{
              stop("Semantics is incorrect, please check the contents of the file containing the words.")
            }
          }
        }else{
          stop(paste("An unknown \"unit type\" has been detected, please check the contents of the file containing the words.",
                     "Reminder of available \"unit types\": unit, nbof, score, percent, cat, hmany, bool"))
        }
        # manage a word and associate it to his category
      }else{
        # precision allows to know if we want the word exactly as written (specific) or not (global)
        precision <- "specific"
        if(str_detect(words_file[[1]][i], "^_.*$") == TRUE){
          if (i < length(words_file[[1]])){
            if (str_detect(words_file[[1]][i+1], "<<<") == FALSE){
              precision <- "global"
            }else{
              stop("Semantics is incorrect, please check the contents of the file containing the words.")
            }
          }
        }
        # manage a "normal" word
        if (str_detect(words_file[[1]][i], "<<<") == FALSE){
          if (is.null(y)){
            words_list[[x]] <- list(str_extract(words_file[[1]][i], "[^_].+"), category)
            names(words_list[[x]]) <- c("word", "category")
            all_names <- c(all_names, initial_name[[1]][i])
            if (myunit[1] %in% avunits){
              words_list[[x]]$unit <- myunit
            }else{
              words_list[[x]]$unit <- "nounit"
            }
            words_list[[x]]$precision <- precision
            x <- x + 1
          }else{
            words_list[[x]] <- y
            words_list[[x+1]] <- list(str_extract(words_file[[1]][i], "[^_].+"), category)
            names(words_list[[x+1]]) <- c("word", "category")
            all_names <- c(all_names, initial_name[[1]][i])
            names(words_list[[x]]) <- group_names
            group_names <- NULL
            if (myunit[1] %in% avunits){
              words_list[[x+1]]$unit <- myunit
            }else{
              words_list[[x+1]]$unit <- "nounit"
            }
            words_list[[x+1]]$precision <- precision
            y <- NULL
            z <- 1
            x <- x + 2
          }
          # manage a word to group (with the "<<<" semantics)
          #   -if a word have the "<<<" semantics, it has to be grouped with the last "normal" word
        }else if (i > 1){
          z <- z + 1
          if (str_detect(words_file[[1]][i-1], "<<<") == FALSE){
            y <- words_list[x-1]
            y[[z]] <- list(str_extract(words_file[[1]][i], "[^<].+"))
            names(y[[z]]) <- "word"
            y[[z]]$category <- category
            group_names <- c(group_names, initial_name[[1]][i-1], str_extract(initial_name[[1]][i], "[^<].+"))
            if (myunit[1] %in% avunits){
              y[[z]]$unit <- myunit
            }else{
              y[[z]]$unit <- "nounit"
            }
            y[[z]]$precision <- precision
            words_list[[x-1]] <- y
            x <- x - 1
          }else{
            y[[z]] <- list(str_extract(words_file[[1]][i], "[^<].+"))
            names(y[[z]]) <- "word"
            y[[z]]$category <- category
            group_names <- c(group_names, str_extract(initial_name[[1]][i], "[^<].+"))
            if (myunit[1] %in% avunits){
              y[[z]]$unit <- myunit
            }else{
              y[[z]]$unit <- "nounit"
            }
            y[[z]]$precision <- precision
            words_list[[x]] <- y
          }
        }else{
          stop("Semantics is incorrect, please check the contents of the PDF file containing the words.")
        }
      }
    }
  }else{
    stop("You must use PDF format.")
  }
  names(words_list) <- str_extract(all_names, "[^_].+")
  if(simplify == TRUE){
    words_list <- easier_words(words_list)
  }
  return(words_list)
}
