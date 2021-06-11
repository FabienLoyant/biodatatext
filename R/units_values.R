#' Extraction of values of interest
#'
#' @description Allows to extract values of interest into the texts. The research is done with words having an indicated “unit type”. This is done from the data table imported with the “import_data” function and from the words list imported with the “import_words” function or made with the “make_words” function. The result is a list of words containing a list of values (integer).
#'
#' @param data table of data imported with the “import_data” function
#' @param word_list a list of words imported with the “import_words” function or made with the “make_words” function
#' @param IDt if TRUE, allows to give results with the text ID instead of the text row numbers
#'
#' @import stringr
#'
#' @return
#' @export
#'
#' @examples
units_values <- function(data, word_list, IDt = FALSE){
  word_list <- easier_words(word_list)
  result <- units_text(data, word_list, test = "extract_char", IDt = IDt)
  lnumbers <- nombres(regex = FALSE, simplify = TRUE)
  lnumbersiemes <- nombriemes(regex = FALSE, simplify = TRUE)
  lnumbersR <- nombres(regex = TRUE, simplify = TRUE)
  lnumbersiemesR <- nombriemes(regex = TRUE, simplify = TRUE)
  for(i in 1:length(result)){
    # search a unit
    if(str_detect(names(result)[i], "[[:blank:]][(](unit)[)]") == TRUE){
      for (k in 1:length(result[[i]])){
        for (j in 1:length(result[[i]][[k]])){
          if (str_detect(result[[i]][[k]][[j]], "character[(]0[)]") == FALSE){
            if (str_detect(result[[i]][[k]][[j]], "[:digit:]+[:blank:]{0,1}[:digit:]*[:blank:]{0,1}[,.]{0,1}[:blank:]{0,1}[:digit:]*") == TRUE){
              result[[i]][[k]][[j]] <- str_extract(result[[i]][[k]][[j]], "[:digit:]+[:blank:]{0,1}[:digit:]*[:blank:]{0,1}[,.]{0,1}[:blank:]{0,1}[:digit:]*")
              result[[i]][[k]][[j]] <- result[[i]][[k]][[j]] %>%
                str_replace_all("[,.]", ".") %>%
                str_replace_all("[:blank:]", "") %>%
                as.numeric()
            }else if (str_detect(result[[i]][[k]][[j]], lnumbersR) == TRUE){
              result[[i]][[k]][[j]] <- which(lnumbers == str_extract(result[[i]][[k]][[j]], lnumbers))-1
            }else if(str_detect(result[[i]][[k]][[j]], lnumbersiemesR) == TRUE){
              result[[i]][[k]][[j]] <- which(lnumbersiemes == str_extract(result[[i]][[k]][[j]], lnumbersiemes))-1
            }else{
              result[[i]][[k]][[j]] <- NA
            }
          }else{
            result[[i]][[k]][[j]] <- NA
          }
        }
      }
      # search a "number of something of the word"
    }else if(str_detect(names(result)[i], "[[:blank:]][(](nbof)[)]") == TRUE){
      for (j in 1:length(result[[i]])){
        if (str_detect(result[[i]][[j]], "character[(]0[)]") == FALSE){
          if (str_detect(result[[i]][[j]], "[^0-9][0-9]+[^0-9]") == TRUE){
            result[[i]][[j]] <- str_extract(result[[i]][[j]], "[^0-9][0-9]+[^0-9]")
            result[[i]][[j]] <- as.integer(str_extract(result[[i]][[j]], "[0-9]+"))
          }else if (str_detect(result[[i]][[j]], lnumbersR) == TRUE){
            result[[i]][[j]] <- which(lnumbers == str_extract(result[[i]][[j]], lnumbers))-1
          }else if(str_detect(result[[i]][[j]], lnumbersiemesR) == TRUE){
            result[[i]][[j]] <- which(lnumbersiemes == str_extract(result[[i]][[j]], lnumbersiemes))-1
          }else{
            result[[i]][[j]] <- NA
          }
        }else{
          result[[i]][[j]] <- NA
        }
      }
      # search a score
    }else if(str_detect(names(result)[i], "[[:blank:]][(](score)[)]") == TRUE){
      for (j in 1:length(result[[i]])){
        if (str_detect(result[[i]][[j]], "character[(]0[)]") == FALSE){
          if (str_detect(result[[i]][[j]], "[^0-9][0-9]+[^0-9]") == TRUE){
            result[[i]][[j]] <- str_extract(result[[i]][[j]], "[^0-9][0-9]+[^0-9]")
            result[[i]][[j]] <- result[[i]][[j]] %>%
              str_replace_all("[,.]", ".") %>%
              str_replace_all("[:blank:]", "") %>%
              as.numeric()
          }else if (str_detect(result[[i]][[j]], lnumbersR) == TRUE){
            result[[i]][[j]] <- which(lnumbers == str_extract(result[[i]][[j]], lnumbers))-1
          }else{
            result[[i]][[j]] <- NA
          }
        }else{
          result[[i]][[j]] <- NA
        }
      }
      # search a percent
    }else if(str_detect(names(result)[i], "[[:blank:]][(](percent)[)]") == TRUE){
      for (j in 1:length(result[[i]])){
        if (str_detect(result[[i]][[j]], "character[(]0[)]") == FALSE){
          if (str_detect(result[[i]][[j]], "[:digit:]+[:blank:]{0,1}[:digit:]*[:blank:]{0,1}[,.]{0,1}[:blank:]{0,1}[:digit:]*[:blank:]{0,1}(%|pourcent)") == TRUE){
            result[[i]][[j]] <- str_extract(result[[i]][[j]], "[:digit:]+[:blank:]{0,1}[,.]{0,1}[:blank:]{0,1}[:digit:]*[:blank:]{0,1}(%|pourcent)")
            result[[i]][[j]] <- result[[i]][[j]] %>%
              str_replace_all("[%|pourcent]", "") %>%
              str_replace_all("[,.]", ".") %>%
              str_replace_all("[:blank:]", "") %>%
              as.numeric()
          }else if (str_detect(result[[i]][[j]], lnumbersR) == TRUE){
            result[[i]][[j]] <- which(lnumbers == str_extract(result[[i]][[j]], lnumbers))-1
          }else{
            result[[i]][[j]] <- NA
          }
        }else{
          result[[i]][[j]] <- NA
        }
      }
      # search a category (MAJ regex !)
    }else if(str_detect(names(result)[i], "[[:blank:]][(](cat)[)]") == TRUE){
      for (k in 1:length(result[[i]])){
        for (j in 1:length(result[[i]][[k]])){
          if (str_detect(result[[i]][[k]][[j]], "character[(]0[)]") == FALSE){
            if (str_detect(result[[i]][[k]][[j]], "[:digit:]+[:blank:]{0,1}[:digit:]*[:blank:]{0,1}[,.]{0,1}[:blank:]{0,1}[:digit:]*") == TRUE){
              result[[i]][[k]][[j]] <- str_extract(result[[i]][[k]][[j]], "[:digit:]+[:blank:]{0,1}[:digit:]*[:blank:]{0,1}[,.]{0,1}[:blank:]{0,1}[:digit:]*")
              result[[i]][[k]][[j]] <- result[[i]][[k]][[j]] %>%
                str_replace_all("[,.]", ".") %>%
                str_replace_all("[:blank:]", "") %>%
                as.numeric()
            }
          }else{
            result[[i]][[k]][[j]] <- NA
          }
        }
      }
      # search the number of "the word"
    }else if(str_detect(names(result)[i], "[[:blank:]][(](hmany)[)]") == TRUE){
      for (j in 1:length(result[[i]])){
        if (str_detect(result[[i]][[j]], "character[(]0[)]") == FALSE){
          if (str_detect(result[[i]][[j]], "[^0-9][0-9]{1,2}[^0-9]") == TRUE){
            result[[i]][[j]] <- str_extract(result[[i]][[j]], "[^0-9][0-9]{1,2}[^0-9]")
            result[[i]][[j]] <- as.integer(str_extract(result[[i]][[j]], "[0-9]+"))
          }else if (str_detect(result[[i]][[j]], lnumbersR) == TRUE){
            result[[i]][[j]] <- which(lnumbers == str_extract(result[[i]][[j]], lnumbers))-1
          }else if(str_detect(result[[i]][[j]], lnumbersiemesR) == TRUE){
            result[[i]][[j]] <- which(lnumbersiemes == str_extract(result[[i]][[j]], lnumbersiemes))-1
          }else{
            result[[i]][[j]] <- NA
          }
        }else{
          result[[i]][[j]] <- NA
        }
      }
    }else if(str_detect(names(result)[i], "[[:blank:]][(](bool)[)]") == TRUE){
      for (j in 1:length(result[[i]])){
        result[[i]][[j]] <- TRUE
      }
    }
  }
  return(result)
}
