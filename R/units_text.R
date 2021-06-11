#' Extraction of text samples of interest
#'
#' @description The main goal is to extract text samples containing values of interest. The research is done with words having an indicated “unit type”. This is done from the data table imported with the “import_data” function and from the words list imported with the “import_words” function or made with the “make_words” function.
#'
#' @param data table of data imported with the “import_data” function
#' @param word_list a list of words imported with the “import_words” function or made with the “make_words” function
#' @param test take as value either “extract”, “extract_mat”, “extract_char” or “locate”, allowing to have a result respectively as a list, a matrix or a string of characters, or to locate the searched words into the texts (localization done thanks to 2 numbers showing the beginning and the end of the word)
#' @param IDt if TRUE, allows to give results with the text ID instead of the text row numbers
#'
#' @import tibble
#'
#' @return
#' @export
#'
#' @examples
units_text <- function(data, word_list, test = "extract", IDt = FALSE){
  if (test == "extract" | test == "extract_mat" | test == "extract_char"){
    # STEP 1: keep only the words with "units" to search
    avunits <- c("unit", "nbof", "score", "percent", "cat", "hmany", "bool")
    allwords <- list()
    counter <- 1
    name_allwords <- NULL
    lnumbers <- nombres(regex = TRUE, simplify = TRUE)
    lnumbersiemes <- nombriemes(regex = TRUE, simplify = TRUE)
    for (i in 1:length(word_list)){
      # obtain a "normal" word
      if (length(word_list[[i]][[1]]) == 1){
        if (word_list[[i]][[3]][1] %in% avunits){
          allwords[[counter]] <- word_list[[i]]
          name_allwords <- c(name_allwords, names(word_list)[[i]])
          counter <- counter + 1
        }
        # obtain a list of grouped words
      }else{
        if (word_list[[i]][[1]][[3]][1] %in% avunits){
          allwords[[counter]] <- word_list[[i]]
          name_allwords <- c(name_allwords, names(word_list)[[i]])
          counter <- counter + 1
        }
      }
    }
    names(allwords) <- name_allwords
  }else{
    allwords <- word_list
  }
  if(length(allwords) == 0){
    stop("There is no unit to search. Check the words file.")
  }
  # STEP 2: text mining to find the "units"
  word_position <- where_is_word(data, allwords, IDt = IDt)
  # locator: list used each loop as a tool to stock all results
  # onelocator: list used each loop as a tool to stock one of the result
  locator <- list()
  onelocator <- list()
  target <- NULL
  name1 <- NULL
  name2 <- NULL
  # OPTION 1: extraction of some information
  if (test == "extract" | test == "extract_mat" | test == "extract_char"){
    result <- list()
    perunit <- list()
    counter2 <- 0
    for (i in 1:length(word_position)){
      if(length(word_position[[i]]) > 0){
        if (length(allwords[[i]][[1]]) == 1){
          target <- allwords[[i]][[3]]
        }else{
          target <- allwords[[i]][[1]][[3]]
        }
        # search units
        if (target[1] == "unit"){
          counter2 <- counter2 + 1
          for (k in 1:(length(target)-1)){
            for (j in 1:length(word_position[[i]])){
              # regex allowing to extract text containing units
              onelocator[[j]] <- str_extract_all(
                if(IDt == TRUE){
                  string = data$TEXTE[data$ID_TEXTE == word_position[[i]][j]]
                }else{
                  string = data$TEXTE[[word_position[[i]][j]]]},
                paste0("([:digit:]+[:blank:]{0,1}[:digit:]*[,.]{0,1}[:digit:]*[:blank:]{0,2}",
                       target[k+1], "[^\\w/].{0,15}[^[:alpha:]]",
                       allwords[[i]][[1]][[1]], "[^[:alpha:]])|([^[:alpha:]]",
                       allwords[[i]][[1]][[1]],
                       "[^[:alpha:]]([:blank:]{0,1}|[^[:alpha:]]+.{0,15})[:digit:]+[:blank:]{0,1}[:digit:]*[:blank:]{0,1}[,.]{0,1}[:blank:]{0,1}[:digit:]*[:blank:]{0,2})",
                       target[k+1], "[^\\w/]"),
                if(test == "extract_mat"){
                  simplify = TRUE
                }else{
                  simplify = FALSE
                })
              if (test == "extract_char"){
                onelocator[[j]] <- as.character(onelocator[[j]])
              }
              name2 <- c(name2, word_position[[i]][[j]])
            }
            names(onelocator) <- name2
            name2 <- NULL
            perunit[[k]] <- onelocator
            onelocator <- list()
          }
          # search a "number of something of the word"
        }else if (target[1] == "nbof"){
          counter2 <- counter2 + 1
          for (k in 1:(length(target)-1)){
            for (j in 1:length(word_position[[i]])){
              # regex allowing to extract text containing a "number of something of the word"
              onelocator[[j]] <- str_extract_all(
                if(IDt == TRUE){
                  string = data$TEXTE[data$ID_TEXTE == word_position[[i]][j]]
                }else{
                  string = data$TEXTE[[word_position[[i]][j]]]},
                paste0("(((([^0-9][0-9]+|", lnumbers, "|", lnumbersiemes, ")+",
                       ".{0,15}(", target[k+1], "){1}.{0,50})|",
                       "((", target[k+1], "){1}.{0,10}[^0-9][0-9]+[^0-9]))[^[:alpha:]]",
                       allwords[[i]][[1]][[1]], "[^[:alpha:]])|",
                       "((", target[k+1], "){1}.{0,40}[^[:alpha:]]",
                       allwords[[i]][[1]][[1]],
                       "[^[:alpha:]].{0,20}[^0-9][0-9]+[^0-9])"),
                if(test == "extract_mat"){
                  simplify = TRUE
                }else{
                  simplify = FALSE
                })
              if (test == "extract_char"){
                onelocator[[j]] <- as.character(onelocator[[j]])
              }
              name2 <- c(name2, word_position[[i]][[j]])
            }
            names(onelocator) <- name2
            name2 <- NULL
            perunit[[k]] <- onelocator
            onelocator <- list()
          }
          # search a score
        }else if (target[1] == "score"){
          counter2 <- counter2 + 1
          for (j in 1:length(word_position[[i]])){
            # regex allowing to extract text containing a score
            onelocator[[j]] <- str_extract_all(
              if(IDt == TRUE){
                string = data$TEXTE[data$ID_TEXTE == word_position[[i]][j]]
              }else{
                string = data$TEXTE[[word_position[[i]][j]]]},
              paste0("[^[:alpha:]]", allwords[[i]][[1]][[1]],
                     "([^[:alpha:]].{0,20}(([^0-9][0-9]+[^0-9])|[^[:alpha:]](", lnumbers, ")[^[:alpha:]])|([0-9]+[^0-9]))"),
              if(test == "extract_mat"){
                simplify = TRUE
              }else{
                simplify = FALSE
              })
            if (test == "extract_char"){
              onelocator[[j]] <- as.character(onelocator[[j]])
            }
            name2 <- c(name2, word_position[[i]][[j]])
          }
          # search a percentage
        }else if (target[1] == "percent"){
          counter2 <- counter2 + 1
          for (j in 1:length(word_position[[i]])){
            # regex allowing to extract text containing a percentage
            onelocator[[j]] <- str_extract_all(
              if(IDt == TRUE){
                string = data$TEXTE[data$ID_TEXTE == word_position[[i]][j]]
              }else{
                string = data$TEXTE[[word_position[[i]][j]]]},
              paste0("((([:digit:]+[:blank:]{0,1}[:digit:]*[:blank:]{0,1}[,.]{0,1}[:blank:]{0,1}[:digit]*)|", lnumbers, ")[:blank:]{0,2}",
                     "(%|pourcent|pourcents)", "[^\\w/].{0,15}, [^[:alpha:]]",
                     allwords[[i]][[1]][[1]], "[^[:alpha:]])|([^[:alpha:]]",
                     allwords[[i]][[1]][[1]],
                     "[^[:alpha:]].{0,15}(([:digit:]+[:blank:]{0,1}[:digit:]*[:blank:]{0,1}[,.]{0,1}[:blank:]{0,1}[:digit]*)|", lnumbers, ")[:blank:]{0,2}",
                     "(%|pourcent|pourcents)", "[^\\w/])"),
              if(test == "extract_mat"){
                simplify = TRUE
              }else{
                simplify = FALSE
              })
            if (test == "extract_char"){
              onelocator[[j]] <- as.character(onelocator[[j]])
            }
            name2 <- c(name2, word_position[[i]][[j]])
          }
          # search a category
        }else if (target[1] == "cat"){
          counter2 <- counter2 + 1
          for (j in 1:length(word_position[[i]])){
            # regex allowing to extract text containing a category
            onelocator[[j]] <- str_extract_all(
              if(IDt == TRUE){
                string = data$TEXTE[data$ID_TEXTE == word_position[[i]][j]]
              }else{
                string = data$TEXTE[[word_position[[i]][j]]]},
              paste0("((", target[2], ")", ".{0,15}[^[:alpha:]]",
                     allwords[[i]][[1]][[1]], "[^[:alpha:]])|([^[:alpha:]]",
                     allwords[[i]][[1]][[1]],
                     "[^[:alpha:]].{0,15}",
                     "(", target[2], "))"),
              if(test == "extract_mat"){
                simplify = TRUE
              }else{
                simplify = FALSE
              })
            if (test == "extract_char"){
              onelocator[[j]] <- as.character(onelocator[[j]])
            }
            name2 <- c(name2, word_position[[i]][[j]])
          }
          # search the number of "the word"
        }else if (target[1] == "hmany"){
          counter2 <- counter2 + 1
          for (j in 1:length(word_position[[i]])){
            # regex allowing to extract text containing the number of "the word"
            onelocator[[j]] <- str_extract_all(
              if(IDt == TRUE){
                string = data$TEXTE[data$ID_TEXTE == word_position[[i]][j]]
              }else{
                string = data$TEXTE[[word_position[[i]][j]]]},
              paste0("(((([0-9]{1,2}|", lnumbers, "|", lnumbersiemes,
                     ".{0,10}(", "[^[:alpha:]]",
                     allwords[[i]][[1]][[1]], "[^[:alpha:]])|",
                     "([^[:alpha:]]",
                     allwords[[i]][[1]][[1]],
                     "[^[:alpha:]]+.{0,20}[^0-9][0-9]{1,2}[^0-9])"),
              if(test == "extract_mat"){
                simplify = TRUE
              }else{
                simplify = FALSE
              })
            if (test == "extract_char"){
              onelocator[[j]] <- as.character(onelocator[[j]])
            }
            name2 <- c(name2, word_position[[i]][[j]])
          }
          # search if the patient of the document is concerned about the word (yes or not)
        }else if(target[1] =="bool"){
          counter2 <- counter2 + 1
          for (j in 1:length(word_position[[i]])){
            # regex allowing to extract text containing the number of "the word"
            onelocator[[j]] <- str_extract_all(
              if(IDt == TRUE){
                string = data$TEXTE[data$ID_TEXTE == word_position[[i]][j]]
              }else{
                string = data$TEXTE[[word_position[[i]][j]]]},
              paste0("[^!.?]*", allwords[[i]][[1]][[1]], "[^!.?]*"),
              if(test == "extract_mat"){
                simplify = TRUE
              }else{
                simplify = FALSE
              })
            if (test == "extract_char"){
              onelocator[[j]] <- as.character(onelocator[[j]])
            }
            name2 <- c(name2, word_position[[i]][[j]])
          }
        }
        # put the result of the loop in locator
        if(target[1] == "unit" | target[1] == "nbof"){
          names(perunit) <- target[2:length(target)]
          locator[[counter2]] <- perunit
          perunit <- list()
          name1 <- c(name1, paste0(names(word_position)[[i]], " (", target[1], ")"))
        }else{
          name1 <- c(name1, paste0(names(word_position)[[i]], " (", target[1], ")"))
          names(onelocator) <- name2
          name2 <- NULL
          locator[[counter2]] <- onelocator
          onelocator <- list()
        }
      }
    }
    # OPTION 2: localization of the cibled words into the text
  }else if (test == "locate"){
    for (i in 1:length(word_position)){
      if(length(word_position[[i]]) > 0){
        for (j in 1:length(word_position[[i]])){
          onelocator[[j]] <- str_locate_all(
            if(IDt == TRUE){
              string = data$TEXTE[data$ID_TEXTE == word_position[[i]][j]]
            }else{
              string = data$TEXTE[[word_position[[i]][j]]]},
            allwords[[i]][[1]][[1]])
          name2 <- c(name2, word_position[[i]][[j]])
        }
      }
      name1 <- c(name1, names(word_position)[[i]])
      names(onelocator) <- name2
      name2 <- NULL
      locator[[i]] <- onelocator
      onelocator <- list()
    }
  }
  names(locator) <- name1
  return(locator)
}
