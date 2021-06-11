#' Group extracted values into an organized table
#'
#' @description Allows to extract values of interest into the texts and group them into a table with columns indicating the text ID, the patient ID and the date. The research is done with words having an indicated “unit type”. This is done from the data table imported with the “import_data” function and from the words list imported with the “import_words” function or made with the “make_words” function.
#'
#' @param data table of data imported with the “import_data” function
#' @param word_list a list of words imported with the “import_words” function or made with the “make_words” function
#'
#' @import tibble
#'
#' @return
#' @export
#'
#' @examples
units_tracking <- function(data, word_list){
  result <- tibble(data$ID_TEXTE, data$ID_patient, data$date)
  name <- c("ID_TEXTE", "ID_patient", "date")
  names(result) <- name
  all_units <- units_values(data, word_list)
  counter <- 3
  for (i in 1:length(all_units)){
    # manage values from a "unit" or "nbof" type
    if (is.list(all_units[[i]][[1]]) == TRUE){
      for (k in 1:length(all_units[[i]])){
        counter <- counter + 1
        result[, counter] <- as.numeric(NA)
        name <- c(name, paste0(names(all_units)[i], ": ", names(all_units[[i]])[k]))
        for (j in 1:length(all_units[[i]][[k]])){
          result[as.integer(names(all_units[[i]][[k]])[j]), counter] <- all_units[[i]][[k]][[j]][1]
        }
      }
      # manage values from a "score", "percent", cat" or "hmany" type
    }else{
      counter <- counter + 1
      result[, counter] <- as.numeric(NA)
      name <- c(name, names(all_units)[i])
      for (j in 1:length(all_units[[i]])){
        result[as.integer(names(all_units[[i]])[j]), counter] <- all_units[[i]][[j]][1]
      }
    }
  }
  names(result) <- name
  return(result)
}
