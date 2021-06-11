#' Data importation
#'
#' @description Import data of a csv file as a table and identify the contents of the columns of interest (patients ID, text, text ID, …).
#'
#' @param file the file pathway
#' @param IDp_column an integer indicating the column containing patients ID
#' @param TEXTE_column an integer indicating the column containing texts
#' @param IDt_column an integer indicating the column containing texts ID
#' @param date_column an integer indicating the column containing dates
#' @param rest if TRUE, the function add the remaining columns
#' @param csv2 if TRUE, allows to use csv2 function instead of csv function
#' @param header TRUE if the file has a header
#'
#' @return
#' @export
#'
#' @examples

import_data <- function(file, IDp_column, TEXTE_column, IDt_column = NULL, date_column = NULL, rest = FALSE, csv2 = FALSE, header = TRUE){
  if (grepl(".csv", tolower(file)) == TRUE){
    if (csv2 == TRUE){
      data_file <- read.csv2(file, header = header)
    }else{
      data_file <- read.csv(file, header = header)
    }
    data <- data_file[,c(IDp_column, TEXTE_column)]
    names(data) <- c("ID_patient", "TEXTE")
    if (!is.null(IDt_column)){
      data$ID_TEXTE <- data_file[,IDt_column]
    }
    if (!is.null(date_column)){
      data$date <- data_file[,date_column]
    }
    if (rest == TRUE){
      x <- 1:length(data_file)
      y <- c(IDp_column, TEXTE_column, IDt_column, date_column)
      colnb <- NULL
      for (i in 1:length(x)){
        done <- 0
        for (j in 1:length(y)){
          if(x[i] != y[j]){
            done <- done + 1
          }
        }
        if (done == length(y)){
          colnb <- c(colnb, x[i])
        }
      }
      for (k in 1:length(colnb)){
        data <- data.frame(data, data_file[colnb[k]])
      }
    }
    data$TEXTE <- tolower(data$TEXTE)
  }else{
    stop("You must use CSV format.")
  }
  return(data)
}
