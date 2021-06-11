#' manual_read
#'
#' @description Allows to do manual verifications (by reading concerned texts) about value extractions.
#'
#' @param data table of data imported with the “import_data” function
#' @param units_track table of “tracking values” made with the “units_tracking” function
#' @param word_col number of the column corresponding to the wanted key word
#' @param word key word (regex used to read the texts quicker)
#' @param test if 1, check only cases without NA values, if 2, check only cases with NA values, if 3, check all cases
#' @param limit number of documents to check (it will be less if there are less documents to check than the limit)
#' @param sbs if TRUE, also to do the analysis document by document, else, all extracted values and code lines to execute are printed
#' @param myid a vector allowing to put the wanted text IDs to check. If NULL, all text IDs found in the data are used.
#'
#' @import stringr
#'
#' @return
#' @export
#'
#' @examples
manual_read <- function(data, units_track, word_col, word, test=1, limit=150, sbs=FALSE, myid=NULL){
  if(test == 1){
    if(length(units_track[1][!is.na(units_track[word_col])]) < limit){
      id <- units_track[1][!is.na(units_track[word_col])]
    }else{
      id <- sample(units_track[1][!is.na(units_track[word_col])], limit)
    }
  }else if(test == 2){
    if(length(units_track[1][is.na(units_track[word_col])]) < limit){
      id <- units_track[1][is.na(units_track[word_col])]
    }else{
      id <- sample(units_track[1][is.na(units_track[word_col])], limit)
    }
  }else if(test == 3){
    if(length(units_track[[1]]) < limit){
      id <- units_track[[1]]
    }else{
      id <- sample(units_track[[1]], limit)
    }
  }else{
    stop("Wrong test")
  }
  if(!is.null(myid)){
    id <- myid
  }
  cat(paste0("MANUAL READ, word=\"", word, "\", column=", word_col, "\n\n"))
  cat(paste0("Number of observations: ", length(id), " (limit=", limit, ")\n\n"))
  if(sbs == TRUE){
    i <- 1
    choice <- "0"
    while(i < (length(id))){
      if(choice == "1"){
        cat(paste0("__ We go back __"))
        i <- i - 1
      }else if(choice == "2"){
        cat(paste0("__ New try __"))
      }else if(choice == "3"){
        i <- i + 1
      }else if(choice == "4"){
        return(id)
      }
      cat(paste0("Verification number ", i, ":\n"))
      print(str_view_all(data$TEXTE[data$ID_TEXTE == id[i]], word))
      cat(paste0("ID_TEXTE=\"", id[i], "\", valeur= ", units_track[word_col][units_track[1] == id[i]], "\n"))
      choice <- readline("Back/Restart/Next/Quit ? (1/2/3/4)\n")
    }
  }else{
    cat(paste0("Code for the texts ? check: \n"))
    for(i in 1:length(id)){
      cat(paste0("str_view_all(data$TEXTE[data$ID_TEXTE == \"", id[i], "\"]", ", \"", word, "\")", "\n"))
    }
    cat(paste0("\n\nValues extracted from the texts ? check: \n"))
    for(i in 1:length(id)){
      cat(paste0("ID_TEXTE=\"", id[i], "\", value= ", units_track[word_col][units_track[1] == id[i]], "\n"))
    }
  }
  return(id)
}
