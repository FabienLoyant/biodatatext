#' Make a tracking of values of interest for patients
#'
#' @description Allows to regroup into a list the organized results of value tracking for a list of wanted patients.
#'
#' @param units_track table of “tracking values” made with the “units_tracking” function
#' @param patient vector containing patients ID
#' @param show_na if TRUE, patients and units with no value are still written but with a “X”, instead of being deleted of the result
#'
#' @import ggplot2
#'
#' @return
#' @export
#'
#' @examples
patient_tracking <- function(units_track, patient = NULL, show_na = FALSE){
  if (is.null(patient)){
    patient <- units_track$ID_patient[-which(duplicated(units_track$ID_patient))]
  }
  mypatients <- list()
  all_i <- NULL
  for (i in 1:length(patient)){
    mypatients[[i]] <- list()
    all_j <- NULL
    interest <- FALSE
    name1 <- names(units_track[4:length(units_track)])
    for (j in 4:length(units_track)){
      mypatients[[i]][[j-3]] <- list()
      mypatients[[i]][[j-3]][[1]] <- units_track[units_track$ID_patient == patient[i], c(3,j)]
      names(mypatients[[i]][[j-3]][[1]]) <- c("date", "value")
      mypatients[[i]][[j-3]][[1]] <- mypatients[[i]][[j-3]][[1]][order(mypatients[[i]][[j-3]][[1]]$date),]
      mypatients[[i]][[j-3]][[2]] <- ggplot(mypatients[[i]][[j-3]][[1]][complete.cases(mypatients[[i]][[j-3]][[1]]),], aes(date, value)) +
        geom_point(shape=18, size = 3, color = "red") +
        xlab("Date") +
        ylab(name1[j-3]) +
        labs(title = paste0("tracking for ", name1[j-3], " \n[patient ID = \"", patient[i], "\"]")) +
        theme(plot.title=element_text(margin=margin(0,0,10,0), size=15, hjust = 0.5),
              panel.background = element_rect(fill = "white"),
              panel.grid.major = element_line(colour = "grey"),
              plot.margin = margin(20,20,20,20))
      if (show_na == FALSE){
        mypatients[[i]][[j-3]][[1]] <- na.omit(mypatients[[i]][[j-3]][[1]])
      }
      names(mypatients[[i]][[j-3]]) <- c("table", "plot")
      if(length(which(!is.na(units_track[units_track$ID_patient == patient[i], j]))) > 0){
        interest <- TRUE
      }else{
        all_j <- c(all_j, j-3)
        name1[j-3] <- paste("[X]", name1[j-3])
      }
    }
    mypatients[[i]][[j-2]] <- list()
    mypatients[[i]][[j-2]][[1]] <- units_track[units_track$ID_patient == patient[i], c(3,4:j)]
    mypatients[[i]][[j-2]][[1]] <- mypatients[[i]][[j-2]][[1]][order(mypatients[[i]][[j-2]][[1]]$date),]
    #mypatients[[i]][[j-2]][[2]] <- ggarrange(if(length() > 0){mypatients[[i]][[1]][[2]]}, if(length() > 1){mypatients[[i]][[2]][[2]]}, if(length() > 2){mypatients[[i]][[3]][[2]]},
    #  if(length() > 4){mypatients[[i]][[4]][[2]]})
    if (interest == FALSE){
      all_i <- c(all_i, i)
      patient[i] <- paste("[X]", patient[i])
      names(mypatients[[i]]) <- c(name1, "[X] ALL")
    }else{
      names(mypatients[[i]]) <- c(name1, "ALL")
    }
    mypatients[[i]] <- mypatients[[i]][-all_j]
  }
  names(mypatients) <- patient
  if(show_na == TRUE){
    return(mypatients)
  }else{
    return(mypatients[-all_i])
  }
}
