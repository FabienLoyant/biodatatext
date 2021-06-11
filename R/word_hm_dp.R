#' Number of documents having searched words per patient
#'
#' @description Count for each word and each patient how many documents of this patient have this word. The words must be as a list previously imported with the ‘import_words” function or made with the “make_words” function.
#'
#' @param data table of data imported with the “import_data” function
#' @param word_list a list of words imported with the “import_words” function or made with the “make_words” function
#' @param rank if TRUE, allows to return all results in descending order
#' @param count if TRUE, allows to count the searched words in each document instead of only indicate if it’s present or not
#'
#' @import tibble
#' @import dplyr
#'
#' @return
#' @export
#'
#' @examples
word_hm_dp <- function(data, word_list, rank = FALSE, count = FALSE){
  # number of documents per patient counting
  counting <- as_tibble(data$ID_patient) %>%
    count(data$ID_patient, sort = FALSE)
  names(counting) <- c("ID_patient", "num")
  # number of patients
  nb_patients <- length(counting$ID_patient)
  result <- list()
  # each patient is associated with his "word_hm" result, and the corresponding percentage
  # (it's the percentage of the patient's documents containing the searched word)
  for (i in 1:nb_patients){
    result[[i]] <- word_hm(data$TEXTE[data$ID_patient == counting$ID_patient[i]], word_list, rank = rank, count = count)
    if(count == FALSE){
      result[[i]]$doc_percent <- round((result[[i]]$n_docs / counting$num[i]) * 100, 2)
    }
  }
  names(result) <- counting$ID_patient
  return(result)
}
