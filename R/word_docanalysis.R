#' All document analyses at once
#'
#' @description Allows to do all document analyses at once, including the “word_hm”, “word_hm_dp” and “word_hm_p” functions. This is done from the data table imported with the “import_data” function and from the words list imported with the “import_words” function or made with the “make_words” function.
#'
#' @param data table of data imported with the “import_data” function
#' @param word_list a list of words imported with the “import_words” function or made with the “make_words” function
#' @param rank if TRUE, allows to return all results in descending order
#'
#' @import tibble
#' @import dplyr
#'
#' @return
#' @export
#'
#' @examples
word_docanalysis <- function(data, word_list, rank = FALSE){
  final_result <- list()
  # number of documents
  nb_docs <- length(data$ID_patient)
  # number of documents per patient counting
  counting <- as_tibble(data$ID_patient) %>%
    count(data$ID_patient, sort = TRUE)
  names(counting) <- c("ID_patient", "num")
  # number of patients
  nb_patients <- length(counting$ID_patient)
  # the final list is filled with the result of the 3 analysis function
  # ("word_hm", "word_hm_dp" and "word_hm_p")
  final_result[[1]] <- word_hm(data$TEXTE, word_list, rank)
  final_result[[2]] <- word_hm_dp(data, word_list, rank)
  final_result[[3]] <- word_hm_p(data, word_list, rank)
  # the result as a percentage is added to the "word_hm" and "word_hm_p" results
  final_result[[1]]$doc_percent <- round((final_result[[1]]$n_docs / nb_docs) * 100, 2)
  final_result[[3]]$pat_percent <- round((final_result[[3]]$n_patients / nb_patients) * 100, 2)
  names(final_result) <- c("nb_doc", "nb_docpatient", "nb_patient")
  return(final_result)
}
