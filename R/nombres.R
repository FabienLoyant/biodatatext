#' nombres with letters
#'
#' @description Return a vector of numbers (in letters) from 0 to 1000
#'
#' @param regex if TRUE, return a result adapted for regex
#' @param simplify if TRUE, remove accents
#'
#' @import stringr
#'
#' @return
#' @export
#'
#' @examples
nombres <- function(regex=FALSE, simplify=TRUE){
  chiffres <- c("zéro", "un", "deux", "trois", "quatre", "cinq", "six", "sept", "huit", "neuf", "dix",
                "onze", "douze", "treize", "quatorze", "quinze", "seize", "dix-sept", "dix-huit", "dix-neuf")
  dizaines <- c("vingt", "trente", "quarante", "cinquante", "soixante", "quatre-vingt")

  all <- NULL
  all <- c(all, chiffres[1:20])
  for(i in 1:5){
    all <- c(all, dizaines[i])
    all <- c(all, paste0(dizaines[i], "-et-", chiffres[2]))
    for(j in 3:10){
      all <- c(all, paste0(dizaines[i], "-", chiffres[j]))
    }
  }
  all <- c(all, paste0(dizaines[5], "-", chiffres[11]))
  all <- c(all, paste0(dizaines[5], "-et-", chiffres[12]))
  for(j in 13:20){
    all <- c(all, paste0(dizaines[5], "-", chiffres[j]))
  }
  all <- c(all, dizaines[6])
  for(j in 2:10){
    all <- c(all, paste0(dizaines[6], "-", chiffres[j]))
  }
  all <- c(all, paste0(dizaines[6], "-", chiffres[11]))
  all <- c(all, paste0(dizaines[6], "-", chiffres[12]))
  for(j in 13:20){
    all <- c(all, paste0(dizaines[6], "-", chiffres[j]))
  }
  all <- c(all, "cent")
  for(j in 2:100){
    all <- c(all, paste0("cent-", all[j]))
  }
  for(i in 3:10){
    all <- c(all, paste0(dizaines[i], "-cent"))
    for(j in 2:100){
      all <- c(all, paste0(chiffres[i], "-cent-", all[j]))
    }
  }
  all <- c(all, "mille")
  if(simplify == TRUE){
    all <- all %>%
      str_replace_all("[éèê]", "e")
  }
  if(regex == TRUE){
    regall <- all[1]
    for(i in 2:length(all)){
      regall <- paste0(regall, "|", all[i])
    }
    return(regall)
  }else{
    return(all)
  }
}
