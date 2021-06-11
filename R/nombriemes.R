#' nombriemes with letters
#'
#' @description Return a vector of nombriemes (in letters) from 0ieme to 1000eme.
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
nombriemes <- function(regex=FALSE, simplify=TRUE){
  chiffres <- c("zéro", "un", "deux", "trois", "quatre", "cinq", "six", "sept", "huit", "neuf", "dix",
                "onze", "douze", "treize", "quatorze", "quinze", "seize", "dix-sept", "dix-huit", "dix-neuf")
  chiffres2 <- c("zéro", "unième", "deuxième", "troisième", "quatrième", "cinquième", "sixième", "septième", "huitième", "neuvième", "dixième",
                 "onzième", "douzième", "treizième", "quatorzième", "quinzième", "seizième", "dix-septième", "dix-huitième", "dix-neuvième")
  dizaines <- c("vingt", "trente", "quarante", "cinquante", "soixante", "quatre-vingt")
  dizaines2 <- c("vingtième", "trentième", "quarantième", "cinquantième", "soixantième", "quatre-vingtième")

  all2 <- NULL
  all2 <- c(all2, "zérotième", "premier")
  all2 <- c(all2, chiffres2[3:20])
  for(i in 1:5){
    all2 <- c(all2, dizaines2[i])
    all2 <- c(all2, paste0(dizaines[i], "-et-", chiffres2[2]))
    for(j in 3:10){
      all2 <- c(all2, paste0(dizaines[i], "-", chiffres2[j]))
    }
  }
  all2 <- c(all2, paste0(dizaines[5], "-", chiffres2[11]))
  all2 <- c(all2, paste0(dizaines[5], "-et-", chiffres2[12]))
  for(j in 13:20){
    all2 <- c(all2, paste0(dizaines[5], "-", chiffres2[j]))
  }
  all2 <- c(all2, dizaines2[6])
  for(j in 2:10){
    all2 <- c(all2, paste0(dizaines[6], "-", chiffres2[j]))
  }
  all2 <- c(all2, paste0(dizaines[6], "-", chiffres2[11]))
  all2 <- c(all2, paste0(dizaines[6], "-", chiffres2[12]))
  for(j in 13:20){
    all2 <- c(all2, paste0(dizaines[6], "-", chiffres2[j]))
  }
  all2 <- c(all2, "centième")
  all2 <- c(all2, "cent-et-unième")
  for(j in 3:100){
    all2 <- c(all2, paste0("cent-", all2[j]))
  }
  for(i in 3:10){
    all2 <- c(all2, paste0(chiffres[i], "-centième"))
    all2 <- c(all2, paste0(chiffres[i], "-cent-et-unième"))
    for(j in 3:100){
      all2 <- c(all2, paste0(chiffres[i], "-cent-", all2[j]))
    }
  }
  all2 <- c(all2, "millième")
  if(simplify == TRUE){
    all2 <- all2 %>%
      str_replace_all("[éèê]", "e")
  }
  if(regex==TRUE){
    regall <- all2[1]
    for(i in 2:length(all2)){
      regall <- paste0(regall, "|", all2[i])
    }
    return(regall)
  }else{
    return(all2)
  }
}
