#' Check for pronouns in strings
#'
#' @param string A string you want to check for pronouns
#' @param pronouns A vector of pronouns, default c("he/him", "she/her", "they/them")
#'
#' @return A data.frame object of logical values representing presence of pronouns
#'
#' @examples check_pronouns("Some call me the space cowboy, He/Him")
#'

check_pronouns <- function(string,
                           pronouns = c("he/him",
                                        "she/her",
                                        "they/them")) {
  sapply(pronouns, function(p)
    grepl(p, tolower(string))) |>
    t() |>
    data.frame()
}
