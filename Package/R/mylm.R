#' Mein lineares Modell
#' 
#' @description Berechnet die Koeffizienten eines linearen Modells
#' @param y abhängige Variable
#' @param X Design-Matrix
#' 
#' @return Rueckgabewert
#' @details Mehr Details
#' @export
#' @examples mylm(rnorm(10), rnorm(10))
mylm <- function(y, X) {
  beta <- solve(crossprod(X, X)) %*% crossprod(X, y)
  class(beta) <- "mylm"
  beta
}

#' @export
print.mylm <- function(x, ...) {
  cat("Geschaetzte Koeffizienten:\n")
  cat(x, "\n")
}

#' Neue generische Funktion
#' 
#' @description Test
#' @param x ein object
#' @param ... zusaetzliche Argumente, die an Methoden weitergereicht werden
#' 
#' @export
myGeneric <- function(x, ...) UseMethod("myGeneric")

#' @export
myGeneric.mylm <- function(x, ...) "test"