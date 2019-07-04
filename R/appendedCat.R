#' Prints first text file and appends into second file
#'
#'
#' @description This function is a generic function to print the first text and appends into second
#'              file if it exists.
#'
#'
#' @param firstText character, First text.
#' @param secondText character, Second text.
#'
#'
#' @return Appended text file.
#'
#'
#' @export
#' @docType methods
#' @rdname appendedCat
#'
#' @author Yong Luo
appendedCat<- function(firstText, secondText = as.character(NA)){
    if(is.na(secondText)){
      cat(paste(firstText, "\n\n", sep = ""))
      output <- firstText
    } else {
      cat(paste(firstText, "\n\n", sep = ""))
      output <- paste(secondText, "\n\n", firstText, sep = "")
    }
    return(output)
  }
