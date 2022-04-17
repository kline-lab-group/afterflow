
#' @title Show Message
#'
#' @description
#' Prints a message that is passed with the timestamp.
#' Indents all lines beyond the first past the time stamp
#' Throws an error instead of just printing nicely if flag is set.
#'
#' @param msg Message to print
#' @param final Name of the end function to call, defaults to base message
#' @param details Optional details, if given, will print along with message
#' @param lvls Integer to indicate number of indent spaces, default to 0
#'
#' @return Nothing
#'
#' @examples
#' show_message("This is a regular message") # Prints message with a timestamp
#' show_message("This is a warning message", final = warning) # Prints warning
#' show_message("This is an error message", final = stop) # Prints message with stop

show_message <- function(msg, details = character(), final = "message") {
  stopifnot(final %in% c("message", "warning", "stop"))
  stime <- format(Sys.time(), "%m-%d %H:%M:%S")
  indent <- paste0(stime, " - ")
  msg <- paste0(msg, ifelse(identical(details, character()), "", ": "), details)
  if (final == "message") {
    msg <- stringr::str_wrap(paste0(indent, msg), exdent = nchar(indent))
  }
  get(final)(msg, call. = ifelse(final == "message", "", FALSE))
  return()
}
