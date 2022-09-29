customStop <- function(msg, func = get_calling_function()){
    custom.msg <- paste('In ', func, ', ', msg, sep = '')
    stop(custom.msg, call. = FALSE)
}
customWarning <- function(msg, func = get_calling_function(), immediate = FALSE){
  custom.msg <- paste('In ', func, ', ', msg, sep = '')
  warning(custom.msg, call. = FALSE, immediate. = immediate)
}
#Note: get_calling_function() (defined in aux_functions.R) guesses which user-level
#function originated the call, so the second argument can generally be left blank
#(of course it is safer to specify, and S3 method [plot, etc.] will not be
#searched).
