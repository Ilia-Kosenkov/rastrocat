# ADOPTED FROM [glue] package; vignettes

sprintf_transformer <- function(text, envir) {
    str_match(text, "^(.*?)(?::(\\ *%-?.+))?$")[2:3] -> expr

    vals <- eval(parse(text = expr[1], keep.source = FALSE), envir)

    if (!is.na(expr[2]))
        return(sprintf(expr[2], vals))

    return(vals)
}

glue_fmt <- function(..., .envir = parent.frame()) {
    glue(..., .envir = .envir, .transformer = sprintf_transformer)
}

glue_fmt_chr <- function(..., .envir = parent.frame()) {
    as.character(glue(..., .envir = .envir, .transformer = sprintf_transformer))
}

`%&%` <- function(x, y) {
    result <- vec_cast_common(x = x, y = y, .to = character())
    result <- vec_recycle_common(x = result$x, y = result$y)
    paste0(result$x, result$y)
}