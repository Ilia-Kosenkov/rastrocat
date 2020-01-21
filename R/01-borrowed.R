# ADOPTED FROM [glue] package; vignettes

sprintf_transformer <- function(text, envir) {
    m <- regexpr(":\\ ?%.+$", text)
    if (m != -1) {
        format <- substring(regmatches(text, m), 2)
        regmatches(text, m) <- ""
        res <- eval(parse(text = text, keep.source = FALSE), envir)

        exec(sprintf, glue("{format}"), !!!res)
    } else {
        eval(parse(text = text, keep.source = FALSE), envir)
    }
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