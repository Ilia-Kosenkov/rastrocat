cc <- vec_c
`%vin%` <- vec_in

assign_inc <- function(what, at, value) {
    q_what <- enquo(what)
    q_at <- enquo(at)

    vec_assert(at, integer(), 1L)

    n <- vec_size(what)

    if (n < at) {
        temp <- vec_init(vec_ptype(what), max(at, n * 2L))
        temp[1:n] <- what[1:n]

        what <- temp
    }

    vec_slice(what, at) <- value

    assign(as.character(quo_get_expr(q_at)), at + 1L, envir = quo_get_env(q_at))
    assign(as.character(quo_get_expr(q_what)), what, envir = quo_get_env(q_what))

    what
}

assert <- function(expr, err_msg = NULL) {
    ex <- enexpr(expr)
    if (!expr) {
        if (vec_is(err_msg, character(), 1L) && !vec_is_empty(err_msg))
            abort(
                glue_fmt("Assertion `{expr_text(ex)}` failed:\n>\t{err_msg}."),
                cc("rastrocat_error_assert", "rastrocat_invalid_arg"))
        else
            abort(
                glue_fmt("Assertion `{expr_text(ex)}` failed."),
                cc("rastrocat_error_assert", "rastrocat_invalid_arg"))
    }
}

validate_formats <- function(input) {
    vec_assert(input, character())
    str_detect(str_trim(input), regex("^(?:[FE]\\d+\\.\\d+|[AI]\\d+)$", ignore_case = TRUE))
}