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