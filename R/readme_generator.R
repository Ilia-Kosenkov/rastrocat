ReadMeGen <- R6::R6Class(
    "ReadMeGen",
    private = list(
        .title = NULL,
        .full_title = NULL,
        .cat_id = NULL,
        .auth_cre = NULL,
        .year = NA_integer_,
        .standard_width = function() 80L,
        .max_cat_id_len = function() 10L
    ),
    public = list(
        initialize = function(cat_id, title, auth_cre, year) {
            vec_assert(cat_id, character(), 1L)
            vec_assert(title, character(), 1L)
            vec_assert(auth_cre, character(), 1L)
            vec_assert(year, integer(), 1L)

            private$.cat_id <- str_trim(cat_id)
            private$.title <- str_trim(title)
            private$.auth_cre <- str_trim(auth_cre)
            private$.year <- year

            private$.full_title <- private$.title
        }
    ))

.validate <- function() {
    current_year <- parse_integer(substr(Sys.Date(), 1L, 4L))
    if (is_na(private$.year) || private$.year < (current_year - 100L) || private$.year > (current_year + 2L))
        abort(glue_fmt("`year`= {private$.year:%4d} seems to be out of range. Verify if it is correct"),
            "rastrocat_parameter_invalid")

    if (private$.year > current_year)
        warn(glue_fmt("`year` = { private$.year:%4d} is set in the future (current year is {current_year:%4d}."),
            "rastrocat_parameter_suspicious")

    if (nchar(private$.cat_id) > private$.max_cat_id_len())
        warn(glue_fmt("`cat_id` = { private$.cat_id} is longer than {private$.max_cat_id_len()} symbols."),
            "rastrocat_parameter_suspicious")

    if (nchar(private$.cat_id) +
        nchar(private$.title) +
        nchar(private$.auth_cre) +
        4L + # Year space
        2L + # Parentheses around authorname
        3L > private$.standard_width()) # Spaces between components
        abort(glue_fmt("`cat_id`, `title`, `auth_cre` and `year` occupy more than maximum of " %&%
                "{private$.standard_width()} allowed symbols in the header."),
            "rastrocat_parameter_invalid")

    return(TRUE)
}

.generate_readme <- function() {
    if (!private$validate())
        abort("The descriptor object is not valid", "rastrocat_descriptor_invalid")


}

ReadMeGen$set("private", "validate", .validate)
ReadMeGen$set("public", "generate_readme", .generate_readme)
