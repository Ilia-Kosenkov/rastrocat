ReadMeGen <- R6::R6Class(
    "ReadMeGen",
    private = list(
        .title = NULL,
        .full_title = NULL,
        .cat_id = NULL,
        .authors = NULL,
        .year = NA_integer_,
        .description = NULL,

        .standard_width = function() 80L,
        .max_cat_id_len = function() 10L,
        .description_offset = function() 4L
    ),
    public = list(
        initialize = function(cat_id, title, authors, year) {
            vec_assert(cat_id, character(), 1L)
            vec_assert(title, character(), 1L)
            vec_assert(authors, character())
            vec_assert(year, integer(), 1L)

            private$.cat_id <- str_trim(cat_id)
            private$.title <- str_trim(title)
            private$.authors <- str_trim(authors)
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

    auth_short <- private$.get_short_author()

    if (nchar(private$.cat_id) +
        nchar(private$.title) +
        nchar(auth_short) +
        4L + # Year space
        2L + # Parentheses around authorname
        3L > private$.standard_width()) # Spaces between components
        abort(glue_fmt("`cat_id`, `title`, `authors` and `year` occupy more than maximum of " %&%
                "{private$.standard_width()} allowed symbols in the header."),
            "rastrocat_parameter_invalid")

    return(TRUE)
}

.generate_line <- function(symbol, size = private$.standard_width()) {
    str_dup(symbol, size)
}

.get_short_author <- function() {
    result <- str_extract(private$.authors[1], "^[\\w\\.\\-']+(?=(\\s|$))")
    if (vec_size(private$.authors) > 1L)
        result <- paste0(result, "+")

    return(result)
}

.wrap_string <- function(str) {
    fixed_size <- private$.standard_width() - private$.description_offset()
    n <- nchar(str) %/% fixed_size

    if (n == 0L)
        return(str)

    output <- vec_init(character(), n + 2L) # extra_space

    id <- 1L
    offset <- 1L

    while (TRUE) {
        dup_size <- ifelse(id == 1L, 0L, private$.description_offset())
        sz <- fixed_size + private$.description_offset() - dup_size

        temp <- str_trim(str_sub(str, offset, offset + sz), "left")
        len <- nchar(temp)


        if (nchar(str) <= offset + sz) {
            output[id] <- str_dup(" ", dup_size) %&% str_trim(temp, "right")
            id <- id + 1L
            offset <- offset + len
            break
        }
        else if (str_ends(temp, "[\\s\\.!?\\-]")) {
            output[id] <- str_dup(" ", dup_size) %&% str_trim(temp, "right")
            id <- id + 1L
            offset <- offset + len
        }
        else {
            pos <- as.vector(str_locate_all(temp, "[\\s\\.!?\\-]")[[1]])
            pos <- pos[vec_size(pos) %/% 2L]
            if (pos < 0.8 * len) {
                output[id] <- str_dup(" ", dup_size) %&% str_trim(temp, "right")
                id <- id + 1L
                offset <- offset + len
            }
            else {
                output[id] <- str_dup(" ", dup_size) %&% str_trim(str_sub(temp, 1L, pos), "right")
                id <- id + 1L
                offset <- offset + pos
            }
        }

        if (offset > str)
            break

        if (id > vec_size(output)) {
            new_buff <- vec_init(character(), 2L * id)
            new_buff[1:vec_size(output)] <- output
            output <- new_buff
        }
    }

    return(discard(output, is.na))
}

.wrap_join <- function(str, by = "\n", pad_with = "") {
    pad_with %&% paste(private$.wrap_string(str), collapse = by)
}

.generate_readme <- function() {
    p_ <- private
    if (!p_$.validate())
        abort("The descriptor object is not valid", "rastrocat_descriptor_invalid")

    output <- vec_init(character(), 10L)

    auth_year_str <- glue_fmt("({p_$.get_short_author()} {p_$.year:%4d})")
    title_len <- p_$.standard_width() - nchar(p_$.cat_id) - nchar(auth_year_str) - 2L
    title <- glue_fmt(glue_fmt("{{p_$.cat_id}} {{p_$.title:%{title_len}s}} {{auth_year_str}}"))

    authors <- paste(p_$.authors, collapse = ", ")

    id <- 1L

    assign_inc(output, id, title)
    assign_inc(output, id, p_$.generate_line("="))
    assign_inc(output, id, p_$.wrap_join(p_$.full_title))
    assign_inc(output, id, p_$.pad_str(p_$.wrap_join(authors)))
    # Bibliography here
    assign_inc(output, id, p_$.generate_line("="))


    id <- id + 1L

    if (!is_null(p_$.description)) {
        assign_inc(output, id, p_$.wrap_join("Description:"))
        assign_inc(output, id, p_$.wrap_join(p_$.description, pad_with = str_dup(" ", p_$.description_offset())))
    }

    paste(output, collapse = "\n")
}

.pad_str <- function(str, symb = " ", size = private$.description_offset()) {
    str_dup(symb, size) %&% str
}

ReadMeGen$set("private", ".wrap_string", .wrap_string)
ReadMeGen$set("private", ".wrap_join", .wrap_join)
ReadMeGen$set("private", ".generate_line", .generate_line)
ReadMeGen$set("private", ".validate", .validate)
ReadMeGen$set("private", ".get_short_author", .get_short_author)
ReadMeGen$set("private", ".pad_str", .pad_str)

ReadMeGen$set("active", "Description", function(value) {
    if (is_missing(value))
        return(private$.description)
    vec_assert(value, character(), 1L)
    private$.description <- value
    self
})


ReadMeGen$set("public", "generate_readme", .generate_readme)
