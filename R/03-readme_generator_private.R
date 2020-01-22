.validate <- function() {
    p_ <- private

    current_year <- parse_integer(substr(Sys.Date(), 1L, 4L))
    if (is_na(p_$.year) || p_$.year < (current_year - 100L) || p_$.year > (current_year + 2L))
        abort(glue_fmt("`year`= {p_$.year:%4d} seems to be out of range. Verify if it is correct"),
            "rastrocat_parameter_invalid")

    if (p_$.year > current_year)
        warn(glue_fmt("`year` = { p_$.year:%4d} is set in the future (current year is {current_year:%4d}."),
            "rastrocat_parameter_suspicious")

    if (nchar(p_$.cat_id) > p_$.max_cat_id_len())
        warn(glue_fmt("`cat_id` = { p_$.cat_id} is longer than {p_$.max_cat_id_len()} symbols."),
            "rastrocat_parameter_suspicious")

    auth_short <- p_$.get_short_author()

    if (nchar(p_$.cat_id) +
        nchar(p_$.title) +
            nchar(auth_short) +
                4L + # Year space
                2L + # Parentheses around authorname
                3L > p_$.standard_width()) # Spaces between components
        abort(glue_fmt("`cat_id`, `title`, `authors` and `year` occupy more than maximum of " %&%
                "{p_$.standard_width()} allowed symbols in the header."),
            "rastrocat_parameter_invalid")

    if (is_null(p_$.format))
        warn("`format` is missing; it is required to build correct descriptior.",
            "rastrocat_parameter_suspicious")
    else {
        if (!(cc("Format", "Label") %vin% names(p_$.format)))
            abort("`format` does not have required `Format` and `Label` columns.",
                "rastrocat_parameter_invalid")



        if (!is_null(p_$.data) && !(p_$.format$Label %vin% names(p_$.data))) {
            df <- paste(setdiff(p_$.format$Label, names(p_$.data)), collapse = ", ")
            abort(glue_fmt("`data` does not have the following columns described in `format`:\n>\t{df}."),
                "rastrocat_parameter_invalid")
        }
    }
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

.wrap_string <- function(str, wrap_at = "[\\s\\.!?\\-]") {
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
        else if (str_ends(temp, wrap_at)) {
            output[id] <- str_dup(" ", dup_size) %&% str_trim(temp, "right")
            id <- id + 1L
            offset <- offset + len
        }
        else {
            pos <- as.vector(str_locate_all(temp, wrap_at)[[1]])
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

.wrap_join <- function(str, by = "\n", pad_with = "", wrap_at = "[\\s\\.!?\\-]") {
    pad_with %&% paste(private$.wrap_string(str, wrap_at), collapse = by)
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
    assign_inc(output, id, p_$.wrap_join(authors, pad_with = p_$.pad_str(), wrap_at = ","))
    if (!is_na(p_$.references)) {
        assign_inc(output, id,
            paste(
                glue_fmt("{str_dup(' ', p_$.description_offset())}<{p_$.references}>"),
                collapse = "\n"))
    }
    if (!is_na(p_$.bibcode_references)) {
        assign_inc(output, id,
            paste(
                glue_fmt("{str_dup(' ', p_$.description_offset())}{p_$.bibcode_references}"),
                collapse = "\n"))
    }
    assign_inc(output, id, p_$.generate_line("="))

    if (!is_na(p_$.keywords)) {
        assign_inc(output, id,
            p_$.wrap_join(
                glue_fmt("Keywords: {paste(p_$.keywords, collapse = ' ; ')}"),
                wrap_at = ";"))
    }

    if (!is_na(p_$.adc_keywords)) {
        assign_inc(output, id,
            p_$.wrap_join(
                glue_fmt("ADC_Keywords: {paste(p_$.adc_keywords, collapse = ' ; ')}"),
                wrap_at = ";"))
    }

    if (!is_na(p_$.abstract)) {
        id <- id + 1L
        assign_inc(output, id, p_$.wrap_join("Abstract:"))
        assign_inc(output, id, p_$.wrap_join(p_$.abstract, pad_with = str_dup(" ", p_$.description_offset())))
    }

    if (!is_na(p_$.description)) {
        id <- id + 1L
        assign_inc(output, id, p_$.wrap_join("Description:"))
        assign_inc(output, id, p_$.wrap_join(p_$.description, pad_with = str_dup(" ", p_$.description_offset())))
    }

    paste(output, collapse = "\n")
}

.pad_str <- function(str = "", symb = " ", size = private$.description_offset()) {
    str_dup(symb, size) %&% str
}

.set_references <- function(...) {
    refs <- str_trim(flatten_chr(list2(...)))
    n <- private$.standard_width() - private$.description_offset() - 2L
    too_large <- keep(refs, ~ nchar(.x) > n)
    if (!vec_is_empty(too_large))
        abort(
            glue_fmt("The following references\n{'\t > ' %&% paste(too_large, collapse = '\n')}\n" %&%
                "are too large and exceed the maximum allowed size of {n}."),
            "rastrocat_parameter_invalid")
    private$.references <- refs
    self
}

.set_bibcode_references <- function(...) {
    refs <- map_if(str_trim(flatten_chr(list2(...))), ~!str_starts(.x, "="), ~paste0("=", .x))
    n <- private$.standard_width() - private$.description_offset()
    too_large <- keep(refs, ~ nchar(.x) > n)
    if (!vec_is_empty(too_large))
        abort(
            glue_fmt("The following references\n{'\t > ' %&% paste(too_large, collapse = '\n')}\n" %&%
                "are too large and exceed the maximum allowed size of {n}."),
            "rastrocat_parameter_invalid")
    private$.bibcode_references <- refs
    self
}

.set_keywords <- function(...) {
    keys <- str_trim(flatten_chr(list2(...)))
    private$.keywords <- keys
    self
}

.set_adc_keywords <- function(...) {
    keys <- str_trim(flatten_chr(list2(...)))
    private$.adc_keywords <- keys
    self
}

.assign_data <- function(format, data = NULL) {
    assert(is.data.frame(format), "`format` should be a `data.frame`-compatible type.")
    assert(vec_size(format) >= 1L, "`format` should have at least one row.")

    if (!is_null(data)) {
        assert(is.data.frame(data), "`data` should be a `data.frame`-compatible type.")
        assert(ncol(data) >= vec_size(format), "`data` should contain a column for each `format` entry.")
        private$.data <- as_tibble(data)
    }

    private$.format <- as_tibble(format)
    self
}