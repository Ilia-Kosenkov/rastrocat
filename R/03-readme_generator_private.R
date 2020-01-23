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
        if (!every(cc("Format", "Label"), vec_in, names(p_$.format)))
            abort("`format` does not have required `Format` and `Label` columns.",
                "rastrocat_parameter_invalid")

        if (!validate_formats(p_$.format$Format))
            abort("`format -> Format` contains unsupported formats.",
                "rastrocat_parameter_invalid")

        if (!is_null(p_$.data) && !every(p_$.data$Data, ~ every(p_$.format$Label, vec_in, names2(.x)))) {
            abort(glue_fmt("`data` does not have all of the columns described in `format`."),
                "rastrocat_parameter_invalid")
        }
    }

    if (!is_empty(p_$.remarks)) {
        if (any(nchar(names2(p_$.remarks)) >= p_$.standard_width() - 2L)) {
            which <- which(nchar(names2(p_$.remarks)) >= p_$.standard_width() - 2L)
            abort(glue_fmt("The following `remarks` names are longer than single file line:\n" %&%
                ">\t{paste(names2(p_$.remarks)[which], collapse = '; ')}"))
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

.wrap_string <- function(
        str,
        wrap_at = "[\\s\\.!?\\-]",
        width = private$.standard_width(),
        pad_offset = private$.description_offset(),
        pad_first = TRUE,
        pad_first_offset = 0L) {
    fixed_size <- width - pad_offset - 1L
    n <- nchar(str) %/% fixed_size

    if (n == 0L)
        return(str)

    output <- vec_init(character(), n + 2L) # extra_space

    id <- 1L
    offset <- 1L

    while (TRUE) {
        dup_size <- ifelse(pad_first && id == 1L, pad_first_offset, pad_offset)
        #sz <- fixed_size + pad_offset - dup_size
        sz <- width - 1L - dup_size

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


        if (offset > nchar(str))
            break

            if (id > vec_size(output)) {
                new_buff <- vec_init(character(), 2L * id)
                new_buff[1:vec_size(output)] <- output
                output <- new_buff
            }
    }

    return(discard(output, is.na))
}

.wrap_join <- function(str, by = "\n", pad_with = "", wrap_at = "[\\s\\.!?\\-]",
        width = private$.standard_width(),
        pad_offset = private$.description_offset(),
        pad_first = TRUE,
        pad_first_offset = 0L) {
    pad_with %&%
        paste(
            private$.wrap_string(
                str, wrap_at,
                width = width, pad_offset = pad_offset,
                pad_first = pad_first,
                pad_first_offset = pad_first_offset),
            collapse = by)
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
        assign_inc(output, id,
            p_$.wrap_join(
                p_$.abstract,
                pad_first_offset = p_$.description_offset()))
    }

    if (!is_na(p_$.description)) {
        id <- id + 1L
        assign_inc(output, id, p_$.wrap_join("Description:"))
        assign_inc(output, id,
            p_$.wrap_join(
                p_$.description,
                pad_first_offset = p_$.description_offset()))
    }

    id <- id + 1L # Space

    assign_inc(output, id, "File Summary:")
    assign_inc(output, id, p_$.generate_line("-"))

    data_list <- p_$.generate_data_list()
    # File summary header
    assign_inc(output, id, data_list$Header)
    assign_inc(output, id, p_$.generate_line("-"))

    # File summary
    for (item in data_list$Body)
        assign_inc(output, id, item)
    assign_inc(output, id, p_$.generate_line("-"))

    id <- id + 2L # Two spaces

    if (!is_null(p_$.data) && !vec_is_empty(p_$.data))
        file_names <- paste(p_$.data$FileName, collapse = " ")
    else
        file_names <- ""

    assign_inc(output, id, p_$.wrap_join(glue_fmt_chr("Byte-by-byte Description of file: {file_names}")))
    assign_inc(output, id, p_$.generate_line("-"))

    format_desc <- p_$.generate_format_table()
    # Format table header
    assign_inc(output, id, format_desc$Header)
    assign_inc(output, id, p_$.generate_line("-"))

    # Format table
    for (item in format_desc$Body) {
        assign_inc(output, id, item)
    }

    assign_inc(output, id, p_$.generate_line("-"))

    # Table notes
    if (!is_na(p_$.table_notes) && !vec_is_empty(p_$.table_notes)) {
        for (item in map_chr(p_$.table_notes, p_$.wrap_join)) {
            id <- id + 1L
            assign_inc(output, id, item)
        }
        id <- id + 1L
    }


    assign_inc(output, id, p_$.generate_line("-"))

    # Insert extra textual information here
    if (!is_empty(p_$.remarks)) {
        map(p_$.remarks, map_chr, p_$.wrap_join, pad_first_offset = p_$.description_offset()) %>%
            map(paste, collapse = "\n") %>%
            imap(~glue_fmt_chr("{.y}:\n{.x}")) -> remarks

        for (item in remarks) {
            id <- id + 1L
            assign_inc(output, id, item)
        }
        id <- id + 1L
    }

    assign_inc(output, id, p_$.generate_line("="))


    auth_len <- nchar(p_$.authors[1])
    rem_space <- p_$.standard_width() - 5L - 10L - 2L
    auth_offset <- (rem_space + auth_len) %/% 2L
    date_offset <- p_$.standard_width() - 5L - 1L - auth_offset - 1L
    date <- format(Sys.time(), "%Y-%m-%d")

    assign_inc(output, id, glue_fmt_chr(glue_fmt("(End) {{p_$.authors[1]:%{auth_offset}s}} {{date:%{date_offset}s}}")))


    last_index <- vec_size(output)
    if (last_index > 1L)
        for (index in last_index:1L) {
            if (!is.na(output[index])) {
                last_index <- index
                break
            }
        }

    paste(
        replace_na(vec_slice(output, 1:max(last_index, 1L)), ""),
        collapse = "\n")
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

.assign_data <- function(format, data = NULL, file_name = NULL, desc = NULL) {
    assert(is.data.frame(format), "`format` should be a `data.frame`-compatible type")
    assert(vec_size(format) >= 1L, "`format` should have at least one row")

    if (!is_null(data)) {
        assert(is.data.frame(data), "`data` should be a `data.frame`-compatible type")
        vec_assert(file_name, character(), 1L)
        vec_assert(desc, character(), 1L)
        private$.data <- tibble(FileName = file_name, Description = desc, Data = list_of(data))
    }

    private$.format <- convert_formats(as_tibble(format))
    self
}

.set_remarks <- function(...) {
    remarks <- vec_cast_common(!!!list2(...), .to = character())
    assert(all(nzchar(names2(remarks))), "Remarks should be convertible to named character list.")

    private$.remarks <- remarks

    self
}

utils::globalVariables(c("FileName", "Data", "Description", "Explanations", "Str"))

.generate_data_list <- function() {
    p_ <- private
    if (is_empty(p_$.data))
        return(NULL)

    max_len <- sum(p_$.format$Size + 2L)

    p_$.data %>%
        transmute(
            FileName,
            Lrecl = max_len,
            Records = map_int(Data, vec_size),
            Explanations = Description) -> tmp
    bind_rows(
        tibble(FileName = "ReadMe", Lrecl = p_$.standard_width(), Records = NA_integer_, Explanations = "This file"),
        tmp) -> tmp


    max_fname_sz <- max(nchar(tmp$FileName))
    int_sz <- cc(6L, 8L)
    func <-
        (~p_$.wrap_join(
            .x,
            pad_offset = max_fname_sz + sum(int_sz) + 6L + p_$.description_offset(),
            pad_first = TRUE,
            pad_first_offset = max_fname_sz + sum(int_sz) + 6L)) %>>%
        (~str_trim(.x, "left"))

    tmp <-  mutate(tmp, Explanations = map_chr(Explanations, func))

    frmt <- glue_fmt(
        "{{FileName:%-{max_fname_sz}s}}" %&%
        "  {{as.character(Lrecl):%{int_sz[1]}s}}" %&%
        "  {{if_else(is.na(Records), '.', as.character(Records)):%{int_sz[2]}s}}" %&%
        "  {{Explanations}}")

    tmp %>%
        mutate(Str = glue_fmt_chr(frmt)) %>%
        pull(Str) -> output

    list(
        Header = glue_fmt_chr(glue_fmt(
            " {{'FileName':%-{max_fname_sz}s}}" %&%
            "  {{'Lrecl':%{int_sz[1]}s}}" %&%
            "  {{'Records':%{int_sz[1]}s}}" %&%
            "  Explanations"
         )),
         Body = output)
}

utils::globalVariables(c("Units", "Size", "Bytes", "Bytes2", "Result"))

.generate_format_table <- function() {
    p_ <- private
    frmt <- p_$.format

    max_len <- sum(frmt$Size + 2L)
    len_size <- max(3L, nchar(as.character(max_len)))

    bytes_size <- ifelse(any(frmt$Size > 1L), 2L * len_size + 1L, len_size)


    if (!("Units" %vin% names2(frmt)))
        frmt <- mutate(frmt, Units = "---")

    if (!("Explanations" %vin% names2(frmt)))
        frmt <- mutate(frmt, Explanations = "?")

    frmt <- mutate(frmt,
        Units = replace_na(Units, "---"),
        Explanations = replace_na(Explanations, "?"))

    units_size <- max(cc(nchar(frmt$Units), 5L))
    label_size <- max(cc(nchar(frmt$Label), 5L))
    format_size <- 6L

    expl_offset <- bytes_size + label_size + units_size + format_size + 5L * 2L

    frmt <- frmt %>%
        mutate(
            Bytes = cumsum(replace_na(lag(Size + 2L), 3L)),
            Bytes2 = Bytes + Size - 1L,
            Bytes = if_else(
                Bytes == Bytes2,
                glue_fmt_chr(glue_fmt("{{Bytes:%{bytes_size}d}}")),
                glue_fmt_chr(glue_fmt("{{Bytes:%{len_size}d}}-{{Bytes2:%{len_size}d}}"))),
            Bytes2 = NULL)


    func <-
        (~p_$.wrap_join(
            .x,
            pad_offset = expl_offset + p_$.description_offset(),
            pad_first = TRUE,
            pad_first_offset = expl_offset)) %>>%
        (~str_trim(.x, "left"))

    row_format <- glue_fmt(
        "  {{Bytes:%{bytes_size}s}}" %&%
        "  {{Format:%-{format_size}s}}" %&%
        "  {{Units:%-{units_size}s}}" %&%
        "  {{Label:%-{label_size}s}}" %&%
        "  {{Explanations}}")

    frmt %>%
        mutate(
            Explanations = map_chr(Explanations, func),
            Result = glue_fmt_chr(row_format)) %>%
        pull(Result) -> body

    header <- glue_fmt_chr(glue_fmt(
        "  {{'Bytes':%{bytes_size}s}}" %&%
        "  {{'Format':%-{format_size}s}}" %&%
        "  {{'Units':%-{units_size}s}}" %&%
        "  {{'Label':%-{label_size}s}}" %&%
        "  Explanations"))

    list(Header = header, Body = body)
}