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
    invisible(self)
}

.set_bibcode_references <- function(...) {
    refs <- map_if(str_trim(flatten_chr(list2(...))), ~ !str_starts(.x, "="), ~ paste0("=", .x))
    n <- private$.standard_width() - private$.description_offset()
    too_large <- keep(refs, ~ nchar(.x) > n)
    if (!vec_is_empty(too_large))
        abort(
            glue_fmt("The following references\n{'\t > ' %&% paste(too_large, collapse = '\n')}\n" %&%
                "are too large and exceed the maximum allowed size of {n}."),
            "rastrocat_parameter_invalid")
    private$.bibcode_references <- refs
    invisible(self)
}

.set_keywords <- function(...) {
    keys <- str_trim(flatten_chr(list2(...)))
    private$.keywords <- keys
    invisible(self)
}

.set_adc_keywords <- function(...) {
    keys <- str_trim(flatten_chr(list2(...)))
    private$.adc_keywords <- keys
    invisible(self)
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
    invisible(self)
}

.set_remarks <- function(...) {
    remarks <- vec_cast_common(!!!list2(...), .to = character())
    assert(all(nzchar(names2(remarks))), "Remarks should be convertible to named character list.")

    private$.remarks <- remarks

    invisible(self)
}
