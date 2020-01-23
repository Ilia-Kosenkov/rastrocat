utils::globalVariables(c("self", "private"))

#' @docType class
#' @title ReadMeGen
#' @description Generator of specific ReadMe files
#' @export
ReadMeGen <- R6::R6Class(
    "ReadMeGen",
    private = list(
        .title = NA_character_,
        .full_title = NA_character_,
        .cat_id = NA_character_,
        .authors = NA_character_,
        .year = NA_integer_,
        .description = NA_character_,
        .abstract = NA_character_,
        .references = NA_character_,
        .bibcode_references = NA_character_,
        .keywords = NA_character_,
        .adc_keywords = NA_character_,
        .data = NULL,
        .format = NULL,
        .table_notes = NA_character_,
        .remarks = NULL,
        .standard_width = function() 80L,
        .max_cat_id_len = function() 10L,
        .description_offset = function() 4L,
        .column_gap = function() 2L
    ),
    public = list(
        #' @description Constructor of \code{ReadMeGen}
        #' @return Constructed object
        #' @param cat_id Mandatory id of the catalogue / project.
        #' @param title Mandatory short title.
        #' @param authors A list of authors.
        #' @param year Year of puclication.
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



ReadMeGen$set("private", ".wrap_string", .wrap_string)
ReadMeGen$set("private", ".wrap_join", .wrap_join)
ReadMeGen$set("private", ".generate_line", .generate_line)
ReadMeGen$set("private", ".validate", .validate)
ReadMeGen$set("private", ".pad_str", .pad_str)
ReadMeGen$set("private", ".generate_data_list", .generate_data_list)
ReadMeGen$set("private", ".generate_format_table", .generate_format_table)


ReadMeGen$set("active", "Description",
function(value) {
    if (is_missing(value))
        return(private$.description)
    vec_assert(value, character(), 1L)
    private$.description <- value
    invisible(self)
})

ReadMeGen$set("active", "Abstract", function(value) {
    if (is_missing(value))
        return(private$.abstract)
    vec_assert(value, character(), 1L)
    private$.abstract <- value
    invisible(self)
})

ReadMeGen$set("active", "FullTitle", function(value) {
    if (is_missing(value))
        return(private$.full_title)
    vec_assert(value, character(), 1L)
    private$.full_title <- value
    invisible(self)
})

ReadMeGen$set("active", "TableNotes", function(value) {
    if (is_missing(value))
        return(private$.table_notes)
    vec_assert(value, character())
    private$.table_notes <- value
    invisible(self)
})


ReadMeGen$set("public", "generate_readme", .generate_readme)
ReadMeGen$set("public", "set_references", .set_references)
ReadMeGen$set("public", "set_bibcode_references", .set_bibcode_references)
ReadMeGen$set("public", "set_keywords", .set_keywords)
ReadMeGen$set("public", "set_adc_keywords", .set_adc_keywords)
ReadMeGen$set("public", "assign_data", .assign_data)
ReadMeGen$set("public", "set_remarks", .set_remarks)
ReadMeGen$set("public", "assign_multiple_datasets", .assign_multiple_datasets)
ReadMeGen$set("public", "generate_data", .generate_data)