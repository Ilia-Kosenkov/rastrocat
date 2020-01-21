ReadMeGen <- R6::R6Class(
    "ReadMeGen",
    private = list(
        .title = NULL,
        .full_title = NULL,
        .cat_id = NULL,
        .authors = NULL,
        .year = NA_integer_,
        .description = NULL,
        .abstract = NULL,
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

ReadMeGen$set("active", "Abstract", function(value) {
    if (is_missing(value))
        return(private$.abstract)
    vec_assert(value, character(), 1L)
    private$.abstract <- value
    self
})


ReadMeGen$set("public", "generate_readme", .generate_readme)


ReadMeGen$new("123", "my_proj", vec_c("Tucholke H.-J.", "de Boer K.S.", "Seitter W.C.", "Tucholke H.-J.", "de Boer K.S.", "Seitter W.C."), 2020L) -> tmp
tmp$Description <- desc
tmp$Abstract <- desc
tmp$generate_readme() %>% cat