if (interactive()) {
    library(rlang)
    library(vctrs)
    library(glue)
    library(stringr)
    library(readr)
    library(magrittr)
    library(purrr)
    library(tidyr)
    library(dplyr)
    # Library imports for debug go here
    #
    #if (!exists("compile_src"))
        #compile_src <<- function() {
            #`%>%` <- dplyr::`%>%`
            #cmds <- vctrs::vec_c(
                #"rm src/*dll",
                #"rm src/*o",
                #"mv src/Makevars.win src/Makevars.win.cache",
                #"mv src/Makevars.win.dbg src/Makevars.win",
                #"cd src && RCMD.exe SHLIB *cpp -o **DLL_NAME**.dll",
                #"mv src/Makevars.win src/Makevars.win.dbg",
                #"mv src/Makevars.win.cache src/Makevars.win")

            #purrr::map_int(cmds, shell)
            #if (getLoadedDLLs() %>% names %>% stringr::str_detect("**DLL_NAME**") %>% any)
                #dyn.unload("src/**DLL_NAME**.dll")

            #dyn.load("src/**DLL_NAME**.dll", local = FALSE)
        #}

    #compile_src()
    purrr::walk(fs::dir_ls("R", glob = "*R"), source)
} else {

    message("Running `roxygen2::roxygenize`...")
    roxygen2::roxygenize(".")
    message("Finished `roxygen2::roxygenize`...")

    is_win <- grepl("[Ww]in(dows)?", Sys.info()["sysname"])
    if (is.na(is_win))
        stop("Unable to detect system. Run `R CMD build` manually.")
    sfx <- ifelse(is_win, ".exe", "")
    cmd_1 <- sprintf("R%s CMD build .", sfx)

    message(paste("Executing:", cmd_1))
    if (is_win)
        shell(cmd_1, mustWork = TRUE)
    else
        system(cmd_1)

    pckgs <- fs::dir_ls(".", glob = "*.gz")

    `%>%` <- dplyr::`%>%`

    stringr::str_match(pckgs, "^.*_((?:[0-9]+?\\.?){3})\\.tar\\.gz$") %>%
        dplyr::as_tibble(.name_repair = ~c("File", "Version")) %>%
        dplyr::mutate(
            VersionNum = stringr::str_split(Version, "\\."),
            Major = purrr::map_int(VersionNum, ~ readr::parse_integer(.x[1])),
            Minor = purrr::map_int(VersionNum, ~ readr::parse_integer(.x[2])),
            Patch = purrr::map_int(VersionNum, ~ readr::parse_integer(.x[3]))) %>%
        dplyr::arrange(desc(Major), desc(Minor), desc(Patch)) %>%
        dplyr::slice(1) %>%
        dplyr::pull(File) -> latest_pckg


    cmd_2 <- sprintf("R%s CMD check %s", sfx, latest_pckg)
    message(paste("Executing:", cmd_2))
    if (is_win)
        shell(cmd_2, mustWork = TRUE)
    else
        system(cmd_2)
}