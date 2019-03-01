#' get a gp1 example data file
#'
#' @param file <chr> name of the file
#' @export
gp1_example <- function(file) {
    system.file("extdata", file, package = "gp1", mustWork = TRUE)
}


#' get well labels from number of rows and cols
#'
#' @param nrow <int> number of rows
#' @param ncol <int> number of cols
#' @param order <chr> defaults to 'rowmajor', alternatively 'colmajor'
#'
mtp_wells <- function(nrow, ncol, order = c("rowmajor", "colmajor")) {
    switch (order[1],
            rowmajor = paste0(rep(LETTERS[1:nrow], each = ncol), sprintf("%02d",rep(1:ncol, nrow))),
            colmajor = paste0(rep(LETTERS[1:nrow], ncol), sprintf("%02d",rep(1:ncol, each = nrow)))
    )
}


#' read GP-1 output file into tidy tibble
#'
#' @param path <chr> path to results file
#' @param well_labels <chr> vector of well labels, defaults to row major 48MTP ('A01', 'A02',...)
#' @export
read_gp1 <- function(path, well_labels = gp1::mtp_wells(6, 8)) {
    col_names <- c("plate", "date", "time", well_labels)
    col_types <- readr::cols(
        .default = readr::col_double(),
        plate = readr::col_character(),
        date = readr::col_character(), # parse as combined datetime below
        time = readr::col_character()
    )
    d <- readr::read_csv(path, col_names = col_names, col_types = col_types)
    d <- tidyr::extract(d, plate, "plate", regex = "Plate\\[(\\d)\\]", convert = F)
    d <- dplyr::mutate(d, datetime = readr::parse_datetime(paste(date, time),
                                               format="%Y/%m/%d %H:%M:%S"))
    d <- dplyr::select(d, -date, -time)
    d <- dplyr::group_by(d, plate, datetime)
    d <- tidyr::gather(d, well, value, -plate, -datetime)
    d <- dplyr::ungroup(d)
    dplyr::arrange(d, plate, well, datetime)
}
