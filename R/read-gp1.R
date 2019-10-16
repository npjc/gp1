#' @importFrom rlang .data
.data

#' get a gp1 example data file
#'
#' @param file `<chr>` name of the file
#' @export
#' @examples
#'     gp1_example('GP1-Results_feb11.txt')
#'     gp1_example('GP1-Results_jan27.txt')
#'     gp1_example('HIPHOP-Feb27Results.txt')
gp1_example <- function(file) {
    system.file("extdata", file, package = "readgp1", mustWork = TRUE)
}


#' read GP-1 output file into tidy tibble
#'
#' @param file `<chr>` path to results file
#' @param all_fields `<lgl>` should all fields be included? Defaults to [FALSE].
#'
#' @return
#' [tibble()] with the following cols:
#'
#' - **plate** identifier as found in file,
#' - **well** identifier,
#' - **runtime** time since run start in seconds,
#' - **measure** numeric value of measurement as recorded,
#'
#' if `all_fields = TRUE`:
#'
#' - **datetime** of measurement in ISO8601 (datetime format),
#' - **measure_type** the type of measurement; currently this is a stub to
#' allow for multi-measurement-type results parsing in the future if needed.
#' @export
#'
#' @examples
#' file <- gp1_example('GP1-Results_feb11.txt')
#' read_gp1(file)
#' read_gp1(file, all_fields = TRUE)
read_gp1 <- function(file, all_fields = FALSE) {
    n_wells <- count_wells_from_header(file)
    dims <- guess_dims_from_n(n_wells)
    well_labels <- well_labels(nrow = dims$nrow, ncol = dims$ncol)
    col_names <- c("plate", "date", "time", well_labels)
    col_types <- readr::cols(
        .default = readr::col_double(),
        plate = readr::col_character(),
        date = readr::col_character(), # parse as combined datetime below
        time = readr::col_character()
    )
    d <- readr::read_csv(file, col_names = col_names, col_types = col_types)
    d <- tidyr::extract(d, .data$plate, "plate", regex = "Plate\\[(\\d)\\]", convert = F)
    d <- dplyr::mutate(d,
                       plate = .data$plate,
                       datetime = parse_gp1_datetime(.data$date, .data$time)
                       )
    d <- dplyr::select(d, -.data$date, -.data$time)
    d <- dplyr::group_by(d, .data$plate, .data$datetime)
    d <- tidyr::gather(d, 'well', 'measure', -.data$plate, -.data$datetime)
    d <- dplyr::group_by(d, .data$plate, .data$well)
    d <- dplyr::mutate(d,
                       runtime = as.integer(.data$datetime - min(.data$datetime)),
                       measure_type = "Epoch (GP-1)"
                       )
    d <- dplyr::ungroup(d)
    d <- dplyr::select(d, .data$plate, .data$well, .data$datetime,
                       .data$runtime, .data$measure_type, .data$measure)
    d <- dplyr::arrange(d, .data$plate, .data$well, .data$datetime)
    if (!all_fields) {
        d <- dplyr::select(d, -.data$datetime, -.data$measure_type)
    }
    d
}

#' count the number of fields that correspond to wells from file header
#'
#' @param path `<chr>` path to raw file
#' @param n_fields_other `<int>` number of fields that are not wells. default: 3
#'
#' @return `<int>` number of wells (fields).
#' @keywords internal
count_wells_from_header <- function(path, n_fields_other = 3L) {
    total <- readr::count_fields(path, readr::tokenizer_csv(), n_max = 1L)
    total - n_fields_other
}

#' guess the dimensions of microtitre plate from the number of wells.
#'
#' @param n `<num>` number of wells
#'
#' @return
#' [list()] of length 2: number of rows (`nrow`) and columns (`ncol`).
#'
#' @keywords internal
guess_dims_from_n <- function(n) {
    dims <- switch(as.character(n),
                   '6'    = c(2L, 3L),
                   '12'   = c(3L, 4L),
                   '24'   = c(4L, 6L),
                   '48'   = c(6L, 8L),
                   '96'   = c(8L, 12L),
                   '384'  = c(16L, 24L),
                   '1536' = c(32L, 48L))
    list(nrow = dims[1], ncol = dims[2])
}

#' well labels from dimensions of microtitre plate.
#'
#' @param nrow `<int>` number of rows
#' @param ncol `<int>` number of cols
#' @param col_nchar `<int>` the number of characters for the column section. This
#'     controls the left-padding of well labels with zeros. e.g. A1 vs. A01. The
#'     default is the number of characters in `ncol` for nice lexicographic
#'     sorting behaviour.
#' @param order `<chr>` defaults to `"rowmajor"`, alternatively `"colmajor"`
#'
#' @return
#' `<chr>` vector of well labels where the row is represented by a letter,
#'  and the col is represented by an intenger, possibly left-padded with one
#'  or more zeros.
#'
#' @keywords internal
well_labels <- function(nrow, ncol, col_nchar = nchar(ncol),
                        order = c("rowmajor", "colmajor")) {
    fmt <- paste0("%s%0",col_nchar,"d")
    order <- match.arg(order)
    switch(order,
            rowmajor = sprintf(fmt, rep(row_labels(seq_len(nrow)), each = ncol), rep(seq_len(ncol), nrow)),
            colmajor = sprintf(fmt, rep(row_labels(seq_len(nrow)), ncol), rep(seq_len(ncol), each = nrow))
    )
}

row_labels <- function(row) {
    labels <- c(LETTERS, paste0(LETTERS[1], LETTERS), paste0(LETTERS[2], LETTERS))
    labels[row]
}

parse_gp1_datetime <- function(date, time) {
    readr::parse_datetime(paste(date, time), format = "%Y/%m/%d %H:%M:%S")
}

