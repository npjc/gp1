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
#' file <- gp1_example('gp1-example1.txt')
#' read_gp1(file)
#' read_gp1(file, all_fields = TRUE)
read_gp1 <- function(file, all_fields = FALSE) {
    n_wells <- count_wells_from_header(file)
    well_labels <- mtputils::well_labels_from_length(n = n_wells)
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

parse_gp1_datetime <- function(date, time) {
    readr::parse_datetime(paste(date, time), format = "%Y/%m/%d %H:%M:%S")
}
