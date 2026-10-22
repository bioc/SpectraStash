## Utility functions used across backends/implementations.

#' Check if the file `x` already exists and throw an error if that's TRUE
#'
#' Used in:
#' - *R/MsBackendHdf5Peaks.R*: `saveMsObject()`, `saveObject()`
#' - *R/MsBackendMzR.R*: `saveMsObject()`, `saveObject()`
#'
#' @noRd
.check_overwriting <- function(x) {
    if (file.exists(x))
        stop("The provided path contains already an MS object stash. ",
             "Overwriting an existing stash is not supported. Please remove ",
             "the directory defined with parameter 'path' first.",
             call. = FALSE)
}

#' Check the type of class (`expected`) written e.g. to a text file as a comment
#'
#' Used in:
#' - *R/MsBackendHdf5Peaks.R*: `readMsObject()`
#' - *R/MsBackendMzR*: `readMsObject()`
#'
#' @noRd
.check_class_comment <- function(x, file, expected) {
    if (x[1L] != expected)
        stop("Invalid class defined in \"", file, "\": found \"", x[1L],
             "\", but it should be \"", expected, "\".", call. = FALSE)
}

#' Check for presence of files `expected` in `path`
#'
#' Used in:
#' - *R/MsBackendHdf5Peaks.R*: `readMsObject()`, `validateMsBackendHdf5Peaks()`
#' - *R/MsBackendMzR.R*: `readMsObject()`, `validateMsBackendMzR()`
#' - *R/MsBackendSql.R*: `validateAlabasterMsBackendOfflineSql()`.
#'
#' @noRd
.check_directory_content <- function(path, expected = character()) {
    if (any(miss <- !file.exists(file.path(path, expected))))
        stop("file(s) ", paste0("\"", expected[miss], "\"", collapse = ", "),
             " not found in ", path, call. = FALSE)
}

#' Helper function to write a `spectraData` `data.frame` to a tab-delimited
#' text file.
#'
#' Used in:
#' - *R/MsBackendHdf5Peaks.R*: `saveMsObject()`
#' - *R/MsBackendMzR*: `saveMsObject()`
#'
#' @noRd
#'
#' @importFrom utils write.table
.write_spectra_data <- function(x, file, sep = "\t", quote = TRUE,
                                row.names = FALSE, ...) {
    suppressWarnings(
        write.table(as.data.frame(x), file = file, sep = sep, quote = quote,
                    row.names = row.names, ...))
}

#' Helper function to read a `spectraData` `data.frame` from a tab-delimited
#' text file.
#'
#' Used in:
#' - *R/MsBackendHdf5Peaks.R*: `readMsObject()`
#' - *R/MsBackendMzR.R*: `readMsObject()`
#'
#' @noRd
#'
#' @importFrom utils read.table
.read_spectra_data <- function(file, sep = "\t", header = TRUE, ...) {
    d <- read.table(file = file, sep = sep, header = header, ...)
    rownames(d) <- NULL
    d
}

#' Copy files from their original location into the stash folder `path` and
#' replace the absolute path with a relative path in column `"dataStorage"` of
#' the `@spectraData`.
#'
#' Used in:
#' - *R/MsBackendHdf5Peaks.R*: `saveMsObject()` for `PlainTextParam`
#'   `saveObject()`
#' - *R/MsBackendMzR.R*: `saveMsObject()` for `PlainTextParam` and
#'   `saveObject()`
#'
#' @param x `MsBackend` with a `@spectraData` slot and `"dataStorage"` column
#'
#' @param path `character(1)` with the path of the stash
#'
#' @return input object but with relative data storage names (relative
#' **within** the stash folder)
#'
#' @noRd
.consolidate_data_storage <- function(x, path) {
    bp <- normalizePath(dataStorageBasePath(x))
    ufiles <- normalizePath(unique(dataStorage(x)))
    nfiles <- sub(bp, file.path(path, ""), ufiles, fixed = TRUE)
    lapply(unique(dirname(nfiles)), dir.create, recursive = TRUE,
           showWarnings = FALSE)
    file.copy(ufiles, nfiles)
    afiles <- factor(normalizePath(dataStorage(x)), levels = ufiles)
    fn <- sub(bp, "", levels(afiles), fixed = TRUE)
    fn <- sub("\\", "", fn, fixed = TRUE)
    levels(afiles) <- file.path(".", fn)
    x@spectraData$dataStorage <- as.character(afiles)
    x
}

#' Convert a stash-local into an absolute path.
#'
#' Used in:
#'
#'
#' @param x `character` with the stash-relative paths.
#'
#' @param path `character(1)` with the absolute path to the stash directory.
#'
#' @return the absolute path for `x`
#'
#' @noRd
.stash_to_absolute_path <- function(x, path) {
    path <- normalizePath(path, winslash = "/")
    normalizePath(sub("^[.]", path, x), winslash = "/")
}
