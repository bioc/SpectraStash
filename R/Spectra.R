## have a `stashBackend` or `setBackend` similar parameter that allows to
## change the backend before stashing. For in-memory it would fall back to
## MsBackendHdf5Peaks, but we could also have MsBackendMgf etc. - maybe even
## suggesting to use MsBackendSql...
## Note: if peaksVariables **other** than `"mz"` and `"intensity"` are used,
## `MsBackendHdf5Peaks` can **not** be used (without data loss) -> error?

#' @title Spectra Stash
#'
#' @name SpectraStash
#'
#' @description
#'
#' `Spectra` objects can be stashed to (or read from) plain text file-based or
#' *alabaster*-based formats using the `saveMsObject()` or `readMsObject()`
#' functions configured with the [PlainTextParam] or [AlabasterParam] parameter
#' objects, respectively. In both cases, the data from the `Spectra` object is
#' stored into the *stash* directory defined with the parameter object.
#' Depending on the used MS backend (and parameters used), the stash can also
#' contain the MS data files.
#'
#' At present, `Spectra` objects using one of the following MS data backends
#' are supported:
#'
#' - `MsBackendMzR`: see [MsBackendMzRStash] for details and options.
#' - `MsBackendHdf5Peaks`: see [MsBackendHdf5PeaksStash] for details and
#'   options.
#' - `MsBackendMemory`: see [MsBackendMemoryStash] for details and options.
#' - `MsBackendDataFrame`: see [MsBackendMemoryStash] for details and options.
#' - `MsBackendOfflineSql` (defined in the
#'   [*MsBackendSql*](https://bioconductor.org/packages/MsBackendSql) package):
#'   see [MsBackendSqlStash] for details and options.
#'
#' The data backend of any `Spectra` backend can eventually be changed to one
#' of the above backends using the [Spectra::setBackend()] method to support
#' saving the object into a `Spectra` stash. Support for additional backends
#' respectively data representations might also be provided by separate R
#' packages.
#'
#' Details on the stash formats are provided in the respective sections below.
#'
#' @section *alabaster*-based format, `AlabasterParam`:
#'
#' With `AlabasterParam`, the `Spectra` object will be exported (or imported)
#' through the *alabaster* framework as a set of JSON and/or HDF5 files.
#' The content of each slot is stored to a separate file with the name
#' matching the slot name (converted to *snake_case*). The object's `MsBackend`
#' is stored into a sub-folder *backend* within the stash folder.
#'
#' For information on the MS backend's data stash see the respective
#' documentation.
#'
#' @section Text-file format, `PlainTextParam`:
#'
#' For this format, the data content of a `Spectra` object is stored into the
#' files:
#'
#' - *spectra_slots.txt*: plain text file containning the *processing queue*
#'   variables (separated by a `"|"`), the processing log messages (separated
#'   by a `"|"`), the processing chunk size and the `MsBackend` class used.
#' - *spectra_processing_queue.json*: the object's processing queue serialized
#'   in JSON format. It can be unserialized using `jsonlite::unserializeJSON()`.
#'
#' For information on the MS backend's data see the respective documentation.
#'
#' @param object A `Spectra` object.
#'
#' @param param Either a `PlainTextParam` or `AlabasterParam`.
#'
#' @param path For `saveObject()`: `character(1)` with the path where the
#'     object should be stored in.
#'
#' @param x A `Spectra` object.
#'
#' @param ... additional arguments passed to the `saveMsObject` or
#'     `readMsObject` method of the `Spectra`'s `MsBackend`, such as
#'     for example `consolidate` or `spectraPath`. See the `saveMsObject()`
#'     and `readMsObject()` documentation of the used `MsBackend` class for
#'     information on supported arguments.
#'
#' @return `readMsObject()` returns a [Spectra::Spectra] object.
#'
#' @author Johannes Rainer, Philippine Louail
#'
#' @examples
#'
#' ## Create a Spectra object from two example MS data files (from MsDataHub)
#' library(Spectra)
#' library(MsDataHub)
#' s <- Spectra(
#'     c(X20171016_POOL_POS_1_105.134.mzML(),
#'       X20171016_POOL_POS_3_105.134.mzML()))
#' s
#'
#' ## Filter the intensities of the Spectra removing peaks with an intensity
#' ## below 100
#' s <- filterIntensity(s, intensity = 100)
#'
#' ## Define the format and location of the `Spectra` stash: use the
#' ## *alabaster*-based format and store the stash in a folder named
#' ## *spectra_stash* in a temporary directory
#' d <- file.path(tempdir(), "spectra_stash")
#' ap <- AlabasterParam(d)
#'
#' ## Stash the `Spectra` object copying in addition the MS data files into the
#' ## stash (`consolidate = TRUE`).
#' saveMsObject(s, ap, consolidate = TRUE)
#'
#' ## Show the content of the stash
#' dir(d)
#'
#' ## Read the `Spectra` object from the stash:
#' res <- readMsObject(Spectra(), ap)
#' res
#'
#' ## It is also possible to read individual contents from the stash. The
#' ## directory *backend* contains for example the stashed `MsBackend` of the
#' ## `Spectra` object. To read only the `MsBackend`:
#' ap2 <- AlabasterParam(file.path(d, "backend"))
#' b <- readMsObject(MsBackendMzR(), ap2)
#' b
#'
#' ## Alternatively, that data can also be read directly with the `readObject()`
#' ## method from the *alabaster.base* package:
#' library(alabaster.base)
#' b <- readObject(file.path(d, "backend"))
#' b
NULL

################################################################################
##    PlainTextParam
################################################################################

#' @rdname SpectraStash
#'
#' @importFrom methods hasMethod
#'
#' @exportMethod saveMsObject
setMethod("saveMsObject", signature(object = "Spectra",
                                    param = "PlainTextParam"),
          function(object, param, ...) {
              dir.create(path = param@path,
                         recursive = TRUE,
                         showWarnings = FALSE)
              if (!hasMethod("saveMsObject", c(class(object@backend)[1L],
                                               "PlainTextParam")))
                  stop("Can not store a 'Spectra' object with backend '",
                       class(object@backend)[1L], "'", call. = FALSE)
              saveMsObject(object@backend, param = param, ...)
              .export_spectra_processing_queue(object, path = param@path)
              .export_spectra_slots(object, path = param@path)
          })

#' @rdname SpectraStash
#'
#' @importFrom stats setNames
setMethod("readMsObject", signature(object = "Spectra",
                                    param = "PlainTextParam"),
          function(object, param, ...) {
              fl  <- file.path(param@path, "spectra_slots.txt")
              if (!file.exists(fl))
                  stop("No 'spectra_slots.txt' file found in ", param@path)
              fls  <- readLines(fl)
              var_names <- sub(" =.*", "", fls)
              var_values <- sub(".* = ", "", fls)
              variables <- setNames(var_values, var_names)
              if (!hasMethod("readMsObject", c(variables[["backend"]],
                                               "PlainTextParam")))
                  stop("Can not read a 'Spectra' object with backend '",
                       variables["backend"], "'", call. = FALSE)
              tryCatch({
                  object@backend <- readMsObject(
                      do.call(variables[["backend"]], list()), param, ...)
              }, error = function(e) {
                  stop("Failed to load a backend of type '",
                       variables[["backend"]],
                       "'. Please load the package defining this type",
                       " of class and try again.\n - ", e$message,
                       call. = FALSE)
              })
              object@processingQueueVariables <- unlist(
                  strsplit(variables[["processingQueueVariables"]],
                           "|", fixed = TRUE))
              object@processing <- unlist(
                  strsplit(variables[["processing"]], "|" , fixed = TRUE))
              object@processingChunkSize <- as.numeric(
                  variables[["processingChunkSize"]])
              fl <- file.path(param@path, "spectra_processing_queue.json")
              if (file.exists(fl))
                  object <- .import_spectra_processing_queue(object, file = fl)
              validObject(object)
              object
          })

################################################################################
##    AlabasterParam
################################################################################

#' @rdname SpectraStash
#'
#' @exportMethod saveObject
setMethod("saveObject", "Spectra", function(x, path, ...) {
    if (!hasMethod("saveObject", class(x@backend)[1L]))
        stop("No method to save a backend of type \"", class(x@backend)[1L],
             "\" available yet. Consider changing to one of the supported ",
             "MS backends using the 'setBackend()' method.")
    dir.create(path = path, recursive = TRUE, showWarnings = FALSE)
    altSaveObject(x@backend, path = file.path(path, "backend"), ...)
    saveObjectFile(path, "spectra",
                   list(spectra = list(version = "1.0")))
    .export_spectra_processing_queue(x, path = path)
    altSaveObject(x@processingQueueVariables,
                  path = file.path(path, "processing_queue_variables"))
    altSaveObject(x@processing, path = file.path(path, "processing"))
    altSaveObject(x@metadata, path = file.path(path, "metadata"))
    altSaveObject(x@processingChunkSize,
                  path = file.path(path, "processing_chunk_size"))
})

validateAlabasterSpectra <- function(path = character(),
                                     metadata = list()) {
    .check_directory_content(path, c("OBJECT", "backend",
                                     "processing_queue_variables",
                                     "spectra_processing_queue.json",
                                     "processing", "metadata",
                                     "processing_chunk_size"))
    ob <- readObjectFile(path)
    if (ob$type != "spectra")
        stop("Invalid OBJECT format. Expected \"spectra\"", call. = FALSE)
}

#' @importFrom Spectra Spectra
readAlabasterSpectra <- function(path = character(), metadata = list(),
                                 ...) {
    validateAlabasterSpectra(path, metadata)
    s <- Spectra()
    s@backend <- altReadObject(file.path(path, "backend"), ...)
    s <- .import_spectra_processing_queue(
        s, file.path(path, "spectra_processing_queue.json"))
    s@processingQueueVariables <- altReadObject(file.path(
        path, "processing_queue_variables"))
    s@processing <- altReadObject(file.path(path, "processing"))
    s@metadata <- altReadObject(file.path(path, "metadata"))
    s@processingChunkSize <- altReadObject(
        file.path(path, "processing_chunk_size"))
    validObject(s)
    s
}

#' @rdname SpectraStash
setMethod("saveMsObject", signature(object = "Spectra",
                                    param = "AlabasterParam"),
          function(object, param, ...) {
              saveObject(object, path = param@path, ...)
          })

#' @rdname SpectraStash
setMethod("readMsObject", signature(object = "Spectra",
                                    param = "AlabasterParam"),
          function(object, param, ...) {
              readAlabasterSpectra(path = param@path, ...)
          })

################################################################################
##    Internal functions
################################################################################

#' @description
#'
#' Export the `processingQueueVariables`, `processing` and
#' `processingChunkSize` slots of a `Spectra` object to a text file.
#' The class of the backend is also saved.
#'
#' @param x  `Spectra`
#'
#' @author Philippine Louail
#'
#' @noRd
.export_spectra_slots <-function(x, path = character()){
    con <- file(file.path(path, "spectra_slots.txt"), open = "wt")
    on.exit(close(con))
    pq <- x@processingQueueVariables
    writeLines(paste0("processingQueueVariables = ", paste(pq, collapse = "|")),
               con = con)
    p <- x@processing
    writeLines(paste0("processing = ", paste(p, collapse = "|")), con = con)
    writeLines(paste0("processingChunkSize = ",
                      Spectra::processingChunkSize(x)), con = con)
    writeLines(paste0("backend = ", class(x@backend)[1L]), con = con)
}

#' Processing queue
#' @param x  `Spectra`
#'
#' @importFrom jsonlite write_json
#'
#' @importFrom jsonlite serializeJSON
#'
#' @author Philippine Louail
#'
#' @noRd
.export_spectra_processing_queue <- function(x, path = character()) {
    write_json(serializeJSON(x@processingQueue),
               file.path(path, "spectra_processing_queue.json"))
}

#' @importFrom jsonlite unserializeJSON
#'
#' @importFrom jsonlite read_json
#'
#' @author Philippine Louail
.import_spectra_processing_queue <- function(x, file = character()) {
    x@processingQueue <- unserializeJSON(read_json(file)[[1L]])
    x
}
