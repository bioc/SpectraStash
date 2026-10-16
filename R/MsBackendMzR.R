## Save/read MsBackendMzR instances to plain text file format.

#' @title MsBackendMzR Stash
#'
#' @name MsBackendMzRStash
#'
#' @description
#'
#' `MsBackendMzR` classes can be stashed to (or read from) plain text
#' file-based or *alabaster*-based formats using the [saveMsObject()] and
#' [readMsObject()] functions combined with the [PlainTextParam] and
#' [AlabasterParam] parameter objects, respectively. Setting parameter
#' `consolidate = TRUE` in the `saveMsObject()` or `saveObject()` function
#' will copy also the original MS data files into the folder generating a
#' self-consistent stash. This stash folder can also be copied to another
#' computer or location in the file system without the need to use parameter
#' `spectraPath` when restoring the data.
#'
#' Additional properties of the stash formats are described in detail in the
#' sections below.
#'
#' @details
#'
#' `MsBackendMzR` objects don't contain any peaks data (i.e., *m/z* and
#' intensity values) but retrieve these from the original MS data files (in
#' mzML, mzXML or CDF format). Unless `consolidate = TRUE` is used, a
#' `MsBackendMzR` stash will therefore only contain the spectra metadata
#' (i.e., the spectra variables) but no peaks data. The reference to the
#' original MS data files is stored as spectra variable *dataStorage* and if
#' the files are no longer available in the directory specified by
#' *dataStorage* the restored object will not be valid, unless the new location
#' is provided with parameter `spectraPath`.
#'
#' @section *alabaster*-based format, `AlabasterParam`:
#'
#' The `saveMsObject()` with an `AlabasterParam` parameter object stashes the
#' provided `MsBackendMzR` object in an *alabster*-based format into the
#' directory defined with argument `param` of the `AlabasterParam`.
#' `readMsObject()` with `AlabasterParam` restores a previously stashed
#' `MsBackend` object. Optional parameter `spectraPath` allows to specify the
#' storage path of the MS data files referenced by the `MsBackendMzR` (in case
#' they are no longer in the same directory when saving the object).
#'
#' In addition, the *alabaster* methods `saveObject()` and `readObject()` can
#' be used to save and read `MsBackendMzR` objects.
#'
#' @section Text-file format, `PlainTextParam`:
#'
#' The `saveMsObject()` function with the `PlainTextParam` stores the spectra
#' metadata (spectra variables) of an `MsBackendMzR` to a plain tabulator
#' delimited text file with the name *ms_backend_spectra_data.txt* in the
#' directory specified with parameter `path` of the `PlainTextParam` object.
#' Importantly, the peaks data (the *m/z* and intensity values) are **not**
#' exported with `saveMsObject()`.
#'
#' `readMsObject()` restores a previously stashed `MsBackendMzR` object from
#' the directory specified with parameter `path` of the `PlainTextParam`.
#'
#' The additional parameter `spectraPath` of `readMsObject()` allows to define
#' the path to the MS data files containing the full MS data (i.e., the mzML,
#' mzXML or CDF files referred to by the `MsBackendMzR`).
#'
#' @param consolidate `logical(1)` whether in addition to the spectra metadata
#'     also the original MS data files should be stored in the stash
#'     folder. Default is `consolidate = FALSE`.
#'
#' @param object An `MsBackendMzR` object.
#'
#' @param param Either a `PlainTextParam` or `AlabasterParam`.
#'
#' @param path For `saveObject()`: `character(1)` with the path where the
#'     object should be stored in.
#'
#' @param spectraPath For `readMsObject()`: optional `character(1)` with the
#'     path to the MS data files (mzML, mzXML or CDF) in case they are on longer
#'     available in the folder referred to by the original stashed
#'     `MsBackendMzR` object.
#'
#' @param x An `MsBackendMzR` object.
#'
#' @param ... Currently ignored.
#'
#' @return `readMsObject()` returns an [Spectra::MsBackendMzR] object.
#'
#' @author Philippine Louail, Johannes Rainer
#'
#' @examples
#'
#' library(SpectraStash)
#' library(Spectra)
#' library(MsDataHub)
#'
#' ## Create a MsBackendMzR from test data
#' be <- backendInitialize(MsBackendMzR(), PestMix1_DDA.mzML())
#' be
#'
#' ## Define a folder where to stash the object
#' pth <- file.path(tempdir(), "mzr_stash")
#'
#' ## Stash the object to this folder in a plain text-based format
#' saveMsObject(be, PlainTextParam(pth))
#'
#' ## Restore the stashed object
#' res <- readMsObject(MsBackendMzR(), PlainTextParam(pth))
#' res
#'
#' ## Clean-up
#' unlink(pth, recursive = TRUE)
#'
#' ## Store the data in *alabaster* format including also the original MS
#' ## data files (`consolidate = TRUE`)
#' saveMsObject(be, AlabasterParam(pth), consolidate = TRUE)
#'
#' ## Get the directory content of the stash folder:
#' dir(pth)
#'
#' ## Restore the object
#' res <- readMsObject(MsBackendMzR(), AlabasterParam(pth))
#' res
#'
#' ## If the data is exported with `consolidate = FALSE` (the default), the
#' ## new location of MS data files could be provided with parameter
#' ## `spectraPath` of the `readMsObject()` function in case they are no
#' ## longer in the path referenced by the stashed object.
#'
#' ## Clean-up
#' unlink(pth, recursive = TRUE)
NULL

################################################################################
##    PlainTextParam
################################################################################

#' @importMethodsFrom MsStash saveMsObject
#'
#' @importClassesFrom Spectra MsBackendMzR
#'
#' @importMethodsFrom Spectra dropNaSpectraVariables
#'
#' @rdname MsBackendMzRStash
setMethod("saveMsObject", signature(object = "MsBackendMzR",
                                    param = "PlainTextParam"),
          function(object, param, consolidate = FALSE) {
              object <- dropNaSpectraVariables(object)
              dir.create(param@path, showWarnings = FALSE, recursive = TRUE)
              fl <- file.path(param@path, .MS_BACKEND_MZR_DATA_FILE)
              .check_overwriting(fl)
              writeLines(paste0("# ", class(object)[1L]), con = fl)
              if (nrow(object@spectraData)) {
                  if (consolidate)
                      object <- .consolidate_data_storage(object, param@path)
                  .write_spectra_data(object@spectraData, fl, append = TRUE)
              }
          })

#' @importMethodsFrom MsStash readMsObject
#'
#' @importFrom methods validObject
#'
#' @importMethodsFrom Spectra dataStorageBasePath<-
#'
#' @importFrom S4Vectors DataFrame
#'
#' @rdname MsBackendMzRStash
setMethod("readMsObject", signature(object = "MsBackendMzR",
                                    param = "PlainTextParam"),
          function(object, param, spectraPath = character()) {
              fl <- file.path(param@path, .MS_BACKEND_MZR_DATA_FILE)
              .check_directory_content(param@path, .MS_BACKEND_MZR_DATA_FILE)
              l2 <- readLines(fl, n = 2)
              .check_class_comment(l2, .MS_BACKEND_MZR_DATA_FILE,
                                   "# MsBackendMzR")
              if (length(l2) > 1L) {
                  object@spectraData <- DataFrame(.read_spectra_data(fl))
                  if (length(spectraPath))
                      dataStorageBasePath(object) <- spectraPath
                  if (all(grepl("^[.](/|\\\\)",
                                unique(object@spectraData$dataStorage))))
                      object@spectraData$dataStorage <- .stash_to_absolute_path(
                          object@spectraData$dataStorage, param@path)
              }
              validObject(object)
              object
          })

.MS_BACKEND_MZR_DATA_FILE <- "ms_backend_spectra_data.txt"

################################################################################
##    AlabasterParam
################################################################################

#' @importMethodsFrom alabaster.base saveObject
#'
#' @importFrom alabaster.base saveObjectFile
#'
#' @importFrom alabaster.base altSaveObject
#'
#' @rdname MsBackendMzRStash
setMethod("saveObject", "MsBackendMzR", function(x, path, consolidate = FALSE,
                                                 ...) {
    x <- dropNaSpectraVariables(x)
    dir.create(path, showWarnings = FALSE, recursive = TRUE)
    .check_overwriting(file.path(path, "OBJECT"))
    saveObjectFile(path, "ms_backend_mz_r")
    if (consolidate && nrow(x@spectraData))
        x <- .consolidate_data_storage(x, path)
    altSaveObject(x@spectraData, path = file.path(path, "spectra_data"))
})

#' @importFrom alabaster.base registerValidateObjectFunction
#'
#' @importFrom alabaster.base readObjectFile
#'
#' @noRd
validateMsBackendMzR <- function(path = character(), metadata = list()) {
    .check_directory_content(path, c("OBJECT", "spectra_data"))
    ob <- readObjectFile(path)
    if (ob$type != "ms_backend_mz_r")
        stop("Invalid OBJECT format. Expected \"ms_backend_mz_r\"",
             call. = FALSE)
}

#' @importFrom alabaster.base altReadObject
#'
#' @importFrom alabaster.base registerReadObjectFunction
#'
#' @importFrom Spectra MsBackendMzR
#'
#' @noRd
readMsBackendMzR <- function(path = character(), metadata = list(),
                             spectraPath = character()) {
    validateMsBackendMzR(path, metadata)
    be <- MsBackendMzR()
    be@spectraData <- altReadObject(file.path(path, "spectra_data"))
    if(nrow(be@spectraData)) {
        if (length(spectraPath))
            dataStorageBasePath(be) <- spectraPath
        if (all(grepl("^[.](/|\\\\)", unique(be@spectraData$dataStorage))))
            be@spectraData$dataStorage <- .stash_to_absolute_path(
            be@spectraData$dataStorage, path)
    }
    if (nrow(be@spectraData) &&
        !all(file.exists(unique(be@spectraData$dataStorage))))
        stop("One or more data file(s) missing. Please use parameter ",
             "'spectraPath' if data files have been moved.")
    validObject(be)
    be
}

#' @rdname MsBackendMzRStash
setMethod("saveMsObject", signature(object = "MsBackendMzR",
                                    param = "AlabasterParam"),
          function(object, param, consolidate = FALSE) {
              saveObject(object, path = param@path, consolidate = consolidate)
          })

#' @rdname MsBackendMzRStash
setMethod("readMsObject", signature(object = "MsBackendMzR",
                                    param = "AlabasterParam"),
          function(object, param, spectraPath = character()) {
              readMsBackendMzR(path = param@path, spectraPath = spectraPath)
          })
