#' @title Stash for backends for data from public repositories
#'
#' @name MsBackendMetablLightsStash
#'
#' @description
#'
#' Backend classes for public repository mass spectrometry data, such as
#' `MsBackendMetaboLights` from the
#' [*MsBackendMetaboLights*](https://bioconductor.org/packages/MsBackendMetaboLights)
#' package can be stored and restored from stash formats extending the
#' [MsBackendMzRStash]. Backends can be saved with [saveMsObject()] and restored
#' with [readMsObject()]. At present only the *alabaster*-based stash format is
#' supported.
#'
#' @note
#'
#' `saveMsObject()` for `MsBackendMetaboLights` does not support (and does not
#' require) the `consilidate` parameter that most other backends implement to
#' create portable stashes. `MsBackendMetaboLights` backends retrieve and cache
#' the mass spectrometry data files if needed from the public repository and
#' are thus inherently portable.
#'
#' @section Alabaster format, `AlabasterParam`:
#'
#' Calling `saveMsObject()` on a `MsBackendMetaboLights` with an
#' `AlabasterParam` parameter object stores the backend data in a directory
#' specified with parameter `path` of the parameter object. The stash folder
#' content is identical to the [MsBackendMzRStash] with only the class
#' definition of the *OBJECT* file being specific for the stored backend class.
#'
#' Restoring a `MsBackendMetaboLights` from the stash folder imports the
#' metadata and, if not available in the local cache, downloads the mass
#' spectrometry data files from the MetaboLights repository. Synchronizing
#' the locally cached data requires an active internet connection, even if no
#' data needs to be downloaded. Setting parameter `offline = TRUE` avoids the
#' synchronization, but requires the mass spectrometry data files to be
#' present in the local cache.
#'
#' The alabaster-format stashes can also be created and restored using the
#' `saveObject()` and `readObject()` functions.
#'
#' @param object An `MsBackendMetaboLights` object.
#'
#' @param offline For `readMsObject()` and `readObject()`: `logical(1)` if
#'     online lookup and synchronization of the mass spectrometry data files
#'     should be disabled. Defaults to `offline = FALSE`. If set to `TRUE` all
#'     mass spectrometry data files need to be already present in the local
#'     cache.
#'
#' @param param An `AlabasterParam` object.
#'
#' @param path For `saveObject()` and `readObject()`
#'
#' @param x For `saveObject()`: a `MsBackendMetaboLights` object.
#'
#' @param ... Ignored.
#'
#' @return `readObject()` and `readMsObject()` return a `MsBackendMetaboLights`
#'     backend.
#'
#' @author Johannes Rainer
#'
#' @examples
#'
#' library(Spectra)
#' library(SpectraStash)
#' library(MsBackendMetaboLights)
#'
#' ## Retrieve mass spectrometry data files matching a specified file pattern
#' ## for the data set MTBLS39 from MetaboLights
#' s <- Spectra(mtblsId = "MTBLS39", source = MsBackendMetaboLights(),
#'     filePattern = "73A.cdf")
#'
#' ## Stash the `Spectra` object to a temporary folder.
#' d <- file.path(tempdir(), "metabolights_stash")
#' saveMsObject(s, AlabasterParam(d))
#'
#' ## List the content of the SpectraStash
#' dir(d)
#'
#' ## List the content of the backend stash (located within the *backend*
#' ## sub-folder)
#' dir(file.path(d, "backend"))
#'
#' ## Restore the `Spectra` object (with the respective backend); using
#' ## `offline = TRUE` to avoid the lookup of the data set on the MetaboLights
#' ## repository - all mass spectrometry data files of the backend must then be
#' ## already present in the local cache.
#' res <- readMsObject(Spectra(), AlabasterParam(d), offline = TRUE)
#' res
NULL

################################################################################
##    MsBackendMetaboLights,AlabasterParam
################################################################################

#' @rdname MsBackendMetablLightsStash
#'
#' @importFrom methods callNextMethod
setMethod("saveObject", "MsBackendMetaboLights", function(x, path,
                                                          ...) {
    l <- list(...)
    if (any(names(l) %in% "consolidate"))
        message("Parameter 'consolidate' is not supported for ",
                "'MsBackendMetaboLights' and therefore ignored")
    callNextMethod()
    saveObjectFile(path, "ms_backend_metabo_lights")
})

validateMsBackendMetaboLights <- function(path = character(),
                                          metadata = list()) {
    .check_directory_content(path, c("OBJECT", "spectra_data"))
    ob <- readObjectFile(path)
    if (ob$type != "ms_backend_metabo_lights")
        stop("Invalid OBJECT format. Expected \"ms_backend_metabo_lights\"",
             call. = FALSE)
}

readMsBackendMetaboLights <- function(path = character(),
                                      metadata = list(), offline = FALSE, ...) {
    if (!.is_ms_backend_metabo_lights_installed())
        stop("Required package 'MsBackendMetaboLights' missing. ",
             "Please install and try again.", call. = FALSE)
    validateMsBackendMetaboLights(path)
    be <- MsBackendMetaboLights::MsBackendMetaboLights()
    be@spectraData <- altReadObject(file.path(path, "spectra_data"))
    MsBackendMetaboLights::mtbls_sync(be, offline = offline)
}

#' @rdname MsBackendMetablLightsStash
setMethod("saveMsObject", signature(object = "MsBackendMetaboLights",
                                    param = "AlabasterParam"),
          function(object, param, ...) {
              saveObject(object, path = param@path, ...)
          })

#' @rdname MsBackendMetablLightsStash
setMethod("readMsObject", signature(object = "MsBackendMetaboLights",
                                    param = "AlabasterParam"),
          function(object, param, offline = FALSE, ...) {
              readMsBackendMetaboLights(path = param@path,
                                        offline = offline, ...)
          })

.is_ms_backend_metabo_lights_installed <- function() {
    requireNamespace("MsBackendMetaboLights", quietly = TRUE)
}
