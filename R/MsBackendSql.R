## Stashing for backends defined in the *MsBackendSql* package

#' @title Stash for SQL-based backends
#'
#' @name MsBackendSqlStash
#'
#' @description
#'
#' `MsBackendOfflineSql` and `MsBackendSql` backend classes defined in the
#' [*MsBackendSql*](https://bioconductor.org/packages/MsBackendSql)
#' Bioconductor package can be stashed (or read from) plain text file or
#' *alabaster*-based stash formats using the [saveMsObject()] and
#' [readMsObject()] functions configuring the stash with a [PlainTextParam] or
#' [AlabasterParam] parameter object. The Alabaster format is the preferred
#' stash format.
#'
#' Additional properties of the stash formats are described in detail in the
#' sections below.
#'
#' @note
#'
#' For some SQL database systems it is possible to store also the database into
#' the stash folder (by setting `consolidate = TRUE` in the `saveMsObject()`
#' call). This is currently supported for:
#'
#' - SQLite-based databases (using the *RSQLite* package)
#'
#' `MsBackendSql` backends are converted to a `MsBackendOfflineSql` before
#' saving into the stash, because database connections can not be serialized.
#' Thus, both classes use the same *MsBackendSqlStash* format (and both can
#' hence be restored from the same stash).
#'
#' @section Alabaster format, `AlabasterParam`:
#'
#' `saveMsObject()` with an `AlabasterParam()` stores the backend to a stash
#' folder in *alabaster* format. Individual components of the backend are
#' stored using base functionality of the *alabaster.base* Bioconductor package
#' into specific sub-folders within the main stash directory. These are:
#'
#' - *dbinfo* directory: contains a `data.frame` saved with *alabaster.base*
#'   with the database-specific information such as database name, host etc.
#'
#' - *spectra_ids* directory: contains an `integer` vector saved with
#'   *alabaster.base* with the IDs (indices) of the individual spectra.
#'
#' The remaining directories hold the content of the `MsBackendCached` object
#' the `MsBackendOfflineSql` extends (i.e., all locally cached data). Using
#' parameter `consolidate = TRUE` will also store the database file within
#' the folder (if supported).
#'
#' In addition to the `saveMsObject()` and `readMsObject()` also the
#' *alabaster.base* `saveObject()` and `readObject()` methods are directly
#' supported.
#'
#' @section Text-file format, `PlainTextParam`:
#'
#' `saveMsObject()` with a `PlainTextParam` stores the content of a
#' `MsBackendOfflineSql` as text files into the stash folder. This creates
#' two text files:
#'
#' - *ms_backend_dbinfo.txt*: a two-column tabulator delimited text file with
#'   columns `"key"` and `"value"` (first line being the header) with each
#'   row representing a key-value pair with the SQL database connection
#'   information. This includes `"dbname"` (the name of the database - for
#'   SQLite databases the SQLite database file name), `"dbdriver"` the database
#'   driver class (e.g. `"RSQLite::SQLiteDriver"` for SQLite databases),
#'   `"user"` with the database user, `"host"` with the host running the
#'   database and `"port"` with the respective port.
#'
#' - *ms_backend_spectra_ids.txt*: a single column text file (first row being
#'   the *header* of the table) with the spectrum identifiers (of
#'   type `integer`).
#'
#' Remaining content, such as eventually present locally cached data, are
#' stored through the `saveMsObject()` function of the parent `MsBackendCached`
#' object (see [MsBackendCachedStash] for details).
#'
#' @param consolidate `logical(1)` whether the database file should be copied
#'     into the stash folder. This is currently only supported for some
#'     database formats and systems. See *notes* section for details.
#'
#' @param object An `MsBackendOfflineSql` or `MsBackendSql` object.
#'
#' @param param Either a `PlainTextParam` or `AlabasterParam`.
#'
#' @param password Optional `character(1)` with the password required to
#'     connect to the database. Passwords are never stored in a stash, thus,
#'     if required, the password to connect to the SQL database must be
#'     provided through this parameter.
#'
#' @param path For `saveObject()` and `readObject()`: the path to the stash
#'     directory.
#'
#' @param x For `saveObject()`: a `MsBackendOfflineSql`.
#'
#' @param ... Currently ignored.
#'
#' @return `readMsObject()` returns a [MsBackendSql::MsBackendOfflineSql]
#'     object or [MsBackendSql::MsBackendSql] object.
#'
#' @author Johannes Rainer
#'
#' @examples
#'
#' ## Create an example `Spectra` object
#' library(Spectra)
#' library(MsDataHub)
#'
#' s <- Spectra(MsDataHub::X20171016_POOL_POS_1_105.134.mzML())
#'
#' ## Store the full data into a SQLite SQL database
#' library(RSQLite)
#' library(MsBackendSql)
#' dbname <- tempfile()
#' s <- setBackend(s, MsBackendOfflineSql(), dbname = dbname, drv = SQLite())
#'
#' ## Stash the `Spectra` object to a temporary folder in alabaster format
#' d <- file.path(tempdir(), "spectra_sql")
#' saveMsObject(s, AlabasterParam(d))
#'
#' ## List the content of the SpectraStash
#' dir(d)
#'
#' ## List the content of the MsBackendOfflineSql stash, which is located
#' ## within the *backend* folder
#' dir(file.path(d, "backend"))
#'
#' ## Restore the object from the stash
#' res <- readMsObject(Spectra(), AlabasterParam(d))
#' res
#'
#' ## Verify a SQL backend is used
#' class(res@backend)
#'
#' ## Repeat using the plain text file format. We need to first remove
#' ## the previously generated stash, as overwriting an existing stash
#' ## is not supported.
#' unlink(d, recursive = TRUE)
#'
#' ## Use `consolidate = TRUE` to store also the database into the stash
#' saveMsObject(s, PlainTextParam(d), consolidate = TRUE)
#'
#' ## List the content of the SpectraStash
#' dir(d)
#'
#' ## Restore the object from the stash
#' res <- readMsObject(Spectra(), PlainTextParam(d))
#' res
#'
#' ## Verify a SQL backend is used
#' class(res@backend)
NULL

################################################################################
##    PlainTextParam
################################################################################

#' @rdname MsBackendSqlStash
#'
#' @importFrom methods getMethod signature
setMethod("saveMsObject", signature(object = "MsBackendOfflineSql",
                                    param = "PlainTextParam"),
          function(object, param, consolidate = FALSE, ...) {
              ## Can not use callNextMethod() because of inheritance of
              ## MsBackendSql and MsBackendOfflineSql
              FUN <- getMethod("saveMsObject",
                               signature(object = "MsBackendCached",
                                         param = "PlainTextParam"))
              FUN(object, param, consolidate = consolidate, ...)
              if (consolidate)
                  object <- .consolidate_database(object, param@path)
              write.table(.dbinfo(object),
                          file.path(param@path, "ms_backend_dbinfo.txt"),
                          quote = TRUE, sep = "\t", row.names = FALSE)
              write.table(data.frame(spectra_id = object@spectraIds),
                          file.path(param@path, "ms_backend_spectra_ids.txt"),
                          sep = "\t", row.names = FALSE)
          })

#' @rdname MsBackendSqlStash
#'
#' @importFrom methods slot<-
setMethod("readMsObject", signature(object = "MsBackendOfflineSql",
                                    param = "PlainTextParam"),
          function(object, param, password = character(), ...) {
              ## Can not use callNextMethod() because of inheritance of
              ## MsBackendSql and MsBackendOfflineSql
              FUN <- getMethod("readMsObject",
                               signature(object = "MsBackendCached",
                                         param = "PlainTextParam"))
              parent <- FUN(object, param, password = password, ...)
              if (file.exists(file.path(param@path, "ms_backend_dbinfo.txt"))) {
                  dbi <- read.table(
                      file.path(param@path, "ms_backend_dbinfo.txt"),
                      header = TRUE, sep = "\t")
                  ## Initialize backend from db to adapt to db content
                  object <- .initialize_ms_backend_offline_sql(
                      object, dbi, param@path, password = password)
                  f <- file.path(param@path, "ms_backend_spectra_ids.txt")
                  if (!file.exists(f))
                      stop("Required file \"ms_backend_spectra_ids.txt\" not",
                           " found in the provided path.", call. = FALSE)
                  si <- as.integer(read.table(f, header = TRUE)[, 1L])
                  slot(object, "spectraIds", check = FALSE) <- si
                  ## Complete with cached content
                  slot(object, "localData", check = FALSE) <- parent@localData
                  slot(object, "nspectra", check = FALSE) <- parent@nspectra
                  slot(object, "spectraVariables", check = FALSE) <-
                      parent@spectraVariables
              } else stop("Required file \"ms_backend_dbinfo.txt\" not found ",
                          "in the provided path.", call. = FALSE)
              validObject(object)
              object
          })

#' @rdname MsBackendSqlStash
setMethod("saveMsObject", signature(object = "MsBackendSql",
                                    param = "PlainTextParam"),
          function(object, param, consolidate = FALSE, ...) {
              FUN <- getMethod("saveMsObject",
                               signature(object = "MsBackendOfflineSql",
                                         param = "PlainTextParam"))
              FUN(.go_offline(object), param, consolidate = consolidate)
          })

#' @rdname MsBackendSqlStash
setMethod("readMsObject", signature(object = "MsBackendSql",
                                    param = "PlainTextParam"),
          function(object, param, password = character()) {
              FUN <- getMethod("readMsObject",
                               signature(object = "MsBackendOfflineSql",
                                         param = "PlainTextParam"))
              object <- FUN(MsBackendSql::MsBackendOfflineSql(),
                            param, password = password)
              object <- .go_online(object)
              validObject(object)
              object
          })

################################################################################
##    AlabasterParam
################################################################################

#' @rdname MsBackendSqlStash
#'
#' @importFrom alabaster.base readObjectFile
#'
#' @importFrom methods as
setMethod("saveObject", "MsBackendOfflineSql", function(x, path,
                                                        consolidate = FALSE,
                                                        ...) {
    altSaveObject(as(x, "MsBackendCached"), path, ...)
    info <- readObjectFile(path)
    info$ms_backend_offline_sql <- list(version = "1.0")
    saveObjectFile(path, "ms_backend_offline_sql", info)
    if (consolidate)
        x <- .consolidate_database(x, path)
    altSaveObject(.dbinfo(x), path = file.path(path, "dbinfo"))
    altSaveObject(x@spectraIds, path = file.path(path, "spectra_ids"))
})

validateAlabasterMsBackendOfflineSql <- function(path = character(),
                                                 metadata = list()) {
    validateMsBackendCached(path)
    .check_directory_content(path, c("spectra_ids", "dbinfo"))
}

readAlabasterMsBackendOfflineSql <- function(path = character(),
                                             metadata = list(),
                                             password = character()) {
    if (!.is_ms_backend_sql_installed())
        stop("Required package 'MsBackendSql' missing. Please install ",
             "and try again.", call. = FALSE)
    validateAlabasterMsBackendOfflineSql(path, metadata)
    dbi <- altReadObject(file.path(path, "dbinfo"))
    object <- .initialize_ms_backend_offline_sql(
        MsBackendSql::MsBackendOfflineSql(), dbi, path, password = password)
    slot(object, "spectraIds", check = FALSE) <- altReadObject(
        file.path(path, "spectra_ids"))
    metadata$type <- "ms_backend_cached"
    parent <- altReadObject(path, metadata = metadata)
    slot(object, "localData", check = FALSE) <- parent@localData
    slot(object, "nspectra", check = FALSE) <- parent@nspectra
    slot(object, "spectraVariables", check = FALSE) <- parent@spectraVariables
    validObject(object)
    object
}

#' @rdname MsBackendSqlStash
setMethod("saveMsObject", signature(object = "MsBackendOfflineSql",
                                    param = "AlabasterParam"),
          function(object, param, consolidate = FALSE, ...) {
              saveObject(object, param@path, consolidate = consolidate, ...)
          })

#' @rdname MsBackendSqlStash
setMethod("readMsObject", signature(object = "MsBackendOfflineSql",
                                    param = "AlabasterParam"),
          function(object, param, password = character()) {
              readAlabasterMsBackendOfflineSql(path = param@path,
                                               password = password)
          })

#' @rdname MsBackendSqlStash
setMethod("saveObject", "MsBackendSql", function(x, path,
                                                 consolidate = FALSE,
                                                 ...) {
    x <- .go_offline(x)
    altSaveObject(as(x, "MsBackendCached"), path, ...)
    info <- readObjectFile(path)
    info$ms_backend_sql <- list(version = "1.0")
    saveObjectFile(path, "ms_backend_sql", info)
    if (consolidate)
        x <- .consolidate_database(x, path)
    altSaveObject(.dbinfo(x), path = file.path(path, "dbinfo"))
    altSaveObject(x@spectraIds, path = file.path(path, "spectra_ids"))
})

readAlabasterMsBackendSql <- function(path = character(),
                                      metadata = list(),
                                      password = character()) {
    object <- readAlabasterMsBackendOfflineSql(path, metadata = metadata,
                                               password = password)
    object <- .go_online(object)
    validObject(object)
    object
}

#' @rdname MsBackendSqlStash
setMethod("saveMsObject", signature(object = "MsBackendSql",
                                    param = "AlabasterParam"),
          function(object, param, consolidate = FALSE, ...) {
              saveObject(object, param@path, consolidate = consolidate, ...)
          })

#' @rdname MsBackendSqlStash
setMethod("readMsObject", signature(object = "MsBackendSql",
                                    param = "AlabasterParam"),
          function(object, param, password = character()) {
              readAlabasterMsBackendOfflineSql(path = param@path,
                                               password = password)
          })

################################################################################
##    Utility functions
################################################################################

.is_ms_backend_sql_installed <- function() {
    requireNamespace("MsBackendSql", quietly = TRUE)
}

#' helper function to convert a `MsBackendSql` to a `MsBackendOfflineSql` if
#' supported.
#'
#' @param x `MsBackendSql` object.
#'
#' @return `MsBackendOfflineSql`
#'
#' @noRd
.go_offline <- function(x) {
    if (!.offline_supported(x))
        stop("Changing to offline-mode is currently not supported for ",
             "database connections of type: ", class(x@dbcon)[1L],
             call. = FALSE)
    dbi <- DBI::dbGetInfo(x@dbcon)
    drv <- strsplit(.class_to_driver(class(x@dbcon)[1L]), "::",
                    fixed = TRUE)[[1L]]
    x <- as(x, "MsBackendOfflineSql")
    requireNamespace(drv[1L])
    x@driver <- do.call(drv[2L], list())
    x@dbname <- dbi$dbname
    x@user <- as.character(dbi$username)
    x@host <- as.character(dbi$host)
    x@port <- as.integer(dbi$port)
    ## DBI::dbDisconnect(x@dbcon)
    x@dbcon <- NULL
    x
}

#' Convert a `MsBackendOfflineSql` to a `MsBackendSql`.
#'
#' @param x `MsBackendOfflineSql`
#'
#' @return `MsBackendSql`
#'
#' @noRd
.go_online <- function(x) {
    con <- MsBackendSql:::.db_connect(x)
    x <- as(x, "MsBackendSql")
    x@dbcon <- con
    x
}

#' Check if the database would support switching to *offline mode*. This mostly
#' depends on what we have implemented so far and less on the database system
#' used as most should support this, but we might need password etc which we
#' might not have.
#'
#' @noRd
.offline_supported <- function(x) {
    class(x@dbcon)[1L] %in% c("SQLiteConnection")
}

#' Map database connection class to the respective connection driver we could
#' use. Needs to manually expanded for new supported database systems.
#'
#' @noRd
.class_to_driver <- function(x) {
    DRV <- c(SQLiteConnection = "RSQLite::SQLite")
    DRV[x]
}

#' Return the database information as a `data.frame` from a
#' `MsBackendOfflineSql`.
#'
#' @note
#'
#' We **don't** extract the password as we will **not** save that in plain
#' text into the stash. The password, if set/required, needs to be provided
#' when reading the backend from the stash.
#'
#' @param x `MsBackendOfflineSql`
#'
#' @return `data.frame` with columns `"key"` and `"value"` with the keys
#'     being `"dbname"`, `"user"`, `"host"`, `"port"` and `"dbdriver"`.
#'
#' @noRd
.dbinfo <- function(x) {
    data.frame(
        key = c("dbname", "dbdriver", "user", "host", "port"),
        value = c(ifelse(length(x@dbname), x@dbname, ""),
                  .sql_driver_class(x@driver),
                  ifelse(length(x@user), x@user, ""),
                  ifelse(length(x@host), x@host, ""),
                  ifelse(length(x@port), x@port, NA_character_)))
}

#' Helper function to check that the information provided in a *dbinfo*
#' `data.frame` is correct, i.e., that all required fields are present:
#' - we expect two columns named `"key"` and `"value"`
#' - values in column `"key"` must contain `"dbname"`, `"dbdriver"`, `"user"`,
#'   `"host"` and `"port"`.
#'
#' @param x `data.frame` such as created by `.dbinfo()` or read from the stash
#'
#' @return The function throws an error or returns `TRUE` invisibly.
#'
#' @noRd
.valid_dbinfo <- function(x) {
    if (!all(c("value", "key") %in% colnames(x)))
        stop("[MsBackendOfflineSql] missing database information: required ",
             "column names \"key\" and \"value\" not present.", call. = FALSE)
    if (length(miss <- setdiff(c("dbname", "dbdriver",
                                 "user", "host", "port"), x$key))) {
        msg <- paste0("\"", miss, "\"", collapse = ", ")
        stop("[MsBackendOfflineSql] missing database information: key(s) ",
             msg, " not found.", call. = FALSE)
    }
    invisible(TRUE)
}

#' Helper to get the name of the SQL database driver (including package) in the
#' form <package>::<driver>
#'
#' @noRd
.sql_driver_class <- function(x) {
    cl <- class(x)
    if (cl != "NULL")
        cl <- paste0(attr(cl, "package"), "::", cl[1L])
    else cl <- ""
    cl
}

#' Save the database file into the stash, if it is supported. Check based on
#' the database driver class if it would/could be supported. The function
#' updates the `@dbname` to a *local* path if consolidation is supported and
#' the file can be copied. At present we support:
#'
#' - SQLite database files (`SQLiteDriver`)
#'
#' @param x `MsBackendOfflineSql` object.
#'
#' @param path `character(1)` with the path where to store the database.
#'
#' @return the input object `x` with an eventually updated `@dbname`.
#'
#' @author Johannes Rainer
#'
#' @noRd
.consolidate_database <- function(x, path) {
    if (class(x@driver)[1L] %in% c("SQLiteDriver")) {
        if (file.copy(x@dbname, file.path(path, basename(x@dbname))))
            x@dbname <- file.path(".", basename(x@dbname))
    } else
        warning("Database can not be stored within the stash. ",
                "Setting 'consolidate' to 'FALSE'.")
    x
}

#' @description
#'
#' Helper function to initialize a `MsBackendOfflineSql` backend based on the
#' *dbi* `data.frame` with database connection information. The function returns
#' an (initialized) instance of `MsBackendOfflineSql`. This backend must
#' then be completed adding cached information.
#'
#' If the database name information `"dbname"` in `dbi` indicates the database
#' file to be located within the stash (i.e., it's name starting with `"./"`)
#' the value of parameter `path` (the path to the stash directory) is used to
#' expand and complete this path.
#'
#' @note
#'
#' The call to `backendInitialize()` is required to ensure slots `@.tables`
#' and `@peak_fun` are correctly initialized base on the **content** of the
#' database.
#'
#' @param object `MsBackendOfflineSql`
#'
#' @param dbi `data.frame` with columns `"key"` and `"value"` containing the
#'     connection information for the database. This is imported from the
#'     stash or created with the `.dbinfo()` function and is validated with
#'     the `.valid_dbinfo()` function.
#'
#' @param path `character(1)` with the path to the stash. This is used to
#'     update the `"dbname"` in case the database was stored within the
#'     stash (through `consolidate = TRUE`).
#'
#' @param password `character(1)` with the password to connect to the database,
#'     if required.
#'
#' @return an initialized `MsBackendOfflineSql` with all database-specific
#'     information set. To completely restore a stashed `MsBackendOfflineSql`
#'     also the cached content needs to be added.
#'
#' @importFrom methods new
#'
#' @noRd
.initialize_ms_backend_offline_sql <- function(object, dbi, path,
                                               password = character()) {
    .valid_dbinfo(dbi)
    dbi <- .dbinfo_update_path(dbi, path = path)
    user <- dbi$value[dbi$key == "user"]
    if (is.na(user) || user == "") user <- character()
    host <- dbi$value[dbi$key == "host"]
    if (is.na(host) || host == "") host <- character()
    port <- as.integer(dbi$value[dbi$key == "port"])
    dbname <- dbi$value[dbi$key == "dbname"]
    if (is.na(dbname) || dbname == "") dbname <- character()
    drv <- dbi$value[dbi$key == "dbdriver"]
    if (is.na(drv) || drv == "") drv <- NULL
    else {
        drv <- strsplit(drv, "::", fixed = TRUE)[[1L]]
        requireNamespace(drv[1L])
        drv <- new(drv[2L])
        object <- backendInitialize(
            object, drv = drv, dbname = dbname, user = user,
            password = password, host = host, port = port)
    }
    object
}

#' Expand a stash-local path of the database file with the path to the stash
#' if needed (i.e., if the database name starts with `"./"`).
#'
#' @param dbi `data.frame` with database connection information
#'
#' @param path `character(1)` with the path to the stash.
#'
#' @return `data.frame` with the `"dbname"` entry updated (if required).
#'
#' @noRd
.dbinfo_update_path <- function(dbi, path) {
    f <- dbi$value[dbi$key == "dbname"]
    if (grepl("^[.](/|\\\\)", f))
        dbi$value[dbi$key == "dbname"] <- .stash_to_absolute_path(f, path)
    dbi
}
