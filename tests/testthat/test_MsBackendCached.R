test_that("saveMsObject/readMsObject,MsBackendCached,PlainTextParam works", {
    d <- file.path(tempdir(), "test_cached")

    ## Empty object
    a <- MsBackendCached()
    dir.create(d, showWarnings = FALSE)
    ptp <- PlainTextParam(d)
    saveMsObject(a, ptp)
    expect_true(all(c("ms_backend_data.txt", "ms_backend_spectra_variables.txt",
                      "ms_backend_nspectra.txt") %in% dir(d)))
    expect_equal(readLines(file.path(d, "ms_backend_nspectra.txt")), "0")
    expect_equal(readLines(file.path(d, "ms_backend_data.txt"), n = 1),
                 "# MsBackendCached")
    expect_equal(readLines(file.path(d, "ms_backend_spectra_variables.txt")),"")
    ## read
    b <- readMsObject(MsBackendCached(), ptp)
    expect_s4_class(b, "MsBackendCached")
    expect_true(validObject(b))
    expect_equal(a, b)
    unlink(d, recursive = TRUE)

    ############################################################################
    ##    Real object
    a <- backendInitialize(a, data = data.frame(opt = 1:10, b = "a"),
                           spectraVariables = c("msLevel", "rtime", "opt", "a"),
                           nspectra = 10)
    saveMsObject(a, ptp)
    expect_true(all(c("ms_backend_data.txt", "ms_backend_spectra_variables.txt",
                      "ms_backend_nspectra.txt") %in% dir(d)))
    expect_equal(readLines(file.path(d, "ms_backend_nspectra.txt")), "10")
    expect_equal(readLines(file.path(d, "ms_backend_data.txt"), n = 1),
                 "# MsBackendCached")
    res <- read.table(file.path(d, "ms_backend_data.txt"), sep = "\t",
                      header = TRUE)
    rownames(res) <- NULL
    expect_equal(res, a@localData)
    res <- readLines(file.path(d, "ms_backend_spectra_variables.txt"))
    expect_equal(strsplit(res, "\t")[[1L]], a@spectraVariables)
    ## read
    b <- readMsObject(MsBackendCached(), ptp)
    expect_s4_class(b, "MsBackendCached")
    expect_true(validObject(b))
    expect_equal(a, b)

    ############################################################################
    ##    Errors
    expect_error(saveMsObject(a, ptp), "contains already an MS object stash")
    l <- readLines(file.path(d, "ms_backend_data.txt"))
    l[1] <- "# MsBackendOther"
    writeLines(l, con = file.path(d, "ms_backend_data.txt"))
    expect_error(readMsObject(MsBackendCached(), ptp), "MsBackendOther")
    unlink(file.path(d, "ms_backend_data.txt"))
    expect_error(readMsObject(MsBackendCached(), ptp), "in the path")
    unlink(d, recursive = TRUE)
})

test_that("saveMsObject/readMsObject,MsBackendCached,AlabasterParam works", {
    d <- file.path(tempdir(), "test_cached")

    ## Empty object
    a <- MsBackendCached()
    expect_no_error(saveObject(a, d))
    expect_true(all(c("local_data", "nspectra", "OBJECT", "spectra_variables")
                    %in% dir(d)))
    ## read
    b <- readObject(d)
    expect_s4_class(b, "MsBackendCached")
    expect_true(validObject(b))
    expect_equal(a, b)
    unlink(d, recursive = TRUE)

    ############################################################################
    ##    Real object
    a <- backendInitialize(a, data = data.frame(opt = 1:10, b = "a"),
                           spectraVariables = c("msLevel", "rtime", "opt", "a"),
                           nspectra = 10)
    ap <- AlabasterParam(d)
    expect_no_error(saveMsObject(a, ap))
    expect_true(all(c("local_data", "nspectra", "OBJECT", "spectra_variables")
                    %in% dir(d)))
    expect_equal(readObjectFile(d)$type, "ms_backend_cached")
    ## read
    b <- readMsObject(MsBackendCached(), ap)
    expect_s4_class(b, "MsBackendCached")
    expect_true(validObject(b))
    expect_equal(a, b)

    ############################################################################
    ##    Errors
    expect_error(saveObject(a, d), "cannot save MsBackendCached")
    unlink(d, recursive = TRUE)
})

test_that("save methods fail for MsBackendCached child classes", {
    ## These tests ensure that any backend extending `MsBackendCached` would
    ## throw an error if it does not implement their own `saveMsObject()`/
    ## `saveObject()` method and only the one from `MsBackendCached` is called.
    setClass("CacheA",
             contains = "MsBackendCached",
             slots = c(something = "numeric"),
             prototype = prototype(something = numeric()))
    a <- backendInitialize(new("CacheA"), nspectra = 10)
    a@something <- c(1.2, 1.4)

    expect_no_error(.valid_upstream_call(
        MsBackendCached(), signature = c("MsBackendCached", "AlabasterParam")))
    expect_error(.valid_upstream_call(
        a, "saveMsObject", signature = c("CacheA", "AlabasterParam")),
        "No method 'saveMsObject'")
    ## Define method
    setMethod("saveMsObject",
              signature(object = "CacheA", param = "AlabasterParam"),
              function(object, param, ...) {
                  callNextMethod()
              })
    expect_no_error(.valid_upstream_call(
        a, "saveMsObject", signature = c("CacheA", "AlabasterParam")))
    ## check also saveObject!
    expect_no_error(.valid_upstream_call(
        MsBackendCached(), "saveObject", "MsBackendCached"))
    expect_error(.valid_upstream_call(a, "saveObject", "MsBackendCached"),
                 "No method 'saveObject'")

    ## check the actual method calls.
    d <- file.path(tempdir(), "cache_test")
    ## PlainTextParam
    p <- PlainTextParam(d)
    expect_error(saveMsObject(a, p), "No method 'saveMsObject' available")
    setMethod("saveMsObject",
              signature(object = "CacheA", param = "PlainTextParam"),
              function(object, param, ...) {
                  callNextMethod()
                  message("saveMsObject,CacheA,PlainTextParam")
              })
    expect_no_error(expect_message(saveMsObject(a, p), "saveMsObject,CacheA"))
    unlink(d, recursive = TRUE)

    ## AlabasterParam; we defined a method already above; why are we then
    ## calling the PlainTextParam one???
    p <- AlabasterParam(d)
    expect_no_error(saveMsObject(a, p))
    unlink(d, recursive = TRUE)

    ## saveObject
    expect_error(saveObject(a, d), "No method 'saveObject' available")
    setMethod("saveObject", "CacheA", function(x, path, ...) {
        callNextMethod()
        message("saveObject,CacheA")
    })
    expect_no_error(expect_message(saveObject(a, d), "saveObject,CacheA"))
    unlink(d, recursive = TRUE)
})
