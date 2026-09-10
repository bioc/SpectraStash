test_that("saveMsObject/readMsObject,MsBackendHdf5Peaks,PlainTextParam works", {
    d <- file.path(tempdir(), "text_h5")

    p <- PlainTextParam(d)

    ## Empty object
    a <- MsBackendHdf5Peaks()
    saveMsObject(a, p)
    res <- readMsObject(MsBackendHdf5Peaks(), p)
    expect_s4_class(res, "MsBackendHdf5Peaks")
    expect_equal(length(res), length(a))
    unlink(d, recursive = TRUE)

    ## Real object
    expect_no_error(saveMsObject(be_hdf5, p))
    expect_equal(dir(d), "ms_backend_spectra_data.txt")
    res <- readMsObject(MsBackendHdf5Peaks(), p)
    expect_s4_class(res, "MsBackendHdf5Peaks")
    expect_equal(rtime(be_hdf5), rtime(res))
    expect_equal(mz(be_hdf5), mz(res))

    expect_error(saveMsObject(be_hdf5, p), "object stash")

    ## Providing `spectraPath`
    dd <- file.path(tempdir(), "h5_peaks")
    dir.create(dd, showWarnings = FALSE)
    file.copy(unique(be_hdf5$dataStorage),
              to = file.path(dd, basename(unique(be_hdf5$dataStorage))))
    res <- readMsObject(MsBackendHdf5Peaks(), p, spectraPath = dd)
    expect_equal(dataStorageBasePath(res), normalizePath(dd))
    unlink(d, recursive = TRUE)
    unlink(dd, recursive = TRUE)

    ## consolidate
    expect_no_error(saveMsObject(be_hdf5, p, consolidate = TRUE))
    expect_true(
        all(c("ms_backend_spectra_data.txt",
              unique(basename(dataStorage(be_hdf5)))) %in% dir(d)))
    res <- readMsObject(MsBackendHdf5Peaks(), p)
    expect_s4_class(res, "MsBackendHdf5Peaks")
    expect_equal(rtime(be_hdf5), rtime(res))
    expect_equal(mz(be_hdf5), mz(res))
    expect_true(normalizePath(dataStorageBasePath(be_hdf5)) !=
                normalizePath(dataStorageBasePath(res)))

    unlink(d, recursive = TRUE)
})

test_that("alabaster functionality works for MsBackenHdf5Peaks", {
    d <- file.path(tempdir(), "test_hdf5")

    expect_no_error(saveObject(be_hdf5, d))
    expect_true(all(c("OBJECT", "spectra_data", "mod_count") %in% dir(d)))

    expect_no_error(validateMsBackendHdf5Peaks(d))
    res <- readMsBackendHdf5Peaks(d)
    expect_s4_class(res, "MsBackendHdf5Peaks")
    expect_equal(res, dropNaSpectraVariables(be_hdf5))

    saveObjectFile(d, "other_whatever")
    expect_error(validateMsBackendHdf5Peaks(d), "Invalid OBJECT")
    unlink(d, recursive = TRUE)

    ## consolidate = TRUE
    expect_no_error(saveObject(be_hdf5, d, consolidate = TRUE))
    expect_true(all(c("OBJECT", "spectra_data", "mod_count",
                      basename(unique(dataStorage(be_hdf5)))) %in% dir(d)))
    res <- readMsBackendHdf5Peaks(d)
    expect_s4_class(res, "MsBackendHdf5Peaks")
    expect_true(normalizePath(dataStorageBasePath(res)) !=
                normalizePath(dataStorageBasePath(be_hdf5)))
    ## unlink(d, recursive = TRUE)

    ## Removing the h5 files in `d`
    fls <- dir(d, pattern = "h5$", full.names = TRUE)
    unlink(fls)

    expect_error(res <- readMsBackendHdf5Peaks(d), "missing")
    res <- readMsBackendHdf5Peaks(
        d, spectraPath = dataStorageBasePath(be_hdf5))
    expect_s4_class(res, "MsBackendHdf5Peaks")
    expect_true(validObject(res))

    unlink(d, recursive = TRUE)

    ## AlabasterParam
    p <- AlabasterParam(d)
    expect_no_error(saveMsObject(be_hdf5, p))
    res <- readMsObject(MsBackendHdf5Peaks(), p)
    expect_equal(res, dropNaSpectraVariables(be_hdf5))

    expect_error(saveMsObject(be_hdf5, p),
                 "cannot save MsBackendHdf5Peaks at existing")
    unlink(d, recursive = TRUE)

    ## AlabasterParam
    p <- AlabasterParam(d)
    expect_no_error(saveMsObject(be_hdf5, p, consolidate = TRUE))
    expect_true(all(c("OBJECT", "spectra_data", "mod_count",
                      basename(unique(dataStorage(be_hdf5)))) %in% dir(d)))

    res <- readMsObject(MsBackendHdf5Peaks(), p)
    expect_equal(normalizePath(dataStorageBasePath(res)), normalizePath(d))
    unlink(d, recursive = TRUE)
})

test_that("readMsObject,MsBackendHdf5Peaks with consolidate after moving", {
    df <- DataFrame(msLevel = c(1L, 2L, 3L, 1L), rtime = c(1.2, 1.45, 2.5, 2.3))
    df$mz <- list(c(12.2, 124.4, 134.23),
                  sort(abs(rnorm(n = 15))),
                  sort(abs(rnorm(n = 5))),
                  sort(abs(rnorm(n = 143))))
    df$intensity <- list(1:3, 1:15, 1:5, 1:143)
    df$scanIndex <- 1:4
    a <- backendInitialize(MsBackendHdf5Peaks(), data = df,
                           hdf5path = tempdir(), file = "peaks.h5")

    d <- file.path(tempdir(), "test_h5")
    d2 <- file.path(tempdir(), "test_h52")
    saveMsObject(a, AlabasterParam(d), consolidate = TRUE)
    fs::dir_copy(d, d2)
    unlink(d, recursive = TRUE)
    res <- readMsObject(MsBackendHdf5Peaks(), AlabasterParam(d2))
    expect_true(validObject(res))
    expect_equal(res$mz, a$mz)

    unlink(d2, recursive = TRUE)
    unlink(file.path(tempdir(), "peaks.h5"))
})
