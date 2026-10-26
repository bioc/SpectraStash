## Tests to stash backends for public repository data.

library(MsBackendMetaboLights)
mtblsId <- "MTBLS39"
be_mtbls <- backendInitialize(MsBackendMetaboLights(), mtblsId = "MTBLS39",
                              filePattern = "73A.cdf")

test_that(".is_ms_backend_metabo_lights_installed works", {
    expect_true(.is_ms_backend_metabo_lights_installed())
})

test_that("alabaster stash for MsBackendMetaboLights works", {
    d <- file.path(tempdir(), "mtbls_stash")
    expect_no_error(saveObject(be_mtbls, d))
    expect_true(all(c("OBJECT", "spectra_data") %in% dir(d)))
    expect_equal(readObjectFile(d)$type, "ms_backend_metabo_lights")

    res <- readObject(d)
    expect_s4_class(res, "MsBackendMetaboLights")
    expect_true(validObject(res))

    ## delete cache and try again.
    mtbls_delete_cache(mtblsId)
    res <- readObject(d)
    expect_s4_class(res, "MsBackendMetaboLights")
    expect_true(validObject(res))
    unlink(d, recursive = TRUE)

    expect_message(
        saveMsObject(be_mtbls, AlabasterParam(d), consolidate = TRUE),"ignored")
    expect_true(all(c("OBJECT", "spectra_data") %in% dir(d)))
    expect_equal(readObjectFile(d)$type, "ms_backend_metabo_lights")
    res <- readMsObject(MsBackendMetaboLights(), AlabasterParam(d),
                        offline = TRUE)
    expect_s4_class(res, "MsBackendMetaboLights")
    expect_true(validObject(res))

    ## What happens if we read the stash with MsBackendMzR?
    expect_error(readMsObject(MsBackendMzR(), AlabasterParam(d)), "Invalid")
    unlink(d, recursive = TRUE)

    ## What happens if we read a MsBackendMzR stash with MsBackendMetaboLights?
    tmp <- as(be_mtbls, "MsBackendMzR")
    saveMsObject(tmp, AlabasterParam(d))
    expect_error(readMsObject(MsBackendMetaboLights(), AlabasterParam(d)),
                 "Invalid")
    unlink(d, recursive = TRUE)
})
