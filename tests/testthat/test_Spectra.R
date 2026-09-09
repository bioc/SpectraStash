test_that("saveMsObject/readMsObject,PlainTextParam works", {
    d <- file.path(tempdir(), "spectra_tests")
    ptp <- PlainTextParam(d)
    a <- Spectra()

    setClass("DummyBackend", contains = "MsBackend")
    a@backend <- new("DummyBackend")
    expect_error(saveMsObject(a, ptp), "with backend")

    ## Empty MsBackendMemory
    a <- Spectra()
    saveMsObject(a, ptp)
    b <- readMsObject(Spectra(), ptp)
    expect_s4_class(b, "Spectra")
    expect_s4_class(b@backend, "MsBackendMemory")

    unlink(d, recursive = TRUE)

    a <- Spectra(be_mzr)
    expect_no_error(saveMsObject(a, ptp))
    expect_true(all(c("ms_backend_spectra_data.txt", "spectra_slots.txt",
                      "spectra_processing_queue.json") %in% dir(d)))
    res <- readMsObject(Spectra(), ptp)
    expect_s4_class(res, "Spectra")
    expect_s4_class(res@backend, "MsBackendMzR")
    expect_equal(rtime(res), rtime(a))
    expect_equal(mz(res), mz(a))
    unlink(d, recursive = TRUE)

    a <- filterRt(a, c(200, 400))
    a <- filterMzRange(a, c(300, 600))
    expect_no_error(saveMsObject(a, ptp))
    expect_true(all(c("ms_backend_spectra_data.txt", "spectra_slots.txt",
                      "spectra_processing_queue.json") %in% dir(d)))
    res <- readMsObject(Spectra(), ptp)
    expect_equal(a@processing, res@processing)
    expect_equal(a@processingQueueVariables, res@processingQueueVariables)
    expect_equal(a@processingChunkSize, res@processingChunkSize)
    expect_equal(rtime(a), rtime(res))
    expect_equal(mz(a), mz(res))
    unlink(d, recursive = TRUE)

    ## consolidate
    expect_no_error(saveMsObject(a, ptp, consolidate = TRUE))
    expect_true(all(c("ms_backend_spectra_data.txt", "spectra_slots.txt",
                      "spectra_processing_queue.json",
                      basename(unique(dataStorage(a)))) %in% dir(d)))
    res <- readMsObject(Spectra(), ptp)
    expect_equal(normalizePath(dataStorageBasePath(res)), normalizePath(d))
    expect_equal(rtime(res), rtime(a))
    expect_equal(mz(res), mz(a))

    unlink(d, recursive = TRUE)

    ## Errors
    expect_error(readMsObject(Spectra(), ptp), "No 'spectra_slots.txt'")

    expect_no_error(saveMsObject(a, ptp))
    with_mocked_bindings("MsBackendMzR" = function() stop("Failure"),
                         code = expect_error(readMsObject(Spectra(), ptp),
                                             "Please load the package"))

    tmp <- readLines(file.path(d, "spectra_slots.txt"))
    tmp[4L] <- "backend = MsBackendOther"
    writeLines(tmp, file.path(d, "spectra_slots.txt"))
    expect_error(readMsObject(Spectra(), ptp), "MsBackendOther")

    unlink(d, recursive = TRUE)
})

test_that("alabaster functions work with Spectra", {
    d <- file.path(tempdir(), "spectra_tests")
    a <- Spectra()

    ## empty object
    a@backend <- MsBackendMzR()
    expect_no_error(saveObject(a, d))
    expect_true(all(c("backend", "OBJECT", "metadata", "processing",
                      "processing_chunk_size", "processing_queue_variables",
                      "spectra_processing_queue.json") %in% dir(d)))
    res <- readObject(d)
    expect_equal(res, a)
    unlink(d, recursive = TRUE)

    ## object with data
    a <- Spectra(be_mzr)
    expect_no_error(saveObject(a, d))
    expect_true(all(c("backend", "OBJECT", "metadata", "processing",
                      "processing_chunk_size", "processing_queue_variables",
                      "spectra_processing_queue.json") %in% dir(d)))
    expect_equal(read_json(file.path(d, "OBJECT"))$type, "spectra")
    expect_no_error(validateAlabasterSpectra(d))
    res <- readObject(d)
    expect_s4_class(res, "Spectra")
    expect_equal(res@processing, a@processing)
    expect_equal(res@processingQueueVariables, a@processingQueueVariables)
    expect_equal(mz(res), mz(a))
    expect_equal(rtime(res), rtime(a))
    saveObjectFile(d, "something else")
    expect_error(validateAlabasterSpectra(d), "Expected")
    unlink(d, recursive = TRUE)

    ## consolidate = TRUE, filter data
    a <- filterRt(a, c(200, 400))
    a <- filterIntensity(a, intensity = 200)
    expect_no_error(saveObject(a, d, consolidate = TRUE))
    expect_true(
        all(c("backend", "OBJECT", "metadata", "processing",
              "processing_chunk_size", "processing_queue_variables",
              "spectra_processing_queue.json") %in% dir(d)))
    expect_true(all(basename(unique(dataStorage(a))) %in%
                    dir(file.path(d, "backend"))))
    res <- readObject(d)
    expect_s4_class(res, "Spectra")
    expect_equal(res@processing, a@processing)
    expect_equal(res@processingQueueVariables, a@processingQueueVariables)
    expect_equal(res@processingChunkSize, a@processingChunkSize)
    expect_equal(rtime(res), rtime(a))
    expect_equal(intensity(res), intensity(a))
    unlink(d, recursive = TRUE)
})

test_that("saveMsObject,readMsObject,AlabasterParam works", {
    d <- file.path(tempdir(), "spectra_tests")
    a <- Spectra()

    ap <- AlabasterParam(d)
    expect_no_error(saveMsObject(a, ap))
    res <- readMsObject(a, ap)
    expect_s4_class(res, "Spectra")
    expect_s4_class(res@backend, "MsBackendMemory")
    expect_equal(length(res), length(a))
    unlink(d, recursive = TRUE)

    setClass("DummyBackend", contains = "MsBackend")
    a@backend <- new("DummyBackend")
    expect_error(saveMsObject(a, ap), "save a backend")

    ## MsBackendMzR
    a <- filterRt(Spectra(be_mzr), c(210, 280))
    a <- filterMzRange(a, mz = c(200, 300))
    expect_no_error(saveMsObject(a, ap, consolidate = TRUE))
    res <- readMsObject(Spectra(), ap)
    expect_s4_class(res, "Spectra")
    expect_equal(normalizePath(dataStorageBasePath(res)),
                 normalizePath(file.path(d, "backend")))
    expect_equal(rtime(res), rtime(a))
    expect_equal(mz(res), mz(a))
    unlink(d, recursive = TRUE)

    ## MsBackendHdf5Peaks
    a <- filterRt(Spectra(be_hdf5), c(210, 280))
    a <- filterMzRange(a, mz = c(200, 300))
    expect_no_error(saveMsObject(a, ap, consolidate = TRUE))
    res <- readMsObject(Spectra(), ap)
    expect_s4_class(res, "Spectra")
    expect_s4_class(res@backend, "MsBackendHdf5Peaks")
    expect_equal(normalizePath(dataStorageBasePath(res)),
                 normalizePath(file.path(d, "backend")))
    expect_equal(rtime(res), rtime(a))
    expect_equal(mz(res), mz(a))

    unlink(d, recursive = TRUE)
})

test_that(".export_spectra_slots works", {
    a <- Spectra()
    d <- file.path(tempdir(), "spectra_tests")
    dir.create(d, showWarnings = FALSE)
    expect_no_error(.export_spectra_slots(a, path = d))
    expect_true(dir(d) == "spectra_slots.txt")
    res <- readLines(file.path(d, "spectra_slots.txt"))
    expect_equal(res, c("processingQueueVariables = ", "processing = ",
                        "processingChunkSize = Inf",
                        "backend = MsBackendMemory"))
    a@processingQueueVariables <- c("precursorMz", "other_var")
    a@processingChunkSize = 10
    a <- filterMzRange(a, c(10, 20))
    expect_no_error(.export_spectra_slots(a, path = d))
    res <- readLines(file.path(d, "spectra_slots.txt"))
    expect_equal(
        res[1L], "processingQueueVariables = precursorMz|other_var|msLevel")
    expect_match(res[2L], "Filter:")
    expect_equal(res[3L], "processingChunkSize = 10")

    unlink(d, recursive = TRUE)
})

test_that(".export/import_spectra_processing_queue works", {
    a <- Spectra()
    d <- file.path(tempdir(), "spectra_tests")
    dir.create(d, showWarnings = FALSE)
    expect_no_error(.export_spectra_processing_queue(a, d))
    expect_equal(dir(d), "spectra_processing_queue.json")

    res <- .import_spectra_processing_queue(
        a, file.path(d, "spectra_processing_queue.json"))
    expect_equal(a@processingQueue, res@processingQueue)

    a <- filterMzRange(a, mz = c(100, 200))
    expect_no_error(.export_spectra_processing_queue(a, d))
    res <- .import_spectra_processing_queue(
        a, file.path(d, "spectra_processing_queue.json"))
    expect_equal(length(a@processingQueue), length(res@processingQueue))
    expect_equal(a@processingQueue[[1L]]@ARGS, res@processingQueue[[1L]]@ARGS)

    myfun <- function(x, ...) {x + 5}
    a <- addProcessing(a, myfun)
    expect_no_error(.export_spectra_processing_queue(a, d))
    res <- .import_spectra_processing_queue(
        a, file.path(d, "spectra_processing_queue.json"))
    expect_equal(length(a@processingQueue), length(res@processingQueue))
    a_fun <- a@processingQueue[[2L]]@FUN
    res_fun <- res@processingQueue[[2L]]@FUN
    expect_equal(a_fun(5), res_fun(5))

    unlink(d, recursive = TRUE)
})
