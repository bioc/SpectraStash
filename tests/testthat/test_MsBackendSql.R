library(MsBackendSql)
library(RSQLite)

s <- Spectra(MsDataHub::X20171016_POOL_POS_1_105.134.mzML())
dbf <- file.path(tempdir(), "test.sqlite")
be_osql <- setBackend(s, backend = MsBackendOfflineSql(),
                      dbname = dbf, drv = SQLite())@backend

test_that("saveMsObject,MsBackendOfflineSql,PlainTextParam works", {
    d <- file.path(tempdir(), "ms_backend_offlinesql")
    ## empty object
    p <- PlainTextParam(d)
    b <- MsBackendOfflineSql()
    saveMsObject(b, p)
    expect_true(all(c("ms_backend_dbinfo.txt", "ms_backend_spectra_ids.txt")
                    %in% dir(d)))
    dbi <- read.table(file.path(d, "ms_backend_dbinfo.txt"),
                      header = TRUE, sep = "\t")
    expect_true(.valid_dbinfo(dbi))
    unlink(d, recursive = TRUE)

    ## real object
    expect_no_error(saveMsObject(be_osql, p))
    expect_true(all(c("ms_backend_dbinfo.txt", "ms_backend_spectra_ids.txt")
                    %in% dir(d)))
    dbi <- read.table(file.path(d, "ms_backend_dbinfo.txt"),
                      header = TRUE, sep = "\t")
    expect_true(.valid_dbinfo(dbi))
    res <- read.table(file.path(d, "ms_backend_spectra_ids.txt"),
                      header = TRUE, sep = "\t")
    expect_equal(res$spectra_id, be_osql@spectraIds)
    unlink(d, recursive = TRUE)

    ## consolidate
    saveMsObject(be_osql, p, consolidate = TRUE)
    expect_true(all(c("ms_backend_dbinfo.txt", "ms_backend_spectra_ids.txt",
                      basename(be_osql@dbname))
                    %in% dir(d)))
    dbi <- read.table(file.path(d, "ms_backend_dbinfo.txt"),
                      header = TRUE, sep = "\t")
    expect_equal(dbi$value[dbi$key == "dbname"],
                 file.path(".", basename(be_osql@dbname)))
    unlink(d, recursive = TRUE)
})

test_that("readMsObject,MsBackendOfflineSql,PlainTextParam works", {
    d <- file.path(tempdir(), "ms_backend_offlinesql")
    p <- PlainTextParam(d)
    ## empty object
    saveMsObject(MsBackendOfflineSql(), p)
    res <- readMsObject(MsBackendOfflineSql(), p)
    expect_s4_class(res, "MsBackendOfflineSql")
    expect_equal(length(res), 0)
    unlink(d, recursive = TRUE)

    ## real object
    saveMsObject(be_osql, p)
    res <- readMsObject(MsBackendOfflineSql(), p)
    expect_s4_class(res, "MsBackendOfflineSql")
    expect_equal(res@dbname, be_osql@dbname)
    expect_equal(res@spectraIds, be_osql@spectraIds)
    expect_equal(res@peak_fun, be_osql@peak_fun)
    expect_equal(rtime(res), rtime(be_osql))
    unlink(d, recursive = TRUE)

    ## real object with cached data
    tmp <- be_osql
    tmp$other_col <- "A"
    tmp$rtime <- tmp$rtime + 10
    saveMsObject(tmp, p)
    res <- readMsObject(MsBackendOfflineSql(), p)
    expect_s4_class(res, "MsBackendOfflineSql")
    expect_equal(res@dbname, tmp@dbname)
    expect_equal(rtime(res), rtime(tmp))
    expect_equal(rtime(res), rtime(be_osql) + 10)
    expect_equal(res@spectraVariables, tmp@spectraVariables)
    expect_true(all(tmp$other_col == "A"))
    unlink(d, recursive = TRUE)

    ## real object with subset
    tmp <- be_osql
    idx <- c(213, 4, 13, 4, 49)
    tmp <- tmp[idx]
    saveMsObject(tmp, p)
    res <- readMsObject(MsBackendOfflineSql(), p)
    expect_equal(res@dbname, tmp@dbname)
    expect_equal(res@spectraIds, tmp@spectraIds)
    expect_equal(rtime(res), rtime(tmp))
    expect_equal(rtime(res), rtime(be_osql)[idx])
    unlink(d, recursive = TRUE)

    ## real object with consolidate
    tmp <- be_osql[1:30]
    saveMsObject(tmp, p, consolidate = TRUE)
    res <- readMsObject(MsBackendOfflineSql(), p)
    expect_equal(basename(res@dbname), basename(be_osql@dbname))
    expect_equal(normalizePath(dirname(res@dbname)), normalizePath(d))
    expect_equal(rtime(res), rtime(be_osql)[1:30])

    ## mess with the exported data.
    ## ms_backend_spectra_ids.txt missing
    file.remove(file.path(d, "ms_backend_spectra_ids.txt"))
    expect_error(readMsObject(MsBackendOfflineSql(), p),
                 "ms_backend_spectra_ids.txt")
    ## ms_backend_dbinfo.txt missing
    unlink(d, recursive = TRUE)
})

test_that(".sql_driver_class works", {
    expect_equal(.sql_driver_class(NULL), "")
    expect_equal(.sql_driver_class(s), "Spectra::Spectra")
    expect_equal(.sql_driver_class(SQLite()), "RSQLite::SQLiteDriver")
})

test_that(".dbinfo works", {
    res <- .dbinfo(be_osql)
    expect_true(is.data.frame(res))
    expect_equal(colnames(res), c("key", "value"))
    expect_equal(res$key, c("dbname", "dbdriver", "user", "host", "port"))
    expect_equal(res$value, c(be_osql@dbname, "RSQLite::SQLiteDriver",
                              "", "", NA_character_))

    tmp <- MsBackendOfflineSql()
    tmp@dbname <- "mordor"
    tmp@user <- "frodo"
    tmp@password <- "mello"
    tmp@host <- "moria"
    tmp@port <- 3128L
    res <- .dbinfo(tmp)
    expect_equal(res$value, c("mordor", "", "frodo", "moria", "3128"))

    expect_true(expect_no_error(.valid_dbinfo(res)))
})

test_that(".valid_dbinfo works", {
    dbi <- data.frame(acol = 1:5, value = c("mydb", "RSQLite::SQLiteDriver",
                                            "frodo", "mordor", "1234"))
    expect_error(.valid_dbinfo(dbi), "column names")
    dbi$key <- c("dbname", "dbdriver", "user", "host", "porto")
    expect_error(.valid_dbinfo(dbi), "missing database information: key")
    dbi$key[5L] <- "port"
    expect_no_error(.valid_dbinfo(dbi))

    dbi <- .dbinfo(be_osql)
    expect_no_error(.valid_dbinfo(dbi))
})

test_that(".consolidate_database works", {
    d <- file.path(tempdir(), "consolidate_test")
    dir.create(d)
    expect_warning(res <- .consolidate_database(MsBackendOfflineSql(), d),
                   "Database can not be")
    expect_s4_class(res, "MsBackendOfflineSql")

    expect_no_warning(res <- .consolidate_database(be_osql, d))
    file.exists(file.path(d, "test.sqlite"))
    expect_s4_class(res, "MsBackendOfflineSql")
    expect_equal(res@dbname, file.path(".", basename(be_osql@dbname)))
    unlink(d, recursive = TRUE)
})

test_that(".initialize_ms_backend_offline_sql works", {
    dbi <- .dbinfo(MsBackendOfflineSql())
    res <- .initialize_ms_backend_offline_sql(MsBackendOfflineSql(), dbi)
    expect_s4_class(res, "MsBackendOfflineSql")
    expect_equal(res@dbname, character())
    expect_equal(res@driver, NULL)

    dbi <- .dbinfo(be_osql)
    res <- .initialize_ms_backend_offline_sql(MsBackendOfflineSql(), dbi)
    expect_s4_class(res, "MsBackendOfflineSql")
    expect_equal(res@dbname, be_osql@dbname)
    expect_equal(rtime(res), rtime(be_osql))
    expect_equal(res@host, be_osql@host)
    expect_equal(res@user, be_osql@user)
    expect_equal(res@password, be_osql@password)
    expect_equal(res@port, be_osql@port)

    dbi$value[dbi$key == "user"] <- "jo"
    dbi$value[dbi$key == "host"] <- "localhost"
    dbi$value[dbi$key == "port"] <- "1234"
    res <- .initialize_ms_backend_offline_sql(MsBackendOfflineSql(), dbi,
                                              password = "what")
    expect_s4_class(res, "MsBackendOfflineSql")
    expect_equal(res@dbname, be_osql@dbname)
    expect_equal(res@user, "jo")
    expect_equal(res@host, "localhost")
    expect_equal(res@port, 1234L)
    expect_equal(res@password, "what")
    expect_equal(rtime(res), rtime(be_osql))

    ## simulating a consolidated/cached object
    p <- dirname(dbi$value[dbi$key == "dbname"])
    dbi$value[dbi$key == "dbname"] <- file.path(
        ".", basename(dbi$value[dbi$key == "dbname"]))
    res <- .initialize_ms_backend_offline_sql(MsBackendOfflineSql(), dbi, p)
    expect_equal(res@dbname, normalizePath(file.path(p, "test.sqlite"),
                                           winslash = "/"))
    expect_equal(rtime(res), rtime(be_osql))
})

test_that(".dbinfo_update_path works", {
    x <- data.frame(key = c("host", "dbname"), value = c("localhost", "test"))
    res <- .dbinfo_update_path(x, tempdir())
    expect_equal(res$value, c("localhost", "test"))

    x <- data.frame(key = c("host", "dbname"), value = c("localhost", ".test"))
    res <- .dbinfo_update_path(x, tempdir())
    expect_equal(res$value, c("localhost", ".test"))

    x <- data.frame(key = c("host", "dbname"), value = c("localhost", "./test"))
    suppressWarnings(
        res <- .dbinfo_update_path(x, tempdir())
    )
    suppressWarnings(
        expect_equal(res$value,
                     c("localhost",
                       normalizePath(
                           file.path(normalizePath(
                               tempdir(), winslash = "/"), "test"),
                           winslash = "/")))
    )
})
