.onLoad <- function(libname, pkgname) {
    ## MsBackendMzR
    registerValidateObjectFunction("ms_backend_mz_r", validateMsBackendMzR)
    registerReadObjectFunction("ms_backend_mz_r", readMsBackendMzR)
    ## MsBackendHdf5Peaks
    registerValidateObjectFunction("ms_backend_hdf5_peaks",
                                   validateMsBackendHdf5Peaks)
    registerReadObjectFunction("ms_backend_hdf5_peaks", readMsBackendHdf5Peaks)
    ## MsBackendCached
    registerValidateObjectFunction("ms_backend_cached", validateMsBackendCached)
    registerReadObjectFunction("ms_backend_cached", readMsBackendCached)
    ## MsBackendMemory
    registerValidateObjectFunction("ms_backend_memory", validateMsBackendMemory)
    registerReadObjectFunction("ms_backend_memory", readMsBackendInMemory)
    ## MsBackendDataFrame
    registerValidateObjectFunction("ms_backend_data_frame",
                                   validateMsBackendMemory)
    registerReadObjectFunction("ms_backend_data_frame", readMsBackendInMemory)
    ## MsbackendOfflineSql
    registerValidateObjectFunction(
        "ms_backend_offline_sql", validateAlabasterMsBackendOfflineSql)
    registerReadObjectFunction(
        "ms_backend_offline_sql", readAlabasterMsBackendOfflineSql)
    ## Spectra
    registerValidateObjectFunction("spectra", validateAlabasterSpectra)
    registerReadObjectFunction("spectra", readAlabasterSpectra)
}
