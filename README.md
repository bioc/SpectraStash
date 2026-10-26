# SpectraStash

*A safe way to store your `Spectra` objects in interoperable formats.*

[![Project Status: Active – The project has reached a stable, usable state and is being actively developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![Test-R-universe](https://github.com/RforMassSpectrometry/SpectraStash/workflows/Test-R-universe/badge.svg)](https://github.com/RforMassSpectrometry/SpectraStash/actions?query=workflow%3ATest-R-universe)
[![codecov](https://codecov.io/gh/rformassspectrometry/SpectraStash/graph/badge.svg?token=dOEe7No0HB)](https://codecov.io/gh/rformassspectrometry/SpectraStash)
[![:name status badge](https://rformassspectrometry.r-universe.dev/badges/:name)](https://rformassspectrometry.r-universe.dev/)
[![years in bioc](http://bioconductor.org/shields/years-in-bioc/SpectraStash.svg)](https://bioconductor.org/packages/SpectraStash)
[![license](https://img.shields.io/badge/license-GPL--3.0-brightgreen.svg)](https://opensource.org/license/gpl-3.0)

---

## Overview

[**MsStash**](https://github.com/RforMassSpectrometry/MsStash) defines a
framework for flexible, language-agnostic import and export formats for mass
spectrometry (MS) data objects in R. The **SpectraStash** package implements
this framework for `Spectra` objects and its data backends.

---

## Key Features

- 📦 Export/import `Spectra` objects and related `MsBackend` implementations
   across interoperable file formats
- 🧩 Modular design via S4 **parameter classes** and generic methods
- 🔄 Fully integrated with [Bioconductor](https://bioconductor.org)'s
  **alabaster.base** package.

---

## Supported Formats

### ✅ Plain Text (`PlainTextParam`)
- Tab-delimited export/import for key objects.

### ✅ Alabaster (`AlabasterParam`)
- Structured archival using HDF5 and JSON (via
  [`alabaster.base`](https://doi.org/doi:10.18129/B9.bioc.alabaster.base))

## Supported MS data objects

From the [*Spectra*](https://bioconductor.org/packages/Spectra) package:

- `Spectra`
- `MsBackendMzR`
- `MsBackendMemory`
- `MsBackendDataFrame`
- `MsBackendHdf5Peaks`
- `MsBackendCached`

From the [*MsBackendSql*](https://bioconductor.org/packages/MsBackendSQl)
package:

- `MsBackendSql`
- `MsBackendOfflineSql`

From the
[*MsBackendMetaboLights*](https://bioconductor.org/packages/MsBackendMetaboLights)
package:

- `MsBackendMetatoLights`

---

## Contributing

We appreciate contributions of all kinds — from bug fixes and tests to
documentation and new format support.

If you're planning to contribute:

1. Read our [contribution guidelines](https://rformassspectrometry.github.io/RforMassSpectrometry/articles/RforMassSpectrometry.html#contributions)
2. Follow the [RforMassSpectrometry style guide](https://rformassspectrometry.github.io/RforMassSpectrometry/articles/RforMassSpectrometry.html)
3. Fork the repo, create a branch, implement your changes, and submit a pull
   request
4. For new formats, implement:
   - A `*Param` S4 class
   - A `readMsObject()` and/or `saveMsObject()` method
   - Tests in `tests/testthat/`
---

## License

This package is licensed under the **GPL 3.0 License**:
📄 [https://opensource.org/license/gpl-3.0](https://opensource.org/license/gpl-3.0)

Documentation (manuals, vignettes) is licensed under **CC BY-NC-SA 4.0**:
📄 [https://creativecommons.org/licenses/by-nc-sa/4.0/](https://creativecommons.org/licenses/by-nc-sa/4.0/)

---

# Funding information

Part of this work was funded by the **European Union** under the
**HORIZON-MSCA-2021** project **101073062: HUMAN – Harmonising and Unifying
Blood Metabolic Analysis Networks**, by the **Autonomous Province of
Bolzano** under the **MetaRbolomics4Galaxy** project (CUP: D53C25001030003) from
the *Joint Projects South Tyrol–Germany 2025* funding program and by the DFG
grant
no. [564004112](https://gepris.dfg.de/gepris/projekt/564004112?language=en).

![EU Logo](https://github.com/rformassspectrometry/Metabonaut/raw/main/vignettes/images/EULogo.jpg)


![funding](https://github.com/rformassspectrometry/MsBackendMassIVE/raw/main/man/figures/SuedDFG-60.png)
