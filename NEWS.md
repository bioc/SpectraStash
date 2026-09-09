# *SpectraStash* 0.97

## Changes in version 0.99.2

- Complete unit test coverage to 100%.

## Changes in version 0.99.1

- Address review comments.

## Changes in version 0.99.0

- Prepare for Bioconductor submission.

## Changes in version 0.97.7

- Fix issue with data storage paths and `consolidate = TRUE`.

## Changes in version 0.97.6

- Add functionality for a `MsBackendMemory` stash.
- Add a package vignette.

## Changes in version 0.97.5

- Refactor code to create self-contained stashes for `MsBackendMzR` and
  `MsBackendHdf5Peaks`: with `consolidate = TRUE` the data storage files are
  copied into the stash folder and the `dataStorage` path is adapted to a
  relative path enabling to directly load the stash also on another computer or
  location in the file system.

## Changes in version 0.97.4

- Add functionality for a `MsBackendCached` stash.

## Changes in version 0.97.3

- Add `readMsObject()` and `saveMsObject()` for `Spectra` objects.

## Changes in version 0.97.2

- Add parameter `consolidate` also to the save methods for `MsBackendMzR`.

## Changes in version 0.97.1

- Implement `PlainTextParam`- and `AlabasterParam`-based stashes for
  `MsBackendHdf5Peaks`.

## Changes in version 0.97.0

- Implement `saveMsObject()` and `readMsObject()` for `MsBackendMzR`.
