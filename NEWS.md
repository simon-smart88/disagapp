disagapp 1.0.0
=============
Initial release

disagapp 1.0.1
=============
- Updated example covariates and aggregation data
- Made `cov_upload` more robust by handling non-tif files and multi-layered rasters

disagapp 1.0.2
=============
- `cov_bioclim` only downloads selected layers from the data source

disagapp 1.0.3
=============
- Improved consistency of various UI elements
- Fixed bug preventing `resp_shape` being included in Rmarkdown

disagapp 1.0.4
=============
- Removed `resp_edit` module due to {leaflet.extras} being archived on CRAN
- Move dependencies from Imports to Depends and import in `global.R` instead
