### **Module:** ***Download covariates***

**BACKGROUND**

This module allows you to download a copy of the covariates used in the analysis so that you do not need do download them again. This avoids the risk that the covariates may be unavailable to download in the future ensuring 
that the analysis is reproducible and speeds up rerunning the analysis.

**IMPLEMENTATION**

* Click *Download covariates* to start the download.

* The covariates are saved as `.tif` files using `terra::writeRaster()` and compressed into a single `.zip` file using `zip::zipr()`

* Once you have downloaded the covariates, if you use the *Session code* module, you will have the option to just load the covariates from the downloaded `.zip` rather than downloading again.
