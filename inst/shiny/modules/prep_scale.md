### **Module: Scale covariates**

**BACKGROUND**

The covariates typically have different ranges of values. If unaltered, you cannot directly compare the coefficient values from the estimated model and setting priors become challenging. This module scales the values of each covariate so that have a mean of 0 and a standard deviation of 0 in order that they can be compared fairly.

**IMPLEMENTATION**

This module uses `terra::global(covariates, "mean")` to calculate the mean of each covariate and `terra::global(covariates, "rms")` to calculate the root mean square. The mean is subtracted from the covariate values to give the residual and this is divided by the root mean square to give the scaled covariate. This is equivalent to using `terra::scale, center = TRUE, scale = TRUE` but allows for the parameters used to scale the covariates to be saved so that they can be used to scale covariates for a different region of interest using the same parameters in the *Transfer predictions* module. You can still view the original unscaled covariates by toggling the switch at the bottom of the map.

