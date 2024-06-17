### **Module: Summarise and resample covariates**

**BACKGROUND**

The covariates and aggregation rasters have different resolutions 
and before they can be used in the model, they need to be adjusted so that the pixels in each raster align perfectly with each other. This module lets you examine the properties of the rasters and choose one to use as a template to transform the others.

**IMPLEMENTATION**
This module uses the `terra::resample()` function to resample the covariates and 

You can choose to remove rows in the table for characteristics of the rasters that are identical by toggling the "Remove identical rows?" switch. 

Click "Prepare covariate summary" to summarise the covariates and the view will switch to a table in the results tab.

Select a covariate from the dropdown list that will be used as a template to resample the others.

Click "Resample covariates" and a table of the resampled covariates will be displayed. You can toggle between the tables by clicking on "Original" or "Resampled".
