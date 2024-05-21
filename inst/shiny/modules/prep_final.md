### **Module: Finalise**

**BACKGROUND**

Prior to fitting the model, all of the data needs to be combined into one object. This module combines the response data, spatial mesh, covariates and aggregation raster.

**IMPLEMENTATION**

- This module uses the `disaggregation::prepare_data()` function. 

- Select the ID variable in the response data from the dropdown list.

- Select the response variable in the response data from the dropdown list.

- If you have created low resolution versions of the covariates, select whether you wish to use this dataset or the original high resolution version.

- Choose whether you wish to handle missing data - you most likely want to do this, or the function will return an error if there is any missing data.

- Click "Prepare data"
