### **Module:** ***Decrease resolution***

**BACKGROUND**

The time required to fit the model increases with the number of cells present in the rasters. You can use this module to generate a set of the rasters with a lower resolution to use in an initial model that will be quicker to fit.

**IMPLEMENTATION**

- This module uses the `terra::aggregate()` function. Covariates are aggregated using the mean of the cells. The aggregation raster is aggregated using the sum of the cells. 

- Before decreasing the resolution you can choose to summarise the number of cells in each polygon, using either a histogram or boxplot.

- Choose a new cell width from the dropdown list - these are multiples of the existing width of the cells - and then click *Decrease resolution*.

