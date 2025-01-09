### **Module:** ***Land use***

**BACKGROUND**

For some forms of response data, it may be appropriate to use land use as the aggregation raster, for example if the response data were for example if the response data was the incidence of a tree disease, then the tree land cover is equivalent to population.

This module uses the same data source as the *Land use* module in the Covariates component. Global estimates of land use from 2015 to 2019 with approximately 80 % accuracy are available from the Copernicus Global Land Service at a 100 m resolution, derived from a range of satellite data sources and 141,000 training locations (Buchorn *et al.* 2020).

**IMPLEMENTATION**

* Values of pixels correspond to the percentage of land in that area corresponding to the land use category.

* Data are downloaded as 20 degree tiles at 100 m resolution and converted to 1 km resolution. Depending on your area of interest, this module may take considerable time to return the data.

* To download the data, choose from the list of land use categories, select the year and click "Download data".

**REFERENCES**

1. <a href="https://land.copernicus.eu/en/products/global-dynamic-land-cover/copernicus-global-land-service-land-cover-100m-collection-3-epoch-2019-globe" target ="_blank">Copernicus Global Land Service</a>

2. Buchhorn, M., Lesiv, M., Tsendbazar, N.E. *et al*. (2020). Copernicus Global Land Cover Layers—Collection 2. *Remote Sensing*, 12(6), 1044. <a href="https://doi.org/10.3390/rs12061044" target ="_blank">DOI: 10.3390/rs12061044</a> 
