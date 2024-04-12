### **Module: Nighttime lights**

**BACKGROUND**

Satellite imagery of the Earth captured at night can detect sources of artificial lighting. The intensity of nighttime lights provides information on economic activity and in turn poverty. NASA's Black Marble programme provides high-quality data products that are ready to use in analyses. 

**IMPLEMENTATION**

This module uses the `bm_raster()` function from `{blackmarbler}` developed by the World Bank to download data for the area of interest. The data returned is the average annual nighttime illumination (product ID <a href = "https://ladsweb.modaps.eosdis.nasa.gov/missions-and-measurements/products/VNP46A4" target="_blank">VNP46A4</a>) at a 15 arc-second resolution (approximately 500 m at the equator).

To obtain the data, select the year of interest and click "Download data".

If you are running *disagapp* locally, a NASA bearer token is required to download nighttime light data. <a href = "https://cran.r-project.org/web/packages/blackmarbler/readme/README.html#token" target="_blank">Click here</a> for details of how to obtain one and then enter it in the box or set it as an environmental variable called 'NASA_bearer'"

**REFERENCES**

Román, M.O. et al. (2018) NASA’s Black Marble nighttime lights product suite. *Remote Sensing of Environment*. 210, 113–143. <a href= "https://doi.org/10.1016/j.rse.2018.03.017" target="_blank">DOI: 10.1016/j.rse.2018.03.017</a>
