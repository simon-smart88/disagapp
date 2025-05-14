### **Module:** ***Climate***

**BACKGROUND**

Climate affects disease, for example cholera is affected by rainfall and flooding, the transmission of airborne disease is influenced by temperature and humidity, and for vector-borne diseases temperature and rainfall affect the lifecycles of vectors (Mahmud *et al.* 2020). Climatic variables are therefore important covariates in many analyses and the WorldClim bioclimatic variables are a global dataset of 19 variables relating to temperature and precipitation.

**IMPLEMENTATION**

- Data are provided from the WorldClim 2.1 dataset, specifically the bioclimatic variables at a 30 arc-second resolution (approximately 1km at the equator). Quarters are defined as any three month long sequence. 
Data are downloaded via the `geodata::worldclim_country` function, but unfortunately the files contain all of the variables so depending on the area of interest can be very large and slow to download.

- The following variables are available for temperature: 

    - Mean temperature
    - Mean diurnal range - the mean difference between monthly minimum and maximum temperatures
    - Isothermality - how large day to night temperature variations are relative to annual variations
    - Temperature seasonality - the standard deviation of monthly mean temperature
    - Maximum temperature warmest month 
    - Minimum temperature coldest month
    - Temperature range - the difference between the maximum temperature of the warmest month and minimum temperature of the coldest month
    - Mean temperature wettest quarter
    - Mean temperature driest quarter
    - Mean temperature warmest quarter
    - Mean temperature coldest quarter
                            
- The following variables are available for precipitation:

    - Total precipitation
    - Precipitation wettest month
    - Precipitation driest month
    - Precipitation seasonality - the coefficient of variation (standard deviation / mean) of monthly precipitation
    - Precipitation wettest quarter
    - Precipitation driest quarter
    - Precipitation warmest quarter
    - Precipitation coldest quarter

**REFERENCES**

1. Mahmud, A.S., Martinez, P.P., He, J. et al. (2020). The Impact of Climate Change on Vaccine-Preventable Diseases: Insights From Current Research and New Directions. *Current Environmental Health Report*, 7, 384–391. <a href="https://doi.org/10.1007/s40572-020-00293-2" target="_blank">DOI: 10.1007/s40572-020-00293-2</a>.

2. Fick, S.E. and Hijmans, R.J. (2017). WorldClim 2: new 1-km spatial resolution climate surfaces for global land areas. *International Journal of Climatology*, 37: 4302-4315. <a href="https://doi.org/10.1002/joc.5086" target="_blank">DOI: 10.1002/joc.5086</a>.  

3. O'Donnell, M.S. and Ignizio, D.A. (2012). Bioclimatic predictors for supporting ecological applications in the conterminous United States. United States: U.S. Geological Survey Data Series 691. <a href="https://pubs.usgs.gov/ds/691/ds691.pdf" target="_blank">https://pubs.usgs.gov/ds/691/ds691.pdf</a>.
