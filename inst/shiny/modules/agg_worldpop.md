### **Module:** ***Worldpop***

**BACKGROUND**

Population counts can used to inform the model where population occurs inside the polygons. Areas with higher populations would be expected to have higher incidences of disease and so including this information helps to 
produce more accurate estimates of disease rates per person.

Data is obtained from the Worldpop Project who combine multiple data sources to produce gridded population estimates (Lloyd *et al.* 2017).

**IMPLEMENTATION**

- Data is downloaded via the Worldpop API using the `{httr2}` package.

- Two methods are available for how the population estimates are derived (Stevens *et al.* 2015). The *unconstrained* method uses population data at a polygon level and covariates to estimate population in each cell. The *constrained* method 
uses satellite imagery to locate settlements and dwellings and only estimates population in cells where dwellings are present. The constrained method produces estimates which are more accurate and which contain more 
empty pixels which is advantageous when fitting the model as empty pixels can be disregarded.

- Constrained data is only available for 2020, but unconstrained data is available annually from 2000 to 2020.

- Unconstrained data is available at 100 m and 1 km resolution, but constrained data is only available at 100 m resolution and so is aggregated if a 1 km resolution is requested.

- In most cases, because of the skewed distribution of population density, to visualise the data more effectively you should plot the data using a log scale, but you can disable this if you prefer by toggling the *Plot as log values* switch.

- To download the data, select the countries, method to use, the desired resolution and then click *Download data*

**REFERENCES**

1. <a href="https://hub.worldpop.org/project/categories?id=3" target="_blank">Worldpop website</a>

2. <a href="https://www.worldpop.org/methods/top_down_constrained_vs_unconstrained/" target="_blank">Constrained vs. unconstrained methods</a>

3. Stevens, F.R., Gaughan, A.E., Linard, C., Tatem, A.J. (2015) Disaggregating Census Data for Population Mapping Using Random Forests with Remotely-Sensed and Ancillary Data. *PLoS ONE* 10(2): e0107042.<a href="https://doi.org/10.1371/journal.pone.0107042" target="_blank">DOI: 10.1371/journal.pone.0107042</a> 

4. Lloyd, C., Sorichetta, A. & Tatem, A. (2017). High resolution global gridded data for use in population studies. *Scientific Data* 4, 170001. <a href="https://doi.org/10.1038/sdata.2017.1" target="_blank">DOI: 10.1038/sdata.2017.1</a>
