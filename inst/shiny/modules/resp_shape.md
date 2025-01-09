### **Module:** ***Upload shapefile***

**BACKGROUND**

This module is for use when you response data and boundary data are both present in a shapefile. 
It is inspired by a function written by <a href="https://www.paulamoraga.com/book-geospatial/sec-shinyexample.html#uploading-data" target ="_blank">Paula Moraga</a>.

**IMPLEMENTATION**

- The shapefile is loaded using the `sf::st_read()` function.

- Click on *Browse...* and select all four files associated with the shapefile, ending in `.dbf`, `.prj`, `.shp` and `.shx`. 

- After selecting the files, the *Select response variable* box will appear. Click on the box to choose the name of the response data from all of the columns present in the shapefile.

- Finally, click *Load data* and the data will be plotted on the map.
