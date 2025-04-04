### **Module:** ***Combine spreadsheet and shapefile***

**BACKGROUND**

This module is for use when you response data is in a spreadsheet (`.csv` or `.xlsx`) and boundary data in a shapefile. 
The spreadsheet needs to contain a column identifying the areas which is also present in the shapefile (although the names can differ).
The parts for uploading the shapefile are inspired by a function written by <a href="https://www.paulamoraga.com/book-geospatial/sec-shinyexample.html#uploading-data" target ="_blank">Paula Moraga</a>.

**IMPLEMENTATION**

- Spreadsheets are loaded using `read.csv()` or `openxlsx::read.xlsx()`. The shapefile is loaded using the `sf::st_read()` function.

- Click on *Browse...* and select the spreadsheet file.

- After selecting the spreadsheet file, the *Select spreadsheet response column* and *Select spreadsheet area column* boxes will appear. Click on the boxes to choose the columns that contain the response data and the names of the areas.

- Click on *Browse...* and select all four files associated with the shapefile, ending in `.dbf`, `.prj`, `.shp` and `.shx`. 

- After selecting the shapefile, the *Select shapefile area column* box will appear. Click on the box to choose the columns that contain the names of the areas.

- Finally, click *Load data* and the data will be plotted on the map. If any rows in the spreadsheet cannot be matched with the boundary data, errors will be printed in the log window.
