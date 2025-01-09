### **Module:** ***Edit data***

**BACKGROUND**

Use this module after uploading the response data if you do not wish to use all of the data in your analysis. You can choose to keep areas that are either inside or outside of a shape you draw on the map.

**IMPLEMENTATION**

- The response data is modified using `sf::st_covered_by()`.

- Navigate to the map tab and use the rectangle or polygon draw tools on the left hand side to draw a shape on the map. Only the areas completely contained by the shape that you draw will be affected by the editing.

- You can use the options on the lower toolbar to either edit or delete shapes you have drawn on the map.

- Select whether you want to keep the areas inside or outside of the shape that you draw.

- Click *Edit data* to remove the selected areas from the data.
