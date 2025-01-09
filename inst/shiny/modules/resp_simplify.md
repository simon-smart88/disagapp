### **Module:** ***Simplify polygons***

**BACKGROUND**

The polygons in your response data may be overly detailed for the analysis, showing the precise locations of boundaries between geographical areas when a less detailed version will give the same results and speed up the analysis. This module simplifies polygons by reducing the distance between individual points on the boundaries whilst preserving their overall shape.

**IMPLEMENTATION**

- This module uses `sf::st_simplify()` to reduce the complexity of the polygons. Choose a distance in metres to simplify the boundaries by and click *Simplify*. 

- For analyses where you will use covariates with a resolution of 1 km, a distance of 100 metres will simplify the polygons by a sensible amount.
