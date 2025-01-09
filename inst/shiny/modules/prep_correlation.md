### **Module:** ***Covariate correlations***

**BACKGROUND**

If two or more covariates are highly correlated then using both in the model will probably add little predictive power and may increase the difficulty of interpreting the coefficients of the fitted model. 
This module lets you plot a correlation matrix of the covariates and if some are highly correlated, you can choose to remove them. 

**IMPLEMENTATION**

- This module uses the `corrplot::corrplot()` function.

- Select the method you would like to use from the dropdown list - these are different styles of how the plot is shown.

- Select the type of correlation matrix from the dropdown list - you can either plot a triangular matrix with the corner in the bottom left (Lower) or top right (Upper) or a square matrix (Full).

- Toggle the switch if you would like to include self-correlations between the covariates (i.e. that are all fully correlated).

- Click *Plot correlation matrix* to generate the plot and the view will switch to show the results panel.

- Once you have examined the plot, if you wish to remove any covariates, select them from the dropdown list and click *Remove selected covariate*.
