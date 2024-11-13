### **Module: Fit model**

**BACKGROUND**

This module fits the model using the `disaggregation::disag_model()` function. Several choices need to be made depending on the type of model to be fitted. These are explained below.

**IMPLEMENTATION**

#### Model family

The choice of model family depends on the form of the response data. The poisson model should be used when the response data is in the form of counts, for example the number of cases of a disease in each polygon. The Gaussian model should be used when the response data is not in the form of counts, for example if it contains non-integer or negative values. 

#### Model link function

The link function informs the relationship between the covariates and the response data and restricts the possible range of the predictions that the model can produce. Typically, each model family uses an associated link function: Poisson models should use the Log link function which ensures that all of the predictions made by the model will be positive. Gaussian models use the Identity link function which does not restrict the predictions made by the model. The link function will be automatically updated when you select a model family but you can override this if you wish.

#### IID effect

The IID effect is a model term applied uniformly across each polygon and which across all polygons has a mean of zero. It is a method of modelling overdispersion and can account for differences in data collection methods between polygons or for the tendency for cases of transmissible diseases to multiply rapidly once an outbreak occurs, but this is not necessarily due to the covariate values. The IID effect is not spatial meaning that two neighboring polygons can have completely opposite values. This term should not be included when using the Gaussian family as the Gaussian error term is essentially identical. When fitting a poisson model, you should include this term unless you wish to run the model more quickly. Once the model is fitted you can see the output of the model including and excluding this term. 

#### Spatial field

The spatial field is a model term similar to the IID effect but varies continuously across the region of interest and can differ inside each polygon. Also, it is a spatial effect meaning that it neighbouring pixels or polygons will have similar values. It can account for the effects of covariates that are not included in the model. As with the IID effect it should normally be included in the model unless you wish to fit the model more quickly.

#### Iterations

During the fitting process, parameters are iterated through to find the optimum values. The iterations setting determines the maximum number to stop the model fitting running forever if it does not converge. Typically 100 iterations is sufficient to fit the model, but you can change this if you wish. If the model does not converge after 100 iterations there is probably something wrong with the data.

#### Priors

As a Bayesian process, the model fitting requires priors to be set that represent our prior knowledge about the parameter values before examining the dataset. By default these are set automatically to aid initial model fitting with some being adjusted according to the response data and the spatial scale. For final model fitting, however you should alter these using your understanding of the system. Click the "Set priors" switch to load the default values and enable them to be edited. The available priors will depend on whether you have included the IID effect and spatial field terms in your model.

For all models you can adjust:

* Mean intercept
* Intercept standard deviation
* Mean slope
* Slope standard deviation

If you include an IID effect you can adjust:

* Maximum IID effect
* IID effect probability

If you include a spatial field you can adjust:

* Minimum rho
* Rho probability
* Maximum sigma
* Sigma probability

**REFERENCES**

* Nandi, A. K., Lucas, T. C. D., Arambepola, R., Gething, P., & Weiss, D. J. (2023). disaggregation: An R Package for Bayesian Spatial Disaggregation Modeling. Journal of Statistical Software, 106(11), 1–19. <a href="https://doi.org/10.18637/jss.v106.i11" target="_blank">DOI: 10.18637/jss.v106.i11</a>
