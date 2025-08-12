### **Module:** ***Prepare mesh***

**BACKGROUND**

This module prepares a spatial mesh which is required to simplify the model fitting process. Instead of modelling the process across an infinite space, the mesh is used to model across the space in discrete locations 
(nodes) and the results in between these nodes can be interpolated from the output. In order to avoid edge effects it is necessary to extend the mesh outside of the area of interest. The number of nodes affects the 
time required to fit the model and a mesh with 200-300 nodes is sufficient to fit an initial model. The default settings are adjusted depending on the area of interest and for an initial model are typically suitable, 
but you may wish to increase the number of nodes to 2000-3000 for final model runs. 

**IMPLEMENTATION**

- The module calls `disaggregation::build_mesh()` which in turn calls `fmesher::fm_nonconvex_hull_inla()` to generate the boundary around the area of interest and `fmesher::fm_mesh_2d()` to generate the mesh. 
The `{fmesher}` package was spun out from the `{INLA}` package (Integrated Nested Laplace Approximation) in 2024 and much of the literature was written before the split occurred.

- There are two sets of parameters that affect the density of the mesh and the shape of the boundary line drawn around the area of interest. You can generate multiple meshes using different parameters and 
these will all be visible in the Results panel. You can select which mesh to use in the model in the model in the *Selected mesh* dropdown menu once you have created at least one mesh.

- The following examples were created using the Madagascar example dataset to demonstrate the effects of changing parameters on the mesh produced. The default values for *Max edge* and *Offset* 
are adjusted depending on the response data and so the examples are shown relative to the default values.

- The *Max edge* parameter controls the maximum distance between nodes and has two values for controlling the parameter inside and outside of the area of interest respectively. This parameter has the 
largest effect on the number of nodes: 


<p align="middle">
  <img src="https://raw.githubusercontent.com/simon-smart88/disagapp/44495a7e5e57d12e1a93bd9f9b0105351e588203/inst/shiny/www/figures/mesh_default.png" height = 400 />
  <img src="https://raw.githubusercontent.com/simon-smart88/disagapp/44495a7e5e57d12e1a93bd9f9b0105351e588203/inst/shiny/www/figures/mesh_max_edge_div_2.png" height = 400 /> 
  <img src="https://raw.githubusercontent.com/simon-smart88/disagapp/44495a7e5e57d12e1a93bd9f9b0105351e588203/inst/shiny/www/figures/mesh_max_edge_div_4.png" height = 400 />
</p>

The *Offset* parameter affects the size of the outer mesh:

<p align="middle">
  <img src="https://raw.githubusercontent.com/simon-smart88/disagapp/44495a7e5e57d12e1a93bd9f9b0105351e588203/inst/shiny/www/figures/mesh_offset_div_2.png" height = 400 />
  <img src="https://raw.githubusercontent.com/simon-smart88/disagapp/44495a7e5e57d12e1a93bd9f9b0105351e588203/inst/shiny/www/figures/mesh_default.png" height = 400 />
  <img src="https://raw.githubusercontent.com/simon-smart88/disagapp/44495a7e5e57d12e1a93bd9f9b0105351e588203/inst/shiny/www/figures/mesh_offset_times_2.png" height = 400 />
</p>

The *Cutoff* parameter determines the minimum distance between nodes. With the other parameters set as the default, reducing the cutoff has no effect, but increasing it removes some of the small triangles around the boundary:

<p align="middle">
  <img src="https://raw.githubusercontent.com/simon-smart88/disagapp/44495a7e5e57d12e1a93bd9f9b0105351e588203/inst/shiny/www/figures/mesh_cutoff_div_2.png" height = 400 /> 
  <img src="https://raw.githubusercontent.com/simon-smart88/disagapp/44495a7e5e57d12e1a93bd9f9b0105351e588203/inst/shiny/www/figures/mesh_default.png" height = 400 />
  <img src="https://raw.githubusercontent.com/simon-smart88/disagapp/44495a7e5e57d12e1a93bd9f9b0105351e588203/inst/shiny/www/figures/mesh_cutoff_times_2.png" height = 400 />
</p>

In most cases, the boundary parameters (*Convex*, *Concave* and *Resolution*) do not need altering, but if you wish to modify them, toggle the *Change boundary parameters* switch. These parameters affect the shape of the boundary. 

*Convex* affects the overall roundness of the boundary with lower values making the boundary rounder.


<p align="middle">
  <img src="https://raw.githubusercontent.com/simon-smart88/disagapp/44495a7e5e57d12e1a93bd9f9b0105351e588203/inst/shiny/www/figures/mesh_convex_-0.1.png" height = 400 /> 
  <img src="https://raw.githubusercontent.com/simon-smart88/disagapp/44495a7e5e57d12e1a93bd9f9b0105351e588203/inst/shiny/www/figures/mesh_convex_-005.png" height = 400 /> 
  <img src="https://raw.githubusercontent.com/simon-smart88/disagapp/44495a7e5e57d12e1a93bd9f9b0105351e588203/inst/shiny/www/figures/mesh_default.png" height = 400 />
</p>

*Concave* affects the pointiness of the boundary, with higher values making the boundary more pointy. As seen in these examples, values that are too high can produce meshes which do not adequately cover the region of interest:

<p align="middle">
  <img src="https://raw.githubusercontent.com/simon-smart88/disagapp/44495a7e5e57d12e1a93bd9f9b0105351e588203/inst/shiny/www/figures/mesh_concave_-1.png" height = 400 />
  <img src="https://raw.githubusercontent.com/simon-smart88/disagapp/44495a7e5e57d12e1a93bd9f9b0105351e588203/inst/shiny/www/figures/mesh_default.png" height = 400 />
  <img src="https://raw.githubusercontent.com/simon-smart88/disagapp/44495a7e5e57d12e1a93bd9f9b0105351e588203/inst/shiny/www/figures/mesh_concave_-0.1.png" height = 400 />
</p>  

*Resolution* affects the distance between nodes on the boundary, with higher values increasing the number of total nodes:

<p align="middle">
  <img src="https://raw.githubusercontent.com/simon-smart88/disagapp/44495a7e5e57d12e1a93bd9f9b0105351e588203/inst/shiny/www/figures/mesh_resolution_150.png" height = 400 />
  <img src="https://raw.githubusercontent.com/simon-smart88/disagapp/44495a7e5e57d12e1a93bd9f9b0105351e588203/inst/shiny/www/figures/mesh_default.png" height = 400 />
  <img src="https://raw.githubusercontent.com/simon-smart88/disagapp/44495a7e5e57d12e1a93bd9f9b0105351e588203/inst/shiny/www/figures/mesh_resolution_600.png" height = 400 />
</p>

**REFERENCES**

1. Lindgren F., Rue, H. (2015). Bayesian Spatial Modelling with R-INLA . *Journal of Statistical Software*, 63(19) <a href="https://doi.org/10.18637/jss.v063.i19" target="_blank">DOI: 10.18637/jss.v063.i19</a>

2. Krainski, E. T. *et al*. (2019). <a href="https://becarioprecario.bitbucket.io/spde-gitbook/ch-intro.html#sec:mesh" target="_blank">Triangulation details and examples</a> in Advanced Spatial Modeling with Stochastic Partial Differential Equations Using R and INLA. Chapman & Hall/CRC Press. Boca Raton, FL.



