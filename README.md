# Intermediate Complexity Catchment-Based Routing for LPJ-Guess



### Routing Method
The purpose of this model is to route surface and subsurface runoff given by the LPJ-Guess DGVM, through a river network created in ArcGIS using the ArcHydro package. The structure of this model consists of a routingFunctions.r file that defines the functions used in routing, and a config script in which parameters are configured and the model is excecuted.

The functions used in this model are defined below.

AggregateRunoff() - Reads in runoff NetCDFs and aggregates to catchments.

RouteWater() - Routes water through edges.

GetGaugeData() - Finds NWIS river gauges, downloads, and processes data for plotting.

GetShapesInBounds() - Subsetts catchments or edges by HUC code, checks if any upstream polygons are missing, and adds them. The three functions below are used.

getParents() - Returns the two parents of an edge given it's ID.

getOrder() - Returns the order of an edge given it's ID.

findAllParents() - Recursive function, returns all the upstream edges given it's ID.


CorrectEdgeSlopes() - Because many slopes calculated in ArcHydro are negative or zero, this function sets them to a minimum slope value.

AssignContribArea() - Assigns contributing area to edges.

AssignBfWidth() - Assigns width dimensions to edges using equation (6).

AssignAcoeff() - Assigns "a" coefficient used in non-linear groundwater discharge relationship given in equation (4).

GetShapesById() - Subsetts catchments or edges by ID. Useful if GetShapesInBounds() is not properly subsetting or if are to simulate is smaller than HUC 10.

MakeHydrographs() - Creates hydrographs automatically given flow and gauge data.



### Mechanics of Model

First, runoff is aggregated from the NetCDF using the AggregateRunoff fucntion. It is first converted to a raster brick, then using the extract() function, aggregated to catchments. A raster pixel is inside the catchment polygon if the center of it is. It may be possible to use the "weights" option of extract() in order to get only the portion of the raster that the polygon covers if not line up exactly. This may also make it possible to use other catchment polygons and edges such as the NHD dataset.

After surface and subsurface runoff have been aggregated, the routing can be done. The routing is governed primarily by the following equation (1).

![alt-tag](https://latex.codecogs.com/gif.latex?\frac{\mathrm{d}S}{\mathrm{d}t}=R_{s}+Q_{gw}+Q_{in}-Q_{out}-Q_{loss})

<img src="https://latex.codecogs.com/gif.latex?\frac{\mathrm{d}S}{\mathrm{d}t}=R_{s}+Q_{gw}+Q_{in}-Q_{out}-Q_{loss}"/>
<img src="https://latex.codecogs.com/png.latex?\frac{\mathrm{d}S}{\mathrm{d}t}=R_{s}+Q_{gw}+Q_{in}-Q_{out}-Q_{loss}"/>
<img src="https://latex.codecogs.com/gif.latex?\frac{\mathrm{d}S}{\mathrm{d}t}=R_{s}&plus;Q_{gw}&plus;Q_{in}-Q_{out}-Q_{loss}"/>
<img src="https://latex.codecogs.com/gif.latex?\frac{\mathrm{d}S}{\mathrm{d}t}=R_{s}&plus;Q_{gw}&plus;Q_{in}-Q_{out}-Q_{loss}"/>
<img src="https://latex.codecogs.com/png.latex?\frac{S}{t}"/>

The term <img src="https://latex.codecogs.com/gif.latex?\frac{\mathrm{d}S}{\mathrm{d}t}"/> represents the change in storage for edges at each timestep. This is equal to the inflow of surface runoff, Rs, plus groundwater discharge, Qgw, from stored subsurface runoff, plus inflow from upstream edges, Qin, minus edge discharge, Qout, and loss to infiltration, irrigation, evaporation, etc., Qloss. Qloss is currently set to zero for simplicity.

Qout is governed by equations (2) and (3). L is the length of a stream reach, in km, v is the velocity of that stream reach at time t, in the same units as L and delta t. Assuming delta t is 1, and that Qin comes in evenly throughout the dat,  the term <img src="https://latex.codecogs.com/gif.latex\dpi{100}?(1-\frac{L}{v\Delta t})"/> gives fraction of water that will leave the reach at each timestep. Assuming inflows Rs and Qgw are distrubuted evenly throughout the reach and that they too come in evenly througout the day, the fraction that will leave the reach in a timestep is given by <img src="https://latex.codecogs.com/gif.latex\dpi{100}?(1-\frac{L}{2v\Delta t})"/> . If <img src="https://latex.codecogs.com/gif.latex\dpi{100}?\frac{L}{v\Delta t}"/> is less than 1, all storage in the river, Sriv, from the previous time step will exit the reach.

<img src="https://latex.codecogs.com/gif.latex\dpi{100}?
Q_{out1}=S_{riv}+(1-\frac{l}{v\Delta t})\sum Q_{in}+(1-\frac{l}{2v\Delta t})(R_{s}+Q_{gw})
"/> (2)

Equation (1) is stable and conserves mass given that <img src="https://latex.codecogs.com/gif.latex\dpi{100}?\frac{L}{v\Delta t} < 1"/> , but when it isn't, given a very L, or very low velocity, equation (2) is used. In this case, the distance water could move in a given timestep is less than the reach length, so none of the Qin will exit the reach in a timestep. Thee term <img src="https://latex.codecogs.com/gif.latex\dpi{100}?\frac{v\Delta t}{l}"/> gives the fraction of the reach for which <img src="https://latex.codecogs.com/gif.latex\dpi{100}?\frac{L}{v\Delta t} < 1"/> is true, and all of Sriv in that sement of the reach exits in a given timestep. Using the same <img src="https://latex.codecogs.com/gif.latex\dpi{100}?(1-\frac{L}{2v\Delta t})"/> routing method, we find that for the fraction of the reach given by <img src="https://latex.codecogs.com/gif.latex\dpi{100}?\frac{v\Delta t}{L}"/> , the L, in this case is equal to <img src="https://latex.codecogs.com/gif.latex\dpi{100}?v\Delta t"/> . Here, one half of the surface runoff and groundwater discharge entering the reach sub-segment, will exit in a timestep. Above that sub-segment, none will exit. This gives us the term <img src="https://latex.codecogs.com/gif.latex\dpi{100}?\frac{v\Delta t}{2L}"/> .
 
<img src="https://latex.codecogs.com/gif.latex\dpi{100}?
Q_{out2}=\frac{v\Delta t}{l}S_{riv}+(\frac{v\Delta t}{2l})(R_{s}+Q_{gw})
"/> (3)

Groundwater dishcharge is based on a  non-linear storage discharge relationship given in the paper (insert link to paper here). At each timestep, subsurface runoff is stored as groundwater, Sgw, and the discharge, Qgw, is calculated the following timestep from equation (4). A is a dimensionless parameter that is currently based off of catchment area, but needs to be calibrated with stream gauges. In this case, b has been fixed to .5, giving exponential relationsip between storage and discharge. 

<img src="https://latex.codecogs.com/gif.latex\dpi{100}?
Q_{gw}=(\frac{S_{gw}}{a})^{\frac{1}{b}}
"/> (4)

At each timestep, velocity is calculated from the previous timestep's values using a modified Manning's equation. R is the hydraulic radius given by equation (7). S is the slope, calculated in ArcGis using the ArcHydro toolset. Mannings coefficient, n,  is is set for the entire watershed, and is currently fixed to 0.07. This could be calibrated in the future and made variable for each edge.

<img src="https://latex.codecogs.com/gif.latex\dpi{100}?
v=\frac{R^{\frac{2}{3}}S^{\frac{1}{2}}}{n}
"/> (5)

Stream dimensions are rectangular, but in the future may be modified to be trapezoidal. Width is calculated with an empirical power law function and calibrated to match observations. Currently, a and b are fixed to 0.3, and 0.6, respectively.

<img src="https://latex.codecogs.com/gif.latex\dpi{100}?
W=a(A_{total})^b
"/> (6)

Height is calculated using equation (8) from previous timestep's storage and dimensions. From this and width, hydraulic radius is calculated in equation (7) for use in Mannings equation.

<img src="https://latex.codecogs.com/gif.latex\dpi{100}?
R=\frac{A}{P}=\frac{HW}{2H+W}
"/> (7)

<img src="https://latex.codecogs.com/gif.latex\dpi{100}?
H=\frac{S_{riv}}{lw} 
"/> (8)

In the future, flood situations may be included where if heigh exceeds a bankfull height, calculated using an empirical power law equation (9), width will 5 times original width to account for flood plain, and mannings n could be increased. This is not currently part of the model.

<img src="https://latex.codecogs.com/gif.latex\dpi{100}?
H_{bf}=a(A_{total})^b 
"/>



