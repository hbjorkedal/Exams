
# Part C, 1)
#Formulating the model to solve this problem

# Sets and indexes 
set R; # set of regions
set F; # set of production facilities
set M; # set of markets

# Parameters
param S{R}; 		# max available supply of salmon per region r
param D{M}; 		# monthly demand in market m
param Ctra1{R, F};	# transporting cost per tonnes from region r to production facility f
param Ctra2{F, M};	# transporting cost per tonnes from production facility f to market m
param V{F};			# maximum volume capacity at facility f

var x{R, F}>=0;		# tonnes of salmon sent from region r to facility f
var y{F, M}>=0;		# tonnes of salmon sent from facility f to market m


#### Ojective function ####

minimize Cost: sum{r in R, f in F} x[r, f] * Ctra1[r, f]
	+ sum{f in F, m in M} y[f, m] * Ctra2[f, m];


subject to

SupplyConstraint{r in R}: # tonnes shipped from region must not exceed supply
	sum{f in F} x[r,f] <= S[r];
	
DemandConstraint{m in M}: # must meet demand for each market m
	sum{f in F} y[f, m] = D[m];
	
CapacityConstraint{f in F}: # tonnes sent to facility must not exceed facility capacity
	sum{r in R} x[r, f] <= V[f];
	
FlowConstraint{f in F}: # tonnes shipped to market must equal tonnes processed
	sum{r in R} x[r, f] = sum{m in M} y[f, m];
	

