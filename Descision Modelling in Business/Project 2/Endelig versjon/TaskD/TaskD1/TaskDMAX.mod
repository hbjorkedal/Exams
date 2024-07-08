set N; #Set of cities
set T; # Set of trucks
set D within N; #Set of cities that can be depot

param c{N, N}; #Traveltime cost between two nodes
param l{D}; #Cost of loading in depot

var x{i in N, j in N, t in T: i <> j} binary; # 1 if truck travel between nodes
var y{D} binary; # 1 if city is a distrubutioncenter
var z{d in D, t in T} binary; #1 if truck starts at distrobutioncenter
var T_max >= 0; # Maksimal reisetid for en lastebil

# Minimizes the longest route of any truck
minimize Max_Truck_Distance:
	T_max;

subject to

# Connects the T_max variable to the route length of the trucks
MaxTimeForEachTruck{t in T}:
    sum{i in N, j in N: i <> j} c[i,j] * x[i,j,t]+
     sum{d in D} l[d] * z[d,t] <= T_max;

# Every city must be entered by a truck
EnterFromSomewhere{j in N}:
	sum{i in N, t in T:i<>j} x[i,j,t]=1;

# If a truck enters a city it must also leave the city	
ConnectedRoute{i in N, t in T}:
    sum{j in N: j<>i} x[j, i, t] = sum{k in N: k<>i} x[i, k, t];

# A truck cant go back the same way it came.
NoImmediateReturn{i in N, j in N, t in T: i <> j}:
    x[i,j,t] + x[j,i,t] <= 1;

# Every truck must leave from a depot
LeavesDepot{t in T, d in D}: 
    z[d,t] <= sum{j in N: d<>j} x[d,j,t];
    
# Every truck must go between excactly 4 cities
CitiesTruck{t in T}:
	sum{i in N, j in N:i<>j} x[i, j, t]=4;

# Must be excactly 4 depots
DepotLimit:
	sum{d in D} y[d] = 4;
	
# There can only be one truck per distributioncenter
OneTruckPerDistributionCenter{d in D}:
    sum{t in T} z[d, t] <= 1;
   
# Every truck must belong to a distrubutioncenter
Truck_Dist{t in T}:
	sum{d in D} z[d,t] = 1;

# Links the z and y variables	
LinkZandY{d in D, t in T}:
	z[d,t] <= y[d];




 
	


    