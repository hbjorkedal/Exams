set N; #Set of cities
set T; # Set of trucks
set D within N; #Set of cities that can be depot
set H within N;
set M within N;

param HI{1..3}; #impact score og high impact areas
param MI{1..3}; #Impact score og mediunm impact 


var x{i in N, j in N, t in T: i <> j} binary; # 1 if truck travel between nodes
var y{D} binary; # 1 if city is a distrubutioncenter
var z{d in D, t in T} binary; #1 if truck starts at distrobutioncenter

var s{i in N, j in N, t in T: i <> j} binary; # 1 if city j is the second city visited by truck t
var r{i in N, j in N, t in T: i <> j} binary; # 1 if city j is the third city visited by truck t

# Maximizes the sum of the emergency score
maximize Emergency_Score:
    sum{i in H, t in T: i in D}z[i, t] * HI[1] +
    sum{i in M, t in T: i in D}z[i, t] * MI[1] +
    sum{i in N, j in H, t in T: i <> j} s[i,j,t]*HI[2]+
    sum{i in N, j in M, t in T: i <> j} s[i,j,t]*MI[2]+
    sum{i in N, j in H, t in T: i <> j} r[i,j,t]*HI[3]+
    sum{i in N, j in M, t in T: i <> j} r[i,j,t]*MI[3];


subject to

# Every city must be entered from somewhere	
EnterFromSomewhere{j in N}:
	sum{i in N, t in T:i<>j} x[i,j,t]=1;

# If a truck enters a city it must also leave the city	
ConnectedRoute{i in N, t in T}:
    sum{j in N: j<>i} x[j, i, t] = sum{k in N: k<>i} x[i, k, t];

# A truck cant go back to the city it just came from
NoImmediateReturn{i in N, j in N, t in T: i <> j}:
    x[i,j,t] + x[j,i,t] <= 1;

# A truck must leave from their assigned depot
LeavesDepot{t in T, d in D}: 
    z[d,t] <= sum{i in N: d<>i} x[d,i,t];

# Every truck must visit exactly 4 cities including its depot        
CitiesTruck{t in T}:
	sum{i in N, j in N:i<>j} x[i, j, t]=4;

# There must be exactly 4 depots
DepotLimit:
	sum{d in D} y[d] = 4;

# There can only be one truck per depot	
OneTruckPerDistributionCenter{d in D}:
    sum{t in T} z[d, t] <= 1;

# Every truck must belong to a depot    
Truck_Dist{t in T}:
	sum{d in D} z[d,t] = 1;

# Linking z to y	
LinkZandY{d in D, t in T}:
	z[d,t] <= y[d];

# Each truck must go from a depot d to a second city j
StartThenSecondCity {t in T, d in D}:
    z[d,t] <= sum{j in N: j <> d} s[d,j,t];

# Each truck must go from a second city j to a third city k    
SecondThenThirdCity {t in T, i in N, j in N: j <> i}:
    s[i,j,t] <= sum{k in N: k <> j} r[j,k,t];

# The second and third city of a truck cant be the same city    
DifferentValues_s_r{i in N, j in N, t in T: i <> j}:
    s[i,j,t] + r[i,j,t] <= 1;

# Each truck can only have one second city    
Max1SS{t in T}:
	sum{i in N, j in N: i<>j}s[i,j,t]=1;

# Each truck can only have one third city	
Max1TS{t in T}:
	sum{i in N, j in N: i<>j}r[i,j,t]=1;

# Each second city s must have a x counterpart	
SecondCity{i in N, j in N, t in T: i <> j}:
    s[i,j,t] <= x[i,j,t];
	
# Each third city r must have a x counterpart	
ThirdCity{i in N, j in N, t in T: i <> j}:
    r[i,j,t] <= x[i,j,t];
