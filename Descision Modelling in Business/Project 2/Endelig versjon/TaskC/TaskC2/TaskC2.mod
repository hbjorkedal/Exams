set J;   # crude oils
set B;   #components
set P;   #blended products
set SP;  #saleable products

set I;   #CDU: Crude Distilling Units
set D;   #depots
set K;   #markets
set T;   #periods
set Lowqfuel within B;

param CMain; #Cost of maintanance
param E{J}; #Crude per period
param A{J, B, I}; #Amount of component b obtained from one unit of crude j refined in CDU i
param R{B, P}; #Amount of component b needed in production of product p
param S{SP}; #Salesprice of saleable product sp
param Cref{J, I}; #Cost of refining crude j in CDU i
param maxR{I}; #max capacity of CDU i
param minR{I}; #min capacity of CDU i
param Cprod{P}; #cost of producing one unit of product p at the blending department

param FixedCost{I}; # Fixed cost associated with running CDU i
param RC; # Reduced cost from running both CDU's

param Ctra1; #cost of transporting one unit of any component from refining to blending
param Ctra2{D}; #cost of transporting one unit lowqfuel from refining to depot d
param Ctra3{D}; #cost of transporting a blended product to depot d
param Ctra4{D, K}; #cost of transporting product from depot D to market K

param Cinvi; #cost of storing one unit of crude at the refinery
param Cinvb; #Cost of storing one component at the blending department
param Cinvp{D}; #Cost of storing one unit of any product at depot D

param delta{SP, K , T}; #Demand for saleable product sp at market k in time t

param Izero{SP,D}; #Initial Inventory
param Ifinal{SP,D}; #Final Inventory

# Binary Variables
var x{I, T} binary; #1 if CDU i is running on day t
var y{I, T} binary; #1 if CDU i had maintainance on day t
var r{T} binary;  # 1 if both CDUs are operating on day t, 0 otherwise


#Flow variables
var IR{J, T}>=0; # Amount of crudeoil j at the refiningdepartment at the end of day t
var IB{B, T}>=0; # Amount of component b at the blendingdepartment at the end of day t
var IP{SP, D, T}>=0; # Amount of saleable product p at depot d at the end of day t


var z{J, I, T}>=0; # Amount of crude j distilled at cdu i in period t
var c{B, T}>=0; # Amount of component b produced on day t
var u{B, T}>=0; # Amount of component b sent to blending department at day t
var BD{Lowqfuel, I, D, T}>=0; # Amount of component b sent from CDU i to depot d at day t

var w{P, T}>=0; # Amount of product p produced at blending department on day t
var PD{P, D, T}>=0; # Amount of product p sent to depot d from the blending department at day t

var v{SP,D,K,T}>=0; #Amount of saleable product p sent from depot d to market k on day t


maximize profit: sum{sp in SP, d in D, k in K, t in T: 0 < t <12} S[sp]*v[sp,d,k,t] # Income from sales
		-sum{i in I, t in T} CMain * y[i,t] # Cost of maintainence
		-sum{j in J, i in I, t in T} Cref[j, i] * z[j, i, t] # Cost of refining crude j in CDU i
		-sum{j in J, t in T} IR[j,t] * Cinvi # Cost of storing crude
		-sum{i in I, d in D, t in T} Ctra2[d]* BD['lowqfuel', i, d, t] #Cost of sending lowqfuel to depot
		-sum{b in B, t in T} Ctra1 * u[b,t] #Cost of sending components to the blending department
		-sum{p in P, t in T} Cprod[p] * w[p,t] # Cost of blending products
		-sum{b in B, t in T} Cinvb * IB[b, t] # Cost of storing components
		-sum{p in P, d in D, t in T} Ctra3[d]*PD[p,d,t] # Cost of sending products to depot
		-sum{sp in SP, d in D, t in T} Cinvp[d]*IP[sp, d, t] # Cost of storing product
		-sum{sp in SP, d in D, k in K, t in T}Ctra4[d, k]* v[sp, d, k, t] # Cost of sending product to market
		-sum{i in I, t in T}(FixedCost[i]*x[i,t]-RC*r[t]); #fixed costs

subject to
#Initial values in storage
InitIR {j in J}: IR[j, 0] = 0;
InitIB {b in B}: IB[b, 0] = 0;
InitIP {sp in SP, d in D}: IP[sp, d, 0] = Izero[sp, d];

InitZ{j in J}: sum{i in I} z[j, i, 0] = 0;
InitW{p in P}: w[p, 0] = 0;
InitR: r[0]=0;

#Ending values
FinalIP {sp in SP, d in D}: IP[sp, d, 12] >= Ifinal[sp, d];

#Balance of stored crude must be equal to yesterdays balance + todays supply - refined crude
BalanceCrude{j in J, t in T: t>0}:
	IR[j, t] = IR[j, t-1] + E[j] - sum{i in I} z[j, i , t];
	
#CDU cant be both running and in maintainence	
CDUMode{i in I, t in T: t>0}:
	y[i, t]+x[i,t] <= 1;
	
#One CDU must be running in all days
CDURun{t in T: t>0}:
	sum{i in I}x[i, t]>= 1;

#Each CDU must be maintained once in the last 6 days
CDUMain{i in I}:
	sum{t in 7..12} y[i,t] >= 1;

# CDU cant produce more than max on a given day
CDUmax{i in I, t in T: t > 0}:
	sum{j in J}z[j,i,t] <= maxR[i]*x[i,t];

# CDU must produce minimum
CDUmin{i in I, t in T: t > 0}:
	sum{j in J}z[j,i,t] >= minR[i]*x[i,t];

#Comps created cant exceed the sum of comps created from the distilled crude
CreateComps{b in B, t in T}:
	c[b,t]= sum{j in J, i in I} z[j, i , t]*A[j, b, i];

#Amount of components sent to blending cant exceed the amount produced on that day	
FlowComps{b in B diff {'lowqfuel'}, t in T}:
	u[b,t] = c[b,t];
    
FlowComps2{i in I, t in T}:
    sum{d in D} BD['lowqfuel', i, d, t] = sum{j in J}z[j,i,t]*A[j,'lowqfuel',i];

#Balance of components must be equal to yesterdays balance and production - components used in product production
BalanceComps{b in B diff {'lowqfuel'}, t in T: t>0}:
	IB[b,t] = IB[b, t-1] + u[b,t-1] - sum{p in P}w[p,t]*R[b,p];

#There must be enough components on a given day to produce the products produced on that day	
CreateProd{b in B, t in T: t>0}:
	sum{p in P}w[p,t] * R[b,p] <= IB[b, t-1] + u[b,t-1];
	
#Amount of product sent to depot must equal to produced product for each day
Productflow{p in P, t in T}:
	sum{d in D} PD[p,d,t] = w[p,t];
	
#Balanceconstraint for belended saleable products
SaleableProductBalance{d in D, p in P, t in T: t>0}:
    IP[p, d, t] = IP[p, d, t-1] + PD[p, d, t-1] - sum{k in K}v[p,d,k,t];

# Separate balance for lowqfuel
SaleableProductBalanceLowqfuel{d in D, t in T: t>0}:
    IP['lowqfuel', d, t] = IP['lowqfuel', d, t-1] + sum{i in I} BD['lowqfuel', i, d, t-1] - sum{k in K}v['lowqfuel',d,k,t];
	 
#Sum of saleable products sent to market cant exceed storage
SaleableProductflow{d in D, p in P, t in T: t>0}:
	sum{k in K} v[p,d,k,t] <= IP[p, d, t-1] + PD[p, d, t-1];
	
#Sum of saleable products sent to market cant exceed storage
SaleableProductflow2{d in D, t in T: t>0}:
	sum{k in K} v['lowqfuel',d,k,t] <= IP['lowqfuel', d, t-1] + sum{i in I}BD['lowqfuel', i, d, t-1];
	
#Products sent to market must be less than or equal to the demand
Demand{k in K, sp in SP, t in T:t>0}:
	sum{d in D}v[sp, d ,k ,t-1] <= delta[sp, k, t];

# Linking the r variable to the runningstate of the CDU's		
BothCDUsOperate {t in T:t>0}:
	 r[t] = sum{i in I}x[i,t] - 1;


