set C; # Set of cardboards
set P; # Set of products
set S; # Set of suppliers
set T; # Set of producttypes
set Q; # Set of qualitytraits

param MaxCardboard{C, S};   # Max available cardboard c from supplier s
param CostCardboard{C, S};  # Cost of cardboard c from suppler s
param MinSupply{S};         # Min purchasevalue from supplier s
param Maxprod{T};           # Max production of productioncategory t
param ProdCost{T};          # Productioncost of productcategory t
param Type{P,T} binary; # Binary value indicating if product p belongs to productcategory t
param MinProd{P};           # Minimum production of product p
param SalePrice{P};         # Saleprice of product p
param QualCardboard{C, Q};  # Qualityvalue of qualitytrait q in cardboard c
param QualProdMin{P, Q};    # Mimimum qualityvalue of qualitytrait t in product p
param QualProdMax{P, Q};    # Maximum qualityvalue of qualitytrait t in product p

var x{C,S}>=0; #Amount of carboard c bought from supplier s
var y{P}>=0;   #Productionquantity of product P
var z{C,P}>=0; #Amount of cardboard c used in product p

#Objective function
maximize profit:
	sum{p in P} y[p] * SalePrice[p]-                         # Revenue from selling products
	sum{c in C, s in S} x[c,s] * CostCardboard[c,s]-         # Cost of buying cardbard
	sum{p in P, t in T} y[p] * ProdCost[t] * Type[p,t];  # Cost of production
	
subject to

Supplymax{c in C, s in S}:
	x[c,s] <= MaxCardboard[c,s]; # Bought cardboard from supplier s must be below maximum capacity from supplier

MeetDemand{p in P}:             # Production of products must be greater or equal to the demand
	y[p] >= MinProd[p];

Capacity{t in T}:                # Production of productioncategory t cant exceed the productioncapacity
	sum{p in P}y[p] * Type[p,t] <= Maxprod[t];
	
QualMin{p in P, q in Q}:         # The qualitytraits must exceed the minimum value for the product
	sum{c in C}z[c,p]*QualCardboard[c,q] >=y[p] * QualProdMin[p,q];
	
QualMax{p in P, q in Q}:         # The qualitytraits must be lower than the maximum value for the product
	sum{c in C}z[c,p]*QualCardboard[c,q] <=y[p]*QualProdMax[p,q];
	
Supplymin{s in S}:               # The company must reach the minimum purchase value for each supplier
	sum{c in C}x[c,s]*CostCardboard[c,s] >= MinSupply[s];
	
Producedequalsbought{c in C}:    # The amount of cardboard used in product must equal the amount of cardboard bought
	sum{p in P}z[c,p] = sum{s in S}x[c,s];
	
ratio{p in P}:                   # The amount of product produced must equal the amount of cardboard used in the product
	y[p] = sum{c in C}z[c,p];
	
