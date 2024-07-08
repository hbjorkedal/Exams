
# Part A, 1) #
# Formulating a model to solve this problem

set G; #Emission gasses
set M; #Set of emission reducing methods

param C{M}; 	# cost per unit of intensity of method m
param R{G,M};	# reduction of gas g with method m
param T{G};		# target reduction for gas g

var x{M}>=0; 	# units of intensity m


##### Objective function #####

minimize cost:
	  sum{m in M} x[m] * C[m]; # total costs of reducing emissions
	  
subject to

EmissionGoal{g in G}: # must meet emission goals
    sum{m in M} R[g,m] * x[m] >= T[g];

