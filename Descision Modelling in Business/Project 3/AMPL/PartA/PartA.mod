set T; # Set of time periods

param PIFED{T}; # Observed Price Index for Existing Dwellings after period t

var gamma >= 0.01, <= 0.99; # Smoothing constant for level
var beta >= 0.01, <= 0.99; # Smoothing constant for trend

var L{T} >= 0; # Level for period t
var R{T}; # Trend for period t
var F{T} >= 0; # Forecast for period t (made before t)

# Objective function to minimize the mean absolute percentage error (MAPE)
minimize MAPE:
    (100/120) * sum{t in T:t>0} abs((F[t] - PIFED[t]) / PIFED[t]);

subject to

# Constraints to update level and trend based on expressions (2) and (3)
UpdateLevel{t in T: t<120}:
    L[t+1] = gamma * PIFED[t+1] + (1 - gamma) * (L[t] + R[t]);

UpdateTrend{t in T: t<120}:
    R[t+1] = beta * (L[t+1] - L[t]) + (1 - beta) * R[t];

# Constraint to calculate the forecast for the next period based on expression (1)
Forecast{t in T:t<120}:
    F[t+1] = L[t] + R[t];
    
# Initial conditions for level and trend
InitialLevel:
    L[0] = 18.8;

InitialTrend:
    R[0] = 0.5;
