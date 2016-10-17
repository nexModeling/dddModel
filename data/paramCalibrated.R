#############################################
# References:                               #
# - Skaugen, Perebon, Nilsson, 2015         #
# - Skaugen, Weltzien, 2016                 #
#############################################

nbLevelZone <- 10

#Max Liquid water content in snow (% os Snow water equivalent, SWE)
# Method: ref Sælthun, 1996
Ws <- pro	<-0.028953 #5

#Temperature gradient (degrees pr 100 meters)
# Method: ref Sælthun, 1996
Tlr <- tgrad	<- -0.65

#Precipitation gradient (fraction pr 100 meters)
# Method: ref Sælthun, 1996
Plr <- pgrad	<-0.01

#Liquid precpititan correction
# Method: calibrated to give mean annual specific
Pc <- pkorr	<-1.0

#Solid precpititan correction
# Method: regressed with Pc
Sc <- skorr	<-1.0

#Threshold temp for snow/rain
# Method: ref Sælthun, 1996
TX	<-0.5

#Threshold temp for snowmelt
# Method: ref Sælthun, 1996
TS	<-0.0

#Degree day factor for snowmelt	(mm/degree/day)
# Method: Regressed
CX	<-4.040838

#Degree day factor for glacial melt
# Method: 1.5*CX (Skaugen et Weltzien, 2016)
CGLAC	<-5.5

#Factor for refreezing (mm/degree/day)
# Method: ref Sælthun, 1996
CFR	<-0.02

#Number of storage layers
# Method: ref Skaugen and Onof (2014)
NoL	<-5

#Degree day factor for evpotranspiration	(mm/degree/day)
# Method: Regressed
cea	<-0.116542

# Ratio defining field capacity.: fracion of D
# Method: Skaugen and Onof (2014),
R	<-0.3

# Shape parameter of GAMMA DISTRIBUTION CELERITIES
# for lower case lambda
# Method: Regressed
Gsh <- Gshape	<-1.249

# Shape parameter of GAMMA DISTRIBUTION CELERITIES
# for lower case lambda
# Method: Regressed
Gsc <- Gscale	<-0.075

# #Quantile in CELERITIY DISTRIBUTION for overland flow.
# Now to be calibrated
gtcel <- 0.9 # <1 # WARNING RANDOM AND SIMPLEST VALUE


############# NOT REFERENCED
#Shape par gamma distribution for capital LAMBDA
# Method: 	#Recession analysis
GshInt	<-1.645

#Scale par gamma distribution for capital LAMBDA
# Method: Recession analysis
GscInt	<-0.036
#############


#CV for log normal snow distribution
#Not in use anymore
CV <- cvHBV	<--1000

# Scale parameter of unit precipitation.
# Method: Estimated from observed spatial variability of precipitation
####Par for new spatial dist of SWE, shape parameter	#Estimated from observed precipitation
a0	<-22.941

#PDecorrelation length of spatial precipitation.
# Method: Estimated from observed spatial variability of precipitation
### par for new spatial dist of SWE, decorrelation length	#Estimated from observed precipitation
d	<-240.9


Dummy	<--1000.0

#Celerity for river flow [m/s]
# Method: standard value
#Fixed (to 1.0?) or calibrated
rv	<-0.59541 # 1

#GST	<--1000		#Max capacity of subsurface					#Not in use anymore


####################################
####################################
####################################

#Temporal resolution		 #
# Timeresinsec	<-86400  # automatic detection automatic

# Mean Annual Discharge (Unit??)
# Method: Measured
MAD	<-2.48
D <- totdef <- 2

UP <- 0           # if 1: update from sattelite derived SCA; if 0: do not
