#=========================================================================
# Computing the temporal development of stand characteristics in Norway
# spruce with the models by Vuokila & Väliaho (1980).
## The initial state is defined by bonity class, biological age, dominant
# height, number of stems and basal area with bark. Note that in order
# for the models to give reasonable predictions, the INITIAL AGE SHOULD
# NOT BE LESS than 20 years for bonity classes H100 = 33 and H100 = 27,
# 25 years for bonity classes H100 = 24 and H100 = 21, and 30 years for
# bonity class H100 = 18. The temporal development of stand characteristics
# is computed in 5-year intervals, starting from the initial age.
## For treatments, the timing and intensity of thinnings as well as the
# rotation length need be specified. Note that for the time being,
# THINNINGS CAN ONLY TAKE PLACE AT 5-YEAR INTERVALS starting from the
# initial age (i.e., the thinning times have to coincide with the times
# for which the stand characteristics are computed in the script). 
#
# Unaffected by thinnings, the temporal development of dominant height
# is computed in three alternative ways:
#
# (i) predicted with the models by Vuokila & Väliaho, starting from the
# initial values of age and dominant height that are set in this script
#
# (ii) taken as the mean curve of the bonity class, according to the models
# by Vuokila & Väliaho
#
# (iii) taken from a PipeQual simulation, carried out so that it produces
# the initial state used in this script at the chosen initial age employed
# in this script (the PipeQual simulation typically starts earlier than
# is possible with these models by Vuokila & Väliaho).
#
# The models for sawtimber and waste percentages of stem volume
# (Sperc.spruce.f and Wperc.spruce.f) do not work properly, particularly
# with small stand volumes. Therefore, the values of sawtimber and pulpwood
# volume (S and FF) remain quite uncertain.
#
# The results are output in two kind of data frames: the temporal development
# of stand characteristics in "VV.growth" and the characteristics of the
# removals in the thinnings in "VV.thinnings". For the dominant height options
# (ii) and (iii), the name of the option is included in the data frame name
# (e.g. "VV.growth.bonity", "VV.thinnings.PipeQual").
#
# Input: Setting of the source of dominant height in the script;
# PipeQual output file "sta.out" in a user-specified directory,
# if the dominant height option (iii) is used; settings of bonity class,
# initial state of stand, thinnings and rotation in the script
# Output: See above.

#==========================================================================
# SET: Selection of the source of the temporal development of dominant height
#----------------------------------------------------------------------------
# "VV" : dominant height model by Vuokila & Väliaho
# "bonity" : mean dominant height curve of the bonity class
# "PipeQual": dominant height curve from PipeQual simulation

#Hsource <- "PipeQual"
#Hsource <- "bonity"
Hsource <- "VV"

# If Htype <- "PipeQual", give the PipeQual output file where the temporal
# development of dominant height is found

#datafile <- "C:/HY-data/AMAKELA/MyDocuments/Research/USER/VisualStudio/GrowthVsVuokilaValiaho/R/OUT/Nynäs3_2/sta.out"
#datafile <-"C:/HY-data/AMAKELA/MyDocuments/Research/USER/Visual Studio/Nitrogen/nitrogen/nitrogen/sta.out"

# SET: Settings of the bonity class, initial state of the stand, thinnings and
# rotation length

#-----------------------------------------------------------------------------
#
# Parameterisizing initial state
#		     H100 = c(33, 30, 27, 24, 21),
#		     T = c(15, 20, 25, 25, 30),
#		     H = c(5.7, 7.0, 7.7, 5.9, 6.0),
#		     G = c(4.9, 7.4, 8.4, 3.1, 1.9),
#		     N = c(2200, 2200, 2000, 2000, 1800) )

# Bonity class
H100 <- 33

# Initial state
#..............
# Biological age (a)
T0 <- 15

# Dominant height (m); if the mean bonity curve or PipeQual simulation is used
# for the temporal development of dominant height (i.e., Hsource <- "bonity" or
# (Hsource <- "PipeQual"), set H0 <- NULL

H0 <- 5.7
#H0 <- NULL

# Number of stems (/ha)
N0 <- 2200

# Basal area at breast height WITH BARK (m2/ha)
G0 <- 4.9

# Rotation length and thinnings
#...............................
# Rotation length (biological age in the end of the rotation, a) 
#Tn <- 80
Tn <- 110

# Number and timing of thinnings (in years of age); if no thinnings are
# performed, set thin.n <- 0 and thin.times <- NULL
thin.n <- 3
#thin.n <- 0
#thin.times <- c(40, 55, 70)
#thin.times <- NULL
#thin.times <- c(25, 35, 45, 65)
#thin.times <- c(30, 40, 50, 60, 70)
#thin.times <- c(35, 45, 60)
thin.times <- c(50, 65, 85)
#thin.times <- c(35, 50, 65)
#thin.times <- c(30, 40, 55)
#thin.times <- c(35, 55)
#thin.times <- c(25, 35, 45, 55, 70)
#thin.times <- c(35, 50, 65)
#thin.times <- c(40, 60)
#thin.times <- c(35, 45, 60, 75)
#thin.times <- c(40, 50, 65, 85)
#thin.times <- c(45, 70)

# Intensities of thinnings (relative amount of removal IN STEM VOLUME WITH
# BARK); if no thinnings are performed, set thin.ints <- NULL
#thin.ints <- c(0.30, 0.30, 0.30)
#thin.ints <- c(0.25, 0.25, 0.25, 0.25)
#thin.ints <- c(0.20, 0.20, 0.20)
#thin.ints <- NULL
#thin.ints <- c(0.15, 0.15, 0.15, 0.15, 0.15)
#thin.ints <- c(0.35, 0.35, 0.35)
#thin.ints <- c(0.35, 0.35)
thin.ints <- c(0.30, 0.30, 0.30)
#thin.ints <- c(0.30, 0.30, 0.30, 0.30, 0.30)
#thin.ints <- c(0.30, 0.30, 0.30, 0.30)
#thin.ints <- c(0.20, 0.20, 0.20, 0.20)

# Models by Vuokila & Väliaho
#----------------------------
# "Mean annual increment percentage of dominant height during
# the future 5-year period" (unit %; H = dominant height, m;
# T = stand age, a):

PH.spruce.f <- function(H, T, a0 = -0.41018, a1 = 616.40, a2 = -3592.9) {
	    X <- 1 / (H^0.55 * T^1.05)
	    PH <- a0 + a1 * X + a2 * X^2
	    return(PH)}

# "Mean annual increment percentage of basal area [WITHOUT BARK] 
# during the future 5-year period" (unit %; H = dominant height, m;
# T = stand age, a; Gu = basal area without bark, m2/ha):

PG.spruce.f <- function(H, T, Gu, a0 = -0.099776, a1 = 3256.7, a2 = -31736) {	    
            X <- 1 / (H^0.5 * Gu^0.6 * T^0.9)	    
            PG <- a0 + a1 * X + a2 * X^2	    
            return(PG)}

# "Mean annual volume increment percentage [WITHOUT BARK] 
# during the future 5-year period" (unit %, H = dominant height, m;
# T = stand age, a; Vu = stem volume without bark, m3/ha):

PV.spruce.f <- function(H, T, Vu, a0 = -0.38873, a1 = 8341.1) {
	    PV <- a0 + a1 * (H^0.5) / (Vu^0.7 * T^1.3)
	    return(PV)}

# Stand form factor (no unit; G = basal area with bark, m2/ha; N = number of stems
# per ha, H100 = bonity index):

FH.spruce.f <- function(G, N, H100, a1 = -0.042807, a2 = -0.049259, a3 = -0.087706) {	    
            FH <- exp(a1 * log(G) + a2 * log(N) + a3 * log(H100))
	    return(FH)}

# Percentage of basal area [assumed WITH BARK, because RN.spruce.f contains explanatory
# variables with bark] removed in thinning
# (unit %; c1 = first thinning, 1 in the first thinning, 0 otherwise; c2 = other thinning,
# 0 in the first thinning, 1 otherwise;  RV = percentage of stem volume removed in
# thinning [assumed WITH BARK], %):

RG.spruce.f <- function(c1, c2, RV, a1 = 2.3851, a2 = 1.4000, a3 = 1.0016) {
	    RG <- a1 * c1 + a2 * c2 + a3 * RV
	    return(RG)}

# Percentage of stems removed in thinning
# (unit %; c1 = first thinning, 1 in the first thinning, 0 otherwise; c2 = other thinning,
# 0 in the first thinning, 1 otherwise;  RV = percentage of stem volume removed in
# thinning [WITH OR WITHOUT BARK?], %; H = dominant height, m; N = number of stems, /ha;
# G = basal area with bark, m2/ha; V = stem volume with bark, m3/ha):

RN.spruce.f <- function(c1, c2, RV, H, N, G, V,
	    a1 = -22.134, a2 = -25.383, a3 = 1.1842, a4 = 0.97946, a5 = 0.42868, a6 = -1.1804) {
	    RN <- a1 * c1 + a2 * c2 + a3 * RV + a4 * H + a5 * N / G + a6 * N / V
	    return(RN)}

# Bark area with basal area excluding bark as the explanatory variable
# (unit m2/ha; Gu = basal area without bark, m2/ha; H = dominant height, m):

BGu.spruce.f <- function(Gu, H, a0 = 0.080806, a1 = 0.62909, a2 = -0.30883, half.MSE = 0.003462) {
	     BGu <- 10^( a0 + half.MSE + a1 * log10(Gu) + a2 * log10(H) )
	     return(BGu)}

# Bark area with basal area including bark as the explanatory variable
# (unit m2/ha; G = basal area with bark, m2/ha; H = dominant height, m):

BG.spruce.f <- function(G, H, a0 = -0.15681, a1 = 0.78270, a2 = -0.33235, half.MSE = 0.00283) {
	     BG <- 10^( a0 + half.MSE + a1 * log10(G) + a2 * log10(H) )
	     return(BG)}

# Bark volume with stem volume including bark as the explanatory variable
# (unit m3/ha; V = stem volume with bark, m3/ha):

BV.spruce.f <- function(V, a0 = -0.47833, a1 = 0.72151, half.MSE = 0.013991) {
	     BV <- exp( a0 + half.MSE + a1 * log(V) )
	     return(BV)}

# Percentage of sawtimber volume of total stem volume including bark
# (unit %; V = stem volume with bark, m3/ha; N = number of stems per ha;
# H = dominant height, m):

Sperc.spruce.f <- function(V, N, H, a0 = 1.2543, a1 = -0.0044921, a2 = -0.10810,
	       a3 = 0.0027574, half.MSE = 0.010764) {
	       Sperc <- 95 * ( 1 - exp(a0 + half.MSE + a1 * 1000 * V/N + a2 * H + a3 * H^2) )
	       return(Sperc)}

# NB. Unlike in the documentation of the model (p. 14-15 and p. 19), the mean
# size of all trees V/N has to be given in dm3, NOT in m3!

# Percentage of waste volume of total stem volume including bark
# (unit %; V = stem volume with bark, m3/ha; N = number of stems per ha;
# H = dominant height, m):

Wperc.spruce.f <- function(V, N, a1 = 0.011141) {
	       Wperc <- 25 * a1 * (1200 / (1000 * V/N - 3.76) - 1) + 0.174
	       return(Wperc)}

# NB. Unlike in the documentation of the model (p. 14-15 and p. 19), the mean
# size of all trees V/N has to be given in dm3, NOT in m3!

# NB. Percentage of pulpwood volume of total stem volume with bark is obtained
# by subtracting percentages of sawtimber and waste volume from 100 %.  

# Initialising the age vector
#----------------------------
# Age (T) up to the end of the rotation, with 5-year intervals, thinning years repeated:

{if ( thin.n == 0 ) T <- seq(from = T0, to = Tn, by = 5)
else 
T <- sort( c( seq(from = T0, to = Tn, by = 5), thin.times) )}

# Computing the temporal development of dominant height (not affected by thinnings)
#----------------------------------------------------------------------------------

# As predicted with the models by Vuokila & Väliaho
#..................................................

if (Hsource == "VV") {   

# Computing the dominant height values for the distinct years (repeated years   
# of thinnings discarded)    

Ttemp <- unique(T)   

Htemp <- vector()   
length(Htemp) <- length(Ttemp)   
Htemp[1] <- H0   

for ( i in 2:length(Ttemp) ) {
       Htemp[i] <- Htemp[i-1] * (5 * PH.spruce.f(H = Htemp[i-1], T = Ttemp[i-1]) / 100 + 1)   }   

# Repeating the values in the thinning years   

ind <- Ttemp %in% thin.times 

# "TRUE" for the thinning years that need be repeated   

H <- sort( c(Htemp, Htemp[ind]) )}

# As the mean curve of the bonity class

#......................................

if (Hsource == "bonity") {   

# Setting the starting values of H and T according to the bonity class (NB. the   
# values for the earliest starting ages tried with "test.bonity.curves.R" are used)   

ind <- c(33, 30, 27, 24, 21, 18) %in% H100   
T0temp <- c(10, 10, 10, 15, 20, 20)[ind]   
H0temp <- c(2.625, 1.8825, 1.3, 2.0, 2.55, 1.505)[ind]   

# Computing the dominant height values for the distinct years starting from   
# the initial age of the bonity class, not from the initial age of this model   
# computation (T0temp instead of T0)   

Ttemp <- seq(from = T0temp, to = Tn, by = 5)   
Htemp <- vector()   
length(Htemp) <- length(Ttemp)    
Htemp[1] <- H0temp   

for ( i in 2:length(Ttemp) ) {
       Htemp[i] <- Htemp[i-1] * (5 * PH.spruce.f(H = Htemp[i-1], T = Ttemp[i-1]) / 100 + 1)   }   

# Interpolating the values for those years that match the 5-year intervals   
# considered here   

Ttemp2 <- unique(T)   
Htemp2 <- approx(x = Ttemp, y = Htemp, xout = Ttemp2, method = "linear")$y   

# Repeating the values in the thinning years   

ind <- Ttemp2 %in% thin.times 

# "TRUE" for the thinning years that need be repeated   
H <- sort( c(Htemp2, Htemp2[ind]) )}


# As taken from PipeQual output
#..............................

if (Hsource == "PipeQual") {   
# Reading the PipeQual data   

df.temp <- read.table(file = datafile,	header = F, sep = "", dec = ".",
 	na.strings = "NA", skip = 1)   

names(df.temp) <- c("time", "Wf", "Wr", "Wb", "Wbd", "Wc", "Hdom", "hav", "BA",	"V", "N", "A", "Marklund", "Wb2", "Wbd2", "CC")   
Htemp <- df.temp$Hdom   
Ttemp <- df.temp$time   

# Selecting those PipeQual years that match the 5-year intervals considered here   
# and repeating the values in the thinning years   

ind <- Ttemp %in% T 

# "TRUE" for the desired PipeQual output years    

ind2 <- Ttemp %in% thin.times 

#"TRUE" for the thinning years   

H <- sort( c(Htemp[ind], Htemp[ind2]) )}

# Initialising the vectors of the other stand characteristics
#------------------------------------------------------------

#Number of stems (N, /ha), basal area with bark (G, m2/ha),
# stem volume with bark (V, m3/ha), and basal area without bark (Gu, m2/ha)
# in the beginning of the simulation:

N <- vector()
length(N) <- length(T)
N[1] <- N0
G <- vector()
length(G) <- length(T)
G[1] <- G0
V <- vector()
length(V) <- length(T)
V[1] <- H[1] * G[1] * FH.spruce.f(G = G[1], N = N[1], H100 = H100)
Gu <- vector()
length(Gu) <- length(T)
Gu[1] <- G[1] - BG.spruce.f(G = G[1], H = H[1])

# Volume without bark per volume with bark (%), and mean size of stem (dm3):
Bperc <- vector()
length(Bperc) <- length(T)
Bperc[1] <- 100 * ( V[1] - BV.spruce.f(V = V[1]) ) / V[1]
vbar <- vector()
length(vbar) <- length(T)
vbar[1] <- 1000 * V[1] / N[1]

# Sawtimber volume with bark (S, m3/ha), zero value up to the age in which
# the sawlogs start to exist (criterium of minimum dbh 17 cm is reached),
# this age (Ts) depends on H100 and is taken from the tables (not explicitly
# stated in the text):
Ts <- c(30, 35, 35, 45, 50)[c(33, 30, 27, 24, 21) %in% H100]

# Age in which sawtimber starts to exist

S <- vector()
length(S) <- length(T)
{if(T0 < Ts) S[1] <- 0
else S[1] <- max(0, V[1] * Sperc.spruce.f(V = V[1], N = N[1], H = H[1]) / 100)}

# Pulpwood volume with bark (FF, m3/ha; minimum top diameter 6 cm),computed
# by using the pulpwood percentage obtained by subtracting sawtimber and
# wastewood percentages from 100 %:

FF <- vector()

# NB. The name "F" consistent with the tables would mask the "FALSE" value of a logical variable

length(FF) <- length(T)
{if(T0 < Ts) FF[1] <- max(0, V[1] * ( 100 - Wperc.spruce.f(V = V[1], N = N[1]) ) / 100)
else FF[1] <- max(0, V[1] * ( 100 - Sperc.spruce.f(V = V[1], N = N0, H = H[1]) -
       	    Wperc.spruce.f(V = V[1], N = N[1]) ) / 100)}

# NB. The sawtimber percentage model Sperc.spruce.f does not work properly
# but can yield even negative values with small stand volumes; here the resulting
# negative values of S and FF are just forced to zero.


# Computing the stand development with the models by Vuokila & Väliaho

#---------------------------------------------------------------------

# If there are no thinnings

{if ( thin.n == 0 ) {   N <- rep(N0, times = length(T))   
for ( i in 2:length(T) ) {
       Gu[i] <- Gu[i-1] * (5 * PG.spruce.f(H = H[i-1], T = T[i-1], Gu = Gu[i-1]) / 100 + 1)
       G[i] <- Gu[i] + BGu.spruce.f(Gu = Gu[i], H = H[i])
       V[i] <- H[i] * G[i] * FH.spruce.f(G = G[i], N = N[i], H100 = H100)
       Bperc[i] <- 100 * ( V[i] - BV.spruce.f(V = V[i]) ) / V[i]
       vbar[i] <- 1000 * V[i] / N[i]
       {       if(T[i] < Ts) {
       	       S[i] <- 0
	       FF[i] <- V[i] * ( 100 - Wperc.spruce.f(V = V[i], N = N[i]) ) / 100 
      }
       else {
       	    S[i] <- V[i] * Sperc.spruce.f(V = V[i], N = N[i], H = H[i]) / 100
	    FF[i] <- V[i] * ( 100 - Sperc.spruce.f(V = V[i], N = N[i], H = H[i]) - 
      	    	 Wperc.spruce.f(V = V[i], N = N[i]) ) / 100       }       }   }}

# If there are thinnings

else {     
# Time steps (not years) when thinnings occur
     steps.thin <- (1:length(T))[duplicated(T)]     

# From the beginning to the first thinning

     i1 <- steps.thin[1] 
# Time step of the first thinning
     for ( i in 2:(i1 - 1) ) {
     	 N[i] <- N[i-1]
	 Gu[i] <- Gu[i-1] * (5 * PG.spruce.f(H = H[i-1], T = T[i-1], Gu = Gu[i-1]) / 100 + 1)
	 G[i] <- Gu[i] + BGu.spruce.f(Gu = Gu[i], H = H[i])
	 V[i] <- H[i] * G[i] * FH.spruce.f(G = G[i], N = N[i], H100 = H100)
	 Bperc[i] <- 100 * ( V[i] - BV.spruce.f(V = V[i]) ) / V[i]
	 vbar[i] <- 1000 * V[i] / N[i]
   {
       	 if(T[i] < Ts) {
       	 	 S[i] <- 0
	 	 FF[i] <- V[i] * ( 100 - Wperc.spruce.f(V = V[i], N = N[i]) ) / 100	 }
       	 else {
       	      S[i] <- V[i] * Sperc.spruce.f(V = V[i], N = N[i], H = H[i]) / 100	 
     FF[i] <- V[i] * ( 100 - Sperc.spruce.f(V = V[i], N = N[i], H = H[i]) -
	      	   Wperc.spruce.f(V = V[i], N = N[i]) ) / 100         } 
      	 }
     }
     N[i1] <- (1 - RN.spruce.f(c1 = 1, c2 = 0, RV = thin.ints[1] * 100, H = H[i1-1],     	       N = N[i1-1], G = G[i1-1], V = V[i1-1]) / 100) * N[i1-1]
     G[i1] <- (1 - RG.spruce.f(c1 = 1, c2 = 0, RV = thin.ints[1] * 100) / 100) * G[i1-1]
     Gu[i1] <- G[i1] - BG.spruce.f(G = G[i1], H = H[i1])
     V[i1] <- (1 - thin.ints[1]) * V[i1-1]
     Bperc[i1] <- 100 * ( V[i1] - BV.spruce.f(V = V[i1]) ) / V[i1]
     vbar[i1] <- 1000 * V[i1] / N[i1]
      {     if(T[i1] < Ts) {     	     S[i1] <- 0	
     FF[i1] <- V[i1] * ( 100 - Wperc.spruce.f(V = V[i1], N = N[i1]) ) / 100     }
     else {
     	  S[i1] <- V[i1] * Sperc.spruce.f(V = V[i1], N = N[i1], H = H[i1]) / 100
	  FF[i1] <- V[i1] * ( 100 - Sperc.spruce.f(V = V[i1], N = N[i1], H = H[i1]) -
	       Wperc.spruce.f(V = V[i1], N = N[i1]) ) / 100 
    }
     }     

# From the first thinning to the last thinning
     for ( j in 2:thin.n ) {     	 
i1 <- steps.thin[j-1] 

# Time step of the previous thinning
    	 i2 <- steps.thin[j] 
# Time step of the current thinning
     	 for ( i in (i1 + 1):(i2 - 1) ) {
	      N[i] <- N[i-1]
	      Gu[i] <- Gu[i-1] * (5 * PG.spruce.f(H = H[i-1], T = T[i-1], Gu = Gu[i-1]) / 100 + 1)
	      G[i] <- Gu[i] + BGu.spruce.f(Gu = Gu[i], H = H[i])
	      V[i] <- H[i] * G[i] * FH.spruce.f(G = G[i], N = N[i], H100 = H100)
	      Bperc[i] <- 100 * ( V[i] - BV.spruce.f(V = V[i]) ) / V[i]
	      vbar[i] <- 1000 * V[i] / N[i]
       	      {
       	      if(T[i] < Ts) {       	      	      S[i] <- 0	
 	      FF[i] <- V[i] * ( 100 - Wperc.spruce.f(V = V[i], N = N[i]) ) / 100	      }
       	      else {
       	      	   S[i] <- V[i] * Sperc.spruce.f(V = V[i], N = N[i], H = H[i]) / 100
	      	   FF[i] <- V[i] * ( 100 - Sperc.spruce.f(V = V[i], N = N[i], H = H[i]) -
	      	   	Wperc.spruce.f(V = V[i], N = N[i]) ) / 100
              }
       	      }
     	  }
	  N[i2] <- (1 - RN.spruce.f(c1 = 0, c2 = 1, RV = thin.ints[j] * 100, H = H[i2-1],
     	       N = N[i2-1], G = G[i2-1], V = V[i2-1]) / 100) * N[i2-1]
	  G[i2] <- (1 - RG.spruce.f(c1 = 0, c2 = 1, RV = thin.ints[j] * 100) / 100) * G[i2-1]
	  Gu[i2] <- G[i2] - BG.spruce.f(G = G[i2], H = H[i2])
	  V[i2] <- (1 - thin.ints[j]) * V[i2-1]	  
          Bperc[i2] <- 100 * ( V[i2] - BV.spruce.f(V = V[i2]) ) / V[i2]
	  vbar[i2] <- 1000 * V[i2] / N[i2]
       	  {
       	  if(T[i2] < Ts) {
       	  	  S[i2] <- 0
	 	  FF[i2] <- V[i2] * ( 100 - Wperc.spruce.f(V = V[i2], N = N[i2]) ) / 100
	  }
       	  else {
       	       S[i2] <- V[i2] * Sperc.spruce.f(V = V[i2], N = N[i2], H = H[i2]) / 100
	       FF[i2] <- V[i2] * ( 100 - Sperc.spruce.f(V = V[i2], N = N[i2], H = H[i2]) -
	      	    Wperc.spruce.f(V = V[i2], N = N[i2]) ) / 100          }
       	  }
     }     
# From the last thinning to the end of the rotation     
         i1 <- steps.thin[thin.n] 

# Time step of the last thinning
     i2 <- length(T) 
# Time step of the end of the rotation
     for ( i in (i1 + 1):i2 ) {
     	 N[i] <- N[i-1]
	 Gu[i] <- Gu[i-1] * (5 * PG.spruce.f(H = H[i-1], T = T[i-1], Gu = Gu[i-1]) / 100 + 1)
	 G[i] <- Gu[i] + BGu.spruce.f(Gu = Gu[i], H = H[i])
         V[i] <- H[i] * G[i] * FH.spruce.f(G = G[i], N = N[i], H100 = H100)
	 Bperc[i] <- 100 * ( V[i] - BV.spruce.f(V = V[i]) ) / V[i]
	 vbar[i] <- 1000 * V[i] / N[i]
       	 {
       	 if(T[i] < Ts) {
       	 	 S[i] <- 0
	 	 FF[i] <- V[i] * ( 100 - Wperc.spruce.f(V = V[i], N = N[i]) ) / 100	 } 
      	 else {
       	      S[i] <- V[i] * Sperc.spruce.f(V = V[i], N = N[i], H = H[i]) / 100
	      FF[i] <- V[i] * ( 100 - Sperc.spruce.f(V = V[i], N = N[i], H = H[i]) -
	      	   Wperc.spruce.f(V = V[i], N = N[i]) ) / 100
         }
       	 }     
}}}

# Computing the characteristics of the timber removed in thinnings
#-----------------------------------------------------------------
# NB. Bperc is omitted since in the tables this was not really computed
# for the removed trees.

if ( thin.n != 0 ) 

{   T.thin <- thin.times
   H.thin <- H[steps.thin]   
   N.thin <- N[steps.thin - 1] - N[steps.thin]
   G.thin <- G[steps.thin - 1] - G[steps.thin]
   V.thin <- V[steps.thin - 1] - V[steps.thin]
    vbar.thin <- 1000 * V.thin / N.thin
   S.thin <- S[steps.thin - 1] - S[steps.thin]
   FF.thin <- FF[steps.thin - 1] - FF[steps.thin]}

# Collating the results in data frames of growth and thinnings

#-------------------------------------------------------------

df.growth <- data.frame(T, H, round(N), G, Gu, V, Bperc, vbar, S, FF)
names(df.growth)[c(3, 10)] <- c("N", "F")

if (Hsource == "VV") {
   assign("VV.growth", df.growth)}
if (Hsource == "bonity") {
   assign("VV.growth.bonity", df.growth)}
if (Hsource == "PipeQual") {
   assign("VV.growth.PipeQual", df.growth)}
if ( thin.n != 0 ) {   df.thinnings <- data.frame(T.thin, H.thin, round(N.thin), G.thin, V.thin,
   		vbar.thin, S.thin, FF.thin)   
names(df.thinnings) <- c("T", "H", "N", "G", "V", "vbar", "S", "F")
   if (Hsource == "VV") assign("VV.thinnings", df.thinnings)
   if (Hsource == "bonity") assign("VV.thinnings.bonity", df.thinnings)
   if (Hsource == "PipeQual") assign("VV.thinnings.PipeQual", df.thinnings)}

# Removing the redundant objects
#-------------------------------

if (Hsource == "VV") rm(Ttemp, Htemp, ind)

if (Hsource == "bonity") rm(T0temp, H0temp, Ttemp, Htemp, Ttemp2, Htemp2, ind)
if (Hsource == "PipeQual") rm(datafile, df.temp, Ttemp, Htemp, ind, ind2)
if ( thin.n != 0 ) rm(i1, i2, j,   steps.thin, T.thin, H.thin, N.thin, G.thin, V.thin, vbar.thin, S.thin, FF.thin,   df.thinnings)
rm(H100, T0, H0, N0, G0, Tn, thin.times, thin.ints, thin.n, Hsource, Ts, i,
	 T, H, N, G, Gu, V, Bperc, vbar, S, FF,	 df.growth)

rm(BG.spruce.f, BGu.spruce.f, BV.spruce.f, FH.spruce.f, PG.spruce.f,   
PH.spruce.f, PV.spruce.f, RG.spruce.f, RN.spruce.f,   
		Sperc.spruce.f, Wperc.spruce.f)