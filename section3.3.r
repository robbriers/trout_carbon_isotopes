
# Results section 3.3 -----------------------------------------------------

library(MuMIn)

# load data files required

riparian_100m <- read.csv("riparian_100m.csv", stringsAsFactors = TRUE)
point_1km <- read.csv("point_1km.csv", stringsAsFactors = TRUE)
riparian_50m <- read.csv("riparian_50m.csv", stringsAsFactors = TRUE)
riparian_100m <- read.csv("riparian_100m.csv", stringsAsFactors = TRUE)


# 100m point buffer models ------------------------------------------------

# single variable models
CF_100m<- lm(Average_d13C ~ CF, data = riparian_100m)
MH_100m<- lm(Average_d13C ~ MH, data = riparian_100m)
Pa_100m<- lm(Average_d13C ~ Pastures, data = riparian_100m)
Pe_100m<- lm(Average_d13C ~ Peat, data = riparian_100m)
TWS_100m<- lm(Average_d13C ~ TWS, data = riparian_100m)
LD_100m<- lm(Average_d13C ~ LD, data = riparian_100m)
MD_100m<- lm(Average_d13C ~ MD, data = riparian_100m)
HD_100m<- lm(Average_d13C ~ HD, data = riparian_100m)
Alt_100m<- lm(Average_d13C ~ Alt_mean, data = riparian_100m)
Slope_100m<- lm(Average_d13C ~ Slope_mean, data = riparian_100m)
# multivariable additive models
LD_Pa_100m<- lm(Average_d13C ~LD+Pastures, data = riparian_100m)
LD_alt_100m<- lm(Average_d13C ~LD+Alt_mean, data = riparian_100m)
LD_slope_100m<- lm(Average_d13C ~Slope_mean, data = riparian_100m)
LD_Pa_alt_100m<- lm(Average_d13C ~LD+Pastures+Alt_mean, data = riparian_100m)
LD_Pa_slope_100m<- lm(Average_d13C ~LD+Pastures+Slope_mean, data = riparian_100m)
LD_Pa_alt_slope_100m<- lm(Average_d13C ~LD+Pastures+Alt_mean+Slope_mean, 
                          data = riparian_100m)
Pa_HD_100m <- lm(Average_d13C~Pastures+HD, data = riparian_100m)
Pa_alt_100m <- lm(Average_d13C~Pastures+Alt_mean, data = riparian_100m)
Pa_MD_100m<- lm(Average_d13C~Pastures+MD, data = riparian_100m)
Pa_HD_alt_100m <- lm(Average_d13C~Pastures+HD+Alt_mean, data = riparian_100m)
Pa_MD_alt_100m <- lm(Average_d13C~Pastures+HD+Alt_mean, data = riparian_100m)
Pa_MD_HD_alt_100m <- lm(Average_d13C~Pastures+MD+HD+Alt_mean, data = riparian_100m)
Pa_MD_HD_100m <- lm(Average_d13C~Pastures+MD+HD, data = riparian_100m)
TWS_CF_100m<- lm(Average_d13C~TWS+CF, data=riparian_100m)
TWS_alt_100m<- lm(Average_d13C~TWS+Alt_mean, data=riparian_100m)
TWS_CF_alt_100m<- lm(Average_d13C~TWS+CF+Alt_mean, data=riparian_100m)
HD_Pe_100m<- lm(Average_d13C~HD+Peat, data=riparian_100m)
HD_slope_100m<- lm(Average_d13C~HD+Slope_mean, data=riparian_100m)
HD_Pe_slope_100m<- lm(Average_d13C~HD+Peat+Slope_mean, data=riparian_100m)
Alt_Pe_100m<- lm(Average_d13C~Alt_mean+Peat, data=riparian_100m)
global_100m <- lm(Average_d13C~LD+Pastures+Slope_mean+Alt_mean+HD+MD+TWS+CF+
                    Peat, data=riparian_100m)
# multivariable models with interactions
LDPa_100m <- lm(Average_d13C~LD*Pastures, data = riparian_100m)

#model selection
models_100m <- model.sel(CF_100m,MH_100m,Pa_100m,Pe_100m,TWS_100m,LD_100m,
                         MD_100m,HD_100m, Alt_100m,Slope_100m,LD_Pa_100m,
                         LD_alt_100m,LD_slope_100m,LD_Pa_alt_100m,
                         LD_Pa_slope_100m,LD_Pa_alt_slope_100m,Pa_HD_100m,
                         Pa_alt_100m,Pa_MD_100m,Pa_HD_alt_100m,Pa_MD_alt_100m,
                         Pa_MD_HD_alt_100m,Pa_MD_HD_100m,TWS_CF_100m,
                         TWS_alt_100m, TWS_CF_alt_100m,HD_Pe_100m,HD_slope_100m,
                         HD_Pe_slope_100m, Alt_Pe_100m, LDPa_100m,global_100m)

# confidence set
conf_set_100m <- models_100m[models_100m$delta<2, ]

# 1km point buffer models -------------------------------------------------

# single variable models
CF_1km<- lm(Average_d13C ~ CF, data = point_1km)
MH_1km<- lm(Average_d13C ~ MH, data = point_1km)
Pa_1km<- lm(Average_d13C ~ Pastures, data = point_1km)
Pe_1km<- lm(Average_d13C ~ Peat, data = point_1km)
TWS_1km<- lm(Average_d13C ~ TWS, data = point_1km)
LD_1km<- lm(Average_d13C ~ LD, data = point_1km)
MD_1km<- lm(Average_d13C ~ MD, data = point_1km)
HD_1km<- lm(Average_d13C ~ HD, data = point_1km)
Alt_1km<- lm(Average_d13C ~ Alt_mean, data = point_1km)
Slope_1km<- lm(Average_d13C ~ Slope_mean, data = point_1km)
# multivariable additive models
LD_Pa_1km<- lm(Average_d13C ~LD+Pastures, data = point_1km)
LD_alt_1km<- lm(Average_d13C ~LD+Alt_mean, data = point_1km)
LD_slope_1km<- lm(Average_d13C ~Slope_mean, data = point_1km)
LD_Pa_alt_1km<- lm(Average_d13C ~LD+Pastures+Alt_mean, data = point_1km)
LD_Pa_slope_1km<- lm(Average_d13C ~LD+Pastures+Slope_mean, data = point_1km)
LD_Pa_alt_slope_1km<- lm(Average_d13C ~LD+Pastures+Alt_mean+Slope_mean, 
                          data = point_1km)
Pa_HD_1km <- lm(Average_d13C~Pastures+HD, data = point_1km)
Pa_alt_1km <- lm(Average_d13C~Pastures+Alt_mean, data = point_1km)
Pa_MD_1km<- lm(Average_d13C~Pastures+MD, data = point_1km)
Pa_HD_alt_1km <- lm(Average_d13C~Pastures+HD+Alt_mean, data = point_1km)
Pa_MD_alt_1km <- lm(Average_d13C~Pastures+HD+Alt_mean, data = point_1km)
Pa_MD_HD_alt_1km <- lm(Average_d13C~Pastures+MD+HD+Alt_mean, data = point_1km)
Pa_MD_HD_1km <- lm(Average_d13C~Pastures+MD+HD, data = point_1km)
TWS_CF_1km<- lm(Average_d13C~TWS+CF, data=point_1km)
TWS_alt_1km<- lm(Average_d13C~TWS+Alt_mean, data=point_1km)
TWS_CF_alt_1km<- lm(Average_d13C~TWS+CF+Alt_mean, data=point_1km)
HD_Pe_1km<- lm(Average_d13C~HD+Peat, data=point_1km)
HD_slope_1km<- lm(Average_d13C~HD+Slope_mean, data=point_1km)
HD_Pe_slope_1km<- lm(Average_d13C~HD+Peat+Slope_mean, data=point_1km)
Alt_Pe_1km<- lm(Average_d13C~Alt_mean+Peat, data=point_1km)
global_1km <- lm(Average_d13C~LD+Pastures+Slope_mean+Alt_mean+HD+MD+TWS+CF+
                    Peat, data=point_1km)
# multivariable models with interactions
LDPa_1km <- lm(Average_d13C~LD*Pastures, data = point_1km)

#model selection
models_1km <- model.sel(CF_1km,MH_1km,Pa_1km,Pe_1km,TWS_1km,LD_1km,
                         MD_1km,HD_1km, Alt_1km,Slope_1km,LD_Pa_1km,
                         LD_alt_1km,LD_slope_1km,LD_Pa_alt_1km,
                         LD_Pa_slope_1km,LD_Pa_alt_slope_1km,Pa_HD_1km,
                         Pa_alt_1km,Pa_MD_1km,Pa_HD_alt_1km,Pa_MD_alt_1km,
                         Pa_MD_HD_alt_1km,Pa_MD_HD_1km,TWS_CF_1km,
                         TWS_alt_1km, TWS_CF_alt_1km,HD_Pe_1km,HD_slope_1km,
                         HD_Pe_slope_1km, Alt_Pe_1km, LDPa_1km,global_1km)

# confidence set
conf_set_1km <- models_1km[models_1km$delta<2, ]


# 50m riparian buffer models ----------------------------------------------

# single variable models
CF_50m<- lm(Average_d13C ~ CF, data = riparian_50m)
MH_50m<- lm(Average_d13C ~ MH, data = riparian_50m)
Pa_50m<- lm(Average_d13C ~ Pastures, data = riparian_50m)
Pe_50m<- lm(Average_d13C ~ Peat, data = riparian_50m)
TWS_50m<- lm(Average_d13C ~ TWS, data = riparian_50m)
LD_50m<- lm(Average_d13C ~ LD, data = riparian_50m)
MD_50m<- lm(Average_d13C ~ MD, data = riparian_50m)
HD_50m<- lm(Average_d13C ~ HD, data = riparian_50m)
Alt_50m<- lm(Average_d13C ~ Alt_mean, data = riparian_50m)
Slope_50m<- lm(Average_d13C ~ Slope_mean, data = riparian_50m)
# multivariable additive models
LD_Pa_50m<- lm(Average_d13C ~LD+Pastures, data = riparian_50m)
LD_alt_50m<- lm(Average_d13C ~LD+Alt_mean, data = riparian_50m)
LD_slope_50m<- lm(Average_d13C ~Slope_mean, data = riparian_50m)
LD_Pa_alt_50m<- lm(Average_d13C ~LD+Pastures+Alt_mean, data = riparian_50m)
LD_Pa_slope_50m<- lm(Average_d13C ~LD+Pastures+Slope_mean, data = riparian_50m)
LD_Pa_alt_slope_50m<- lm(Average_d13C ~LD+Pastures+Alt_mean+Slope_mean, 
                          data = riparian_50m)
Pa_HD_50m <- lm(Average_d13C~Pastures+HD, data = riparian_50m)
Pa_alt_50m <- lm(Average_d13C~Pastures+Alt_mean, data = riparian_50m)
Pa_MD_50m<- lm(Average_d13C~Pastures+MD, data = riparian_50m)
Pa_HD_alt_50m <- lm(Average_d13C~Pastures+HD+Alt_mean, data = riparian_50m)
Pa_MD_alt_50m <- lm(Average_d13C~Pastures+HD+Alt_mean, data = riparian_50m)
Pa_MD_HD_alt_50m <- lm(Average_d13C~Pastures+MD+HD+Alt_mean, data = riparian_50m)
Pa_MD_HD_50m <- lm(Average_d13C~Pastures+MD+HD, data = riparian_50m)
TWS_CF_50m<- lm(Average_d13C~TWS+CF, data=riparian_50m)
TWS_alt_50m<- lm(Average_d13C~TWS+Alt_mean, data=riparian_50m)
TWS_CF_alt_50m<- lm(Average_d13C~TWS+CF+Alt_mean, data=riparian_50m)
HD_Pe_50m<- lm(Average_d13C~HD+Peat, data=riparian_50m)
HD_slope_50m<- lm(Average_d13C~HD+Slope_mean, data=riparian_50m)
HD_Pe_slope_50m<- lm(Average_d13C~HD+Peat+Slope_mean, data=riparian_50m)
Alt_Pe_50m<- lm(Average_d13C~Alt_mean+Peat, data=riparian_50m)
global_50m <- lm(Average_d13C~LD+Pastures+Slope_mean+Alt_mean+HD+MD+TWS+CF+
                    Peat, data=riparian_50m)
# multivariable models with interactions
LDPa_50m <- lm(Average_d13C~LD*Pastures, data = riparian_50m)

#model selection
models_50m <- model.sel(CF_50m,MH_50m,Pa_50m,Pe_50m,TWS_50m,LD_50m,
                         MD_50m,HD_50m, Alt_50m,Slope_50m,LD_Pa_50m,
                         LD_alt_50m,LD_slope_50m,LD_Pa_alt_50m,
                         LD_Pa_slope_50m,LD_Pa_alt_slope_50m,Pa_HD_50m,
                         Pa_alt_50m,Pa_MD_50m,Pa_HD_alt_50m,Pa_MD_alt_50m,
                         Pa_MD_HD_alt_50m,Pa_MD_HD_50m,TWS_CF_50m,
                         TWS_alt_50m, TWS_CF_alt_50m,HD_Pe_50m,HD_slope_50m,
                         HD_Pe_slope_50m, Alt_Pe_50m, LDPa_50m,global_50m)

# confidence set
conf_set_50m <- models_50m[models_50m$delta<2, ]


# 100m riparian buffer models ---------------------------------------------

# single variable models
CF_100m<- lm(Average_d13C ~ CF, data = riparian_100m)
MH_100m<- lm(Average_d13C ~ MH, data = riparian_100m)
Pa_100m<- lm(Average_d13C ~ Pastures, data = riparian_100m)
Pe_100m<- lm(Average_d13C ~ Peat, data = riparian_100m)
TWS_100m<- lm(Average_d13C ~ TWS, data = riparian_100m)
LD_100m<- lm(Average_d13C ~ LD, data = riparian_100m)
MD_100m<- lm(Average_d13C ~ MD, data = riparian_100m)
HD_100m<- lm(Average_d13C ~ HD, data = riparian_100m)
Alt_100m<- lm(Average_d13C ~ Alt_mean, data = riparian_100m)
Slope_100m<- lm(Average_d13C ~ Slope_mean, data = riparian_100m)
# multivariable additive models
LD_Pa_100m<- lm(Average_d13C ~LD+Pastures, data = riparian_100m)
LD_alt_100m<- lm(Average_d13C ~LD+Alt_mean, data = riparian_100m)
LD_slope_100m<- lm(Average_d13C ~Slope_mean, data = riparian_100m)
LD_Pa_alt_100m<- lm(Average_d13C ~LD+Pastures+Alt_mean, data = riparian_100m)
LD_Pa_slope_100m<- lm(Average_d13C ~LD+Pastures+Slope_mean, data = riparian_100m)
LD_Pa_alt_slope_100m<- lm(Average_d13C ~LD+Pastures+Alt_mean+Slope_mean, 
                          data = riparian_100m)
Pa_HD_100m <- lm(Average_d13C~Pastures+HD, data = riparian_100m)
Pa_alt_100m <- lm(Average_d13C~Pastures+Alt_mean, data = riparian_100m)
Pa_MD_100m<- lm(Average_d13C~Pastures+MD, data = riparian_100m)
Pa_HD_alt_100m <- lm(Average_d13C~Pastures+HD+Alt_mean, data = riparian_100m)
Pa_MD_alt_100m <- lm(Average_d13C~Pastures+HD+Alt_mean, data = riparian_100m)
Pa_MD_HD_alt_100m <- lm(Average_d13C~Pastures+MD+HD+Alt_mean, data = riparian_100m)
Pa_MD_HD_100m <- lm(Average_d13C~Pastures+MD+HD, data = riparian_100m)
TWS_CF_100m<- lm(Average_d13C~TWS+CF, data=riparian_100m)
TWS_alt_100m<- lm(Average_d13C~TWS+Alt_mean, data=riparian_100m)
TWS_CF_alt_100m<- lm(Average_d13C~TWS+CF+Alt_mean, data=riparian_100m)
HD_Pe_100m<- lm(Average_d13C~HD+Peat, data=riparian_100m)
HD_slope_100m<- lm(Average_d13C~HD+Slope_mean, data=riparian_100m)
HD_Pe_slope_100m<- lm(Average_d13C~HD+Peat+Slope_mean, data=riparian_100m)
Alt_Pe_100m<- lm(Average_d13C~Alt_mean+Peat, data=riparian_100m)
global_100m <- lm(Average_d13C~LD+Pastures+Slope_mean+Alt_mean+HD+MD+TWS+CF+
                    Peat, data=riparian_100m)
# multivariable models with interactions
LDPa_100m <- lm(Average_d13C~LD*Pastures, data = riparian_100m)

#model selection
models_100m <- model.sel(CF_100m,MH_100m,Pa_100m,Pe_100m,TWS_100m,LD_100m,
                         MD_100m,HD_100m, Alt_100m,Slope_100m,LD_Pa_100m,
                         LD_alt_100m,LD_slope_100m,LD_Pa_alt_100m,
                         LD_Pa_slope_100m,LD_Pa_alt_slope_100m,Pa_HD_100m,
                         Pa_alt_100m,Pa_MD_100m,Pa_HD_alt_100m,Pa_MD_alt_100m,
                         Pa_MD_HD_alt_100m,Pa_MD_HD_100m,TWS_CF_100m,
                         TWS_alt_100m, TWS_CF_alt_100m,HD_Pe_100m,HD_slope_100m,
                         HD_Pe_slope_100m, Alt_Pe_100m, LDPa_100m,global_100m)

# confidence set
conf_set_100m <- models_100m[models_100m$delta<2, ]

