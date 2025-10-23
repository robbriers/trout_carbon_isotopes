
# Results section 3.2 -----------------------------------------------------

library(MuMIn)
library(ggplot2)
library(ggeffects)

# Read in data

catchment_data <- read.csv("catchment_data.csv", stringsAsFactors = TRUE)

# Single variable models
CF<- glm(Average_d13C ~ CF, data = catchment_data)
MH<- glm(Average_d13C ~ MH, data = catchment_data)
Pa<- glm(Average_d13C ~ Pastures, data = catchment_data)
Pe<- glm(Average_d13C ~ Peat, data = catchment_data)
TWS<- glm(Average_d13C ~ TWS, data = catchment_data)
LD<- glm(Average_d13C ~ Low.drainage, data = catchment_data)
MD<- glm(Average_d13C ~ Mixed.drainage, data = catchment_data)
HD<- glm(Average_d13C ~ High.drainage, data = catchment_data)
Alt<- glm(Average_d13C ~ Alt_mean, data = catchment_data)
Slope<- glm(Average_d13C ~ Slope_mean, data = catchment_data)

# Multivariable additive models
LD_Pa<- glm(Average_d13C ~Low.drainage+Pastures, data = catchment_data)
LD_alt<- glm(Average_d13C ~Low.drainage+Alt_mean, data = catchment_data)
LD_slope<- glm(Average_d13C ~Slope_mean, data = catchment_data)
LD_Pa_alt<- glm(Average_d13C ~Low.drainage+Pastures+Alt_mean, 
                data = catchment_data)
LD_Pa_slope<- glm(Average_d13C ~Low.drainage+Pastures+Slope_mean, 
                  data = catchment_data)
LD_Pa_alt_slope<- glm(Average_d13C ~Low.drainage+Pastures+Alt_mean+Slope_mean, 
                      data = catchment_data)
Pa_HD <- glm(Average_d13C~Pastures+High.drainage, data = catchment_data)
Pa_alt <- glm(Average_d13C~Pastures+Alt_mean, data = catchment_data)
Pa_MD<- glm(Average_d13C~Pastures+Mixed.drainage, data = catchment_data)
Pa_HD_alt <- glm(Average_d13C~Pastures+High.drainage+Alt_mean, 
                 data = catchment_data)
Pa_MD_alt <- glm(Average_d13C~Pastures+High.drainage+Alt_mean, 
                 data = catchment_data)
Pa_MD_HD_alt <- glm(Average_d13C~Pastures+Mixed.drainage+High.drainage+Alt_mean,
                    data = catchment_data)
Pa_MD_HD <- glm(Average_d13C~Pastures+Mixed.drainage+High.drainage, 
                data = catchment_data)
TWS_CF<- glm(Average_d13C~TWS+CF, data=catchment_data)
TWS_alt<- glm(Average_d13C~TWS+Alt_mean, data=catchment_data)
TWS_CF_alt<- glm(Average_d13C~TWS+CF+Alt_mean, data=catchment_data)
HD_Pe<- glm(Average_d13C~High.drainage+Peat, data=catchment_data)
HD_slope<- glm(Average_d13C~High.drainage+Slope_mean, data=catchment_data)
HD_Pe_slope<- glm(Average_d13C~High.drainage+Peat+Slope_mean, 
                  data=catchment_data)
Alt_Pe<- glm(Average_d13C~Alt_mean+Peat, data=catchment_data)
global <- glm(Average_d13C~Low.drainage+Pastures+Slope_mean+Alt_mean
              +High.drainage+Mixed.drainage+TWS+CF+Peat, data=catchment_data)

# Multivariable model with interactions
LDPa <- glm(Average_d13C~Low.drainage*Pastures, data = catchment_data)


# Model selection
model.sel(CF,MH,Pa,Pe,TWS,LD,MD,HD,Alt,Slope,LD_Pa,LD_alt,LD_slope,LD_Pa_alt,
          LD_Pa_slope,LD_Pa_alt_slope, Pa_HD,Pa_alt,Pa_MD,Pa_HD_alt,Pa_MD_alt,
          Pa_MD_HD_alt,Pa_MD_HD, TWS_CF,TWS_alt,TWS_CF_alt, HD_Pe,HD_slope,
          HD_Pe_slope, Alt_Pe, LDPa,global)

# Create Figure 5 
# Use ggpredict to derive predictions from model over required terms and scale
predictions <- ggpredict(LDPa, terms = c("Low.drainage", "Pastures [0,50,100]"))

# Create model plot
p <- ggplot()+
  geom_ribbon(data=predictions, (aes(x=x, ymin=conf.low, ymax=conf.high, 
                                     fill=group, alpha=0.1)))+
  scale_fill_viridis_d(option="plasma", begin = 0.9, end = 0.5, alpha = 1)+
  geom_line(data=predictions, (aes(x=x, y=predicted, colour=group)), 
            linewidth=1)+
  scale_colour_viridis_d(option="plasma", begin = 0.7, end = 0.1)+
  geom_point(data=catchment_data, aes(Low.drainage, Average_d13C, 
                                      size=Pastures, alpha=0.4))+
  scale_size_continuous(range = c(2, 8))+
  guides(alpha="none")+
  guides(size="none")+
  guides(fill="none")+
  labs(x = "Percentage of catchment with low drainage soils",
       y = expression(paste(delta^{13}, "C(‰)")),
       title = "",
       colour = "% Pastures")+
  theme_classic(base_size = 16)
# show the plot
p
