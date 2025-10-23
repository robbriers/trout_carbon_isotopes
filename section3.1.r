
# Results section 3.1 -----------------------------------------------------

library(ggplot2)

# load data files

site_isotope_data<-read.csv("site_isotope_data.csv", 
                            stringsAsFactors = TRUE)
trout_baetis_data <- read.csv("trout_baetis_data.csv", 
                              stringsAsFactors = TRUE)

# Between-site isotope comparison -----------------------------------------

# Comparison of medians

kruskal.test(d13C~Site, data=site_isotope_data)
kruskal.test(d15N~Site, data=site_isotope_data)

# Figures

# Reorder Site based on median d13C
site_isotope_data$Site <- reorder(site_isotope_data$Site, 
                                  site_isotope_data$d13C, median, na.rm = TRUE)

# Create Fig 3a
trout_c_plot <- ggplot(site_isotope_data, aes(x = Site, y = d13C, 
                                              fill = Catchment)) +
  geom_boxplot() +
  theme_classic(base_size = 18) +
  theme(
    text = element_text(size = 18),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom")+
  ylab(expression(paste(delta^{13}, "C (\u2030)"))) +
  xlab("Site (Ordered by δ¹³C Median)") +
  labs(fill = "Catchment") +
  scale_fill_brewer(palette = "Set1")
trout_c_plot


# Create Fig 3b
site_isotope_data$Site <- reorder(site_isotope_data$Site, 
                                  site_isotope_data$d15N, median, na.rm = TRUE)
trout_n_plot <- ggplot(site_isotope_data, aes(x = Site, y = d15N, 
                                              fill = Catchment)) +
  geom_boxplot() +
  theme_classic(base_size = 18) +
  theme(
    text = element_text(size = 18),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom")+
  ylab(expression(paste(delta^{15}, "N (\u2030)"))) +
  xlab("Site (Ordered by δ¹⁵N Median)") +
  labs(fill = "Catchment") +
  scale_fill_brewer(palette = "Set1")
trout_n_plot

# Trout-Baetis isotope comparison -----------------------------------------

# Linear model analysis
m1 <- lm(Trout.Average.d13C~Baetis.d13C, data=trout_baetis_data)
summary(m1)

# Assessing between catchment variation
t.test(Trout.Average.d13C~Catchment, data=trout_baetis_data)
t.test(Baetis.d13C~Catchment, data=trout_baetis_data)


# Create Figure 4
shapes <- c( 16, 17)
colors <- c("red", "blue")

plot(Trout.Average.d13C ~ Baetis.d13C, data=trout_baetis_data, 
     ylab = expression(paste(delta^{13}, "C(\u2030) of Trout") ), 
     xlab = expression(paste(delta^{13}, "C(\u2030) of Baetis") ),
     pch = shapes,
     col=colors,
     cex = 1.5,
     lwd = 2,
     bty="n",
     xlim=c(-42, -26),
     ylim=c(-38, -26),
     xaxt="n")
axis(side = 1, at = seq(-42, -26, by = 2))
legend("topleft", legend = sort(unique(trout_baetis_data$Catchment)), 
       pch = shapes, col = colors, 
       bg = "transparent",bty="n")
abline(m1, lwd=1.5, col="black")
