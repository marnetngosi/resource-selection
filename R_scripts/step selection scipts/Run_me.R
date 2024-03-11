###########################################
# Time-varying habitat selection analysis #
###########################################

# Romain Dejeante ; Marion Valeix ; Simon Chamaill�-Jammes

# Appendix S1

library(here)
library(raster)
library(adehabitatHR)
library(dplyr)
library(amt)
library(tidyr)
library(dynamichazard)
library(ggplot2)
library(viridis)
library(cowplot)

# Implementing time-varying HSA
source(here("./Appendix_function.R"))

# Import simulated movement and landscape
simulated_mvt = readRDS(here("./simulated_mvt.rds"))
head(simulated_mvt)
simulated_landscape = readRDS(here("./simulated_landscape.rds"))

# Polygon used to generate random points
xy = SpatialPointsDataFrame(coords = simulated_mvt[,c("x","y")],
                            data = data.frame("name" = simulated_mvt$id))
spPolygon <- getverticeshr(kernelUD(xy[,1], h="href"),
                           percent = 99)

# Generate random points within polygon
data_track_rsf = prepRSF(simulated_mvt, spPolygon, n_control = 100)
head(data_track_rsf)

# Extract environmental layers
data_track_rsf <- data_track_rsf %>% 
  make_track(x_, y_, t_, id, all_cols=T) %>% 
  extract_covariates(simulated_landscape) %>% 
  drop_na()
data_track_rsf = data.frame(data_track_rsf)
head(data_track_rsf)

# Time-varying RSF
f1 <- ddhazard(Surv(t_, case_) ~ layer,
                      data_track_rsf,
                      by = 1,
                      max_T = max(data_track_rsf$t_),
                      Q_0 = diag(1, 2),
                      Q = diag(1, 2),
                      control = ddhazard_control(eps = 0.01))

# basic plot
plot(f1, cov_index = 2)

# Extract the estimated selection strength from the ddhazard model
data_tRSF = Outcome_tRSF(f1)
head(data_tRSF)

# Plot time-varying RSF
plot_tRSF(data_tRSF)

# Compare the estimated and expected selection strengths
data_tRSF = left_join(simulated_mvt,data_tRSF, by = c("t" = "t"))
g0_f1 = ggplot(data = data_tRSF)+
  geom_ribbon(aes(x=t, y=layer, ymin = layer_lower95,
                  ymax = layer_upper95, fill = "Estimated"), alpha = 0.5)+
  geom_line(aes(x=t, y=beta, col = "Expected"), lwd = 1.1) +
  geom_line(aes(x=t, y=layer, col = "Estimated"), lwd = 1.1)+
  theme_bw()+
  ylab("Habitat selection strength")+
  scale_color_viridis(discrete = T) +
  scale_fill_viridis(discrete = T)+guides(fill = "none")
g_map = ggplot()+
  geom_raster(data = gplot_data(simulated_landscape), aes(x,y,fill=value))+
  scale_fill_viridis(name = "Layer")+
  theme_classic()+
  guides(fill = "none")
g1 =  g_map +
  geom_path(data = data_tRSF, aes(x,y, col = beta), lwd = 1.2)+
  scale_color_viridis(name = "Expected")
g2 =  g_map +
  geom_path(data = data_tRSF, aes(x,y, col = layer), lwd = 1.2)+
  scale_color_viridis(name = "Estimated")
plot_grid(plot_grid(g1, g2, ncol = 2),g0_f1, ncol=1)


