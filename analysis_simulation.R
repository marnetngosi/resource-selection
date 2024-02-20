library(ggplot2)
library(glmmTMB)
library(dplyr)
# library(sjPlot)
library(viridis)
library(cowplot)
library(here)
library(tidyr)

step_vec = c(20,250) # nstep during a beta = cst phasis
n_phasis = round(500/step_vec)  # here we considere it as the number of phases per individual
n_ind = 100
n_repet = 2
Q0 = c(0.1,2) #c(0.01, 0.02, 0.05, 0.1, 0.3, 0.5, 0.8, 1, 1.5, 2)
Q = c(0.1,2) #c(0.01, 0.02, 0.05, 0.1, 0.3, 0.5, 0.8, 1, 1.5, 2)
beta_min = -5
beta_max = 5
# 
# data_rsf_parameter = data.frame()
# 
# for (r in 1:n_repet){
#   # For each repet, we simulate n_ind individuals over 500 time steps
#   print(r)
#   for (s in 1:length(step_vec)){
#     nstep = step_vec[s]
#     for (q in 1:length(Q)){
#       
#       data_tRSF = readRDS(here(paste0("./0_Ecology_submission/Data/simulation/data_tRSF",nstep,"_repet_", r)))
#       data_rsf_parameter = rbind(data_rsf_parameter,data_tRSF)
#     }
#   }
# }

data_rsf_parameter <- readRDS(here("./0_Ecology_submission/Data/simulation/data_rsf_parameter.rds"))
data_rsf_parameter <- data_rsf_parameter %>% 
  mutate(nstep = factor(nstep, levels=c('250', '20')))

################################################################################
#################### ILLUSTRAION MODEL EVALUATION ##############################
################################################################################

# Figure 1 : example of simulation - comparison between 1 expected and the mean estimation over n_ind individuals
data_rsf_parameter_example <- data_rsf_parameter %>% 
  filter(repet == 1) %>% 
  filter(Q %in% c(0.1, 2)) %>% 
  mutate(nstep = factor(nstep, levels=c('250', '20'))) %>% 
  group_by(Q, nstep, t) %>%
  summarise(beta_mean_t = mean(beta_habitat, na.rm = TRUE),
            sd_beta = sd(beta_habitat, na.rm = TRUE),
            n.beta = n(),
            beta = beta) %>%
  mutate(se.beta = sd_beta / sqrt(n.beta),
         beta_mean_t_lower = beta_mean_t - qt(1 - (0.05 / 2), n.beta - 1) * se.beta,
         beta_mean_t_upper = beta_mean_t + qt(1 - (0.05 / 2), n.beta - 1) * se.beta,
         beta_sd_inf = beta_mean_t - sd_beta,
         beta_sd_sup = beta_mean_t + sd_beta) %>% 
  # summarise(beta_mean_t = mean(beta_habitat, na.rm = T),
  #           beta_mean_t_lower = as.numeric(mean(beta_habitat, na.rm = T)+sqrt(sd(beta_habitat, na.rm = T))%*%t(qnorm(c(0.025)))),
  #           beta_mean_t_upper = as.numeric(mean(beta_habitat, na.rm = T)+sqrt(sd(beta_habitat, na.rm = T))%*%t(qnorm(c(0.975)))),
  #           beta = beta) %>% 
  distinct()


ga = ggplot(data_rsf_parameter_example %>% filter(Q == 0.1) %>% filter(nstep == 250))+
  geom_ribbon( aes(x=t, y=beta_mean_t, ymin = beta_sd_inf,
                   ymax = beta_sd_sup, fill = "Estimated"), alpha = 0.3)+
  geom_ribbon( aes(x=t, y=beta_mean_t, ymin = beta_mean_t_lower,
                   ymax = beta_mean_t_upper, fill = "Estimated"), alpha = 0.5)+
  geom_line(aes(x=t, y=beta, col = "Expected"), lwd = 1.1) +
  geom_line(aes(x=t, y=beta_mean_t, col = "Estimated"), lwd = 1)+
  theme_classic()+
  theme( plot.title = element_text(size = 10),
         plot.subtitle = element_text(size = 10),
         strip.text = element_text(size=10, face="bold"),
         axis.title=element_text(size=10, face="bold"),
         axis.text=element_text(size=10),
         legend.text = element_text(size=10),
         legend.title = element_blank())+
  ylab("Coefficient")+
  xlab("Time")+
  geom_hline(yintercept = 0, linetype = "dashed", lwd = 1)+
  # scale_color_manual(values = c("#000000", "#fdbb84"))+
  # scale_fill_manual(values = c("#000000", "#fdbb84"))+
  scale_color_viridis(discrete = T) +
  scale_fill_viridis(discrete = T)+
  guides(fill = "none")+
  theme(legend.position = c(0.22, 0.9),
        legend.background = element_rect(fill = "transparent"),
        strip.background = element_rect(fill=alpha("#542788",0.2)))+
  labs(title = "Q = 0.1   Rare change scenario")+
  ylim(-6.5,6)



gb = ggplot(data_rsf_parameter_example %>% filter(Q == 0.1) %>% filter(nstep == 20))+
  geom_ribbon( aes(x=t, y=beta_mean_t, ymin = beta_sd_inf,
                   ymax = beta_sd_sup, fill = "Estimated"), alpha = 0.3)+
  geom_ribbon( aes(x=t, y=beta_mean_t, ymin = beta_mean_t_lower,
                   ymax = beta_mean_t_upper, fill = "Estimated"), alpha = 0.5)+
  geom_line(aes(x=t, y=beta, col = "Expected"), lwd = 1.1) +
  geom_line(aes(x=t, y=beta_mean_t, col = "Estimated"), lwd = 1)+
  theme_classic()+
  theme( plot.title = element_text(size = 10),
         plot.subtitle = element_text(size = 10),
         strip.text = element_text(size=10, face="bold"),
         axis.title=element_text(size=10, face="bold"),
         axis.text=element_text(size=10),
         legend.text = element_text(size=10),
         legend.title = element_blank())+
  ylab("Coefficient")+
  xlab("Time")+
  geom_hline(yintercept = 0, linetype = "dashed", lwd = 1)+
  # scale_color_manual(values = c("#000000", "#fdbb84"))+
  # scale_fill_manual(values = c("#000000", "#fdbb84"))+
  scale_color_viridis(discrete = T) +
  scale_fill_viridis(discrete = T)+
  guides(fill = "none")+
  theme(legend.position = "none",
        legend.background = element_rect(fill = "transparent"),
        strip.background = element_rect(fill=alpha("#542788",0.2)))+
  labs(title = "Q = 0.1   Frequent change scenario")+
  ylim(-5,5)



gc = ggplot(data_rsf_parameter_example %>% filter(Q == 2) %>% filter(nstep == 250))+
  geom_ribbon( aes(x=t, y=beta_mean_t, ymin = beta_sd_inf,
                   ymax = beta_sd_sup, fill = "Estimated"), alpha = 0.3)+
  geom_ribbon( aes(x=t, y=beta_mean_t, ymin = beta_mean_t_lower,
                   ymax = beta_mean_t_upper, fill = "Estimated"), alpha = 0.5)+
  geom_line(aes(x=t, y=beta, col = "Expected"), lwd = 1.1) +
  geom_line(aes(x=t, y=beta_mean_t, col = "Estimated"), lwd = 1)+
  theme_classic()+
  theme( plot.title = element_text(size = 10),
         plot.subtitle = element_text(size = 10),
         strip.text = element_text(size=10, face="bold"),
         axis.title=element_text(size=10, face="bold"),
         axis.text=element_text(size=10),
         legend.text = element_text(size=10),
         legend.title = element_blank())+
  ylab("Coefficient")+
  xlab("Time")+
  geom_hline(yintercept = 0, linetype = "dashed", lwd = 1)+
  # scale_color_manual(values = c("#000000", "#fdbb84"))+
  # scale_fill_manual(values = c("#000000", "#fdbb84"))+
  scale_color_viridis(discrete = T) +
  scale_fill_viridis(discrete = T)+
  guides(fill = "none")+
  theme(legend.position = "none",
        legend.background = element_rect(fill = "transparent"),
        strip.background = element_rect(fill=alpha("#542788",0.2)))+
  labs(title = "Q = 2   Rare change scenario")+
  ylim(-6.5,6)



gd = ggplot(data_rsf_parameter_example %>% filter(Q == 2) %>% filter(nstep == 20))+
  geom_ribbon( aes(x=t, y=beta_mean_t, ymin = beta_sd_inf,
                   ymax = beta_sd_sup, fill = "Estimated"), alpha = 0.3)+
  geom_ribbon( aes(x=t, y=beta_mean_t, ymin = beta_mean_t_lower,
                   ymax = beta_mean_t_upper, fill = "Estimated"), alpha = 0.5)+
  geom_line(aes(x=t, y=beta, col = "Expected"), lwd = 1.1) +
  geom_line(aes(x=t, y=beta_mean_t, col = "Estimated"), lwd = 1)+
  theme_classic()+
  theme( plot.title = element_text(size = 10),
         plot.subtitle = element_text(size = 10),
         strip.text = element_text(size=10, face="bold"),
         axis.title=element_text(size=10, face="bold"),
         axis.text=element_text(size=10),
         legend.text = element_text(size=10),
         legend.title = element_blank())+
  ylab("Coefficient")+
  xlab("Time")+
  geom_hline(yintercept = 0, linetype = "dashed", lwd = 1)+
  # scale_color_manual(values = c("#000000", "#fdbb84"))+
  # scale_fill_manual(values = c("#000000", "#fdbb84"))+
  scale_color_viridis(discrete = T) +
  scale_fill_viridis(discrete = T)+
  guides(fill = "none")+
  theme(legend.position = "none",
        legend.background = element_rect(fill = "transparent"),
        strip.background = element_rect(fill=alpha("#542788",0.2)))+
  labs(title = "Q = 2   Frequent change scenario")+
  ylim(-5,5)



g0 = plot_grid(ga,gb,gc,gd, ncol = 2,
               labels = c("(a)", "(c)", "(b)", "(d)"))
g0

png(here("./0_Ecology_submission/Figure/Evaluation_example2.png"), height = 14, width = 18, units = "cm", res = 600)
g0
dev.off()

g0 = plot_grid(ga,gc,gb,gd, ncol = 1,
               labels = c("(a)", "(b)", "(c)", "(d)"))
g0

png(here("./0_Ecology_submission/Figure/Evaluation_example2.png"), height = 24, width = 8.5, units = "cm", res = 600)
g0
dev.off()


################################################################################
####################### ANALYSIS POPULATION LEVEL ##############################
################################################################################

data_rsf_parameter_mean <- data_rsf_parameter %>% 
  group_by(repet, Q, nstep, t) %>% 
  summarise(beta_mean_t = mean(beta_habitat, na.rm = T),
            beta_mean_t_lower = as.numeric(mean(beta_habitat, na.rm = T)+sqrt(sd(beta_habitat, na.rm = T))%*%t(qnorm(c(0.025)))),
            beta_mean_t_upper = as.numeric(mean(beta_habitat, na.rm = T)+sqrt(sd(beta_habitat, na.rm = T))%*%t(qnorm(c(0.975)))),
            beta = beta) %>% 
  distinct() %>% 
  ungroup()


# INDEX MEASURE = LINEAR REGRESSION
data_rsf_parameter_mean <- data_rsf_parameter_mean %>% 
  mutate(Q = factor(Q),
         nstep = factor(nstep),
         repet = factor(repet))
model_evaluation = glmmTMB(beta_mean_t ~ beta*Q*nstep + (1|repet),
                           data_rsf_parameter_mean)
summary(model_evaluation)

newdata <- expand.grid(
  beta =  seq(min(data_rsf_parameter_mean$beta, na.rm = T),max(data_rsf_parameter_mean$beta, na.rm = T), 0.5),
  Q = factor(c(0.1,2)),
  nstep  = factor(c(250,20)),
  repet = factor(1))
levels(newdata$nstep) <- c("250", "20") # so same as:

preds_SR <-predict(model_evaluation,newdata,type="response", re.form = NA, se = T)
ci <- preds_SR$fit+(preds_SR$se)%*%t(qnorm(c(0.025,0.5,0.975)))
dimnames(ci)[[2]]<-c("lower95", "est", "upper95")
SR = (ci) # level = 0
pred_df <- cbind(newdata,SR)


dose.labs <- c("Q = 0.1", "Q = 2")
names(dose.labs) <- c("0.1", "2")
nstep.labs <- c("Rare change scenario", "Frequent change scenario")
names(nstep.labs) <- c("250", "20")

g1 = ggplot()+
  geom_point(data = data_rsf_parameter_mean %>%  filter(row_number() %% 30 == 1),
             aes(x=beta, y = beta_mean_t), size = 0.1)+
  geom_ribbon(data = pred_df,
              aes(x=beta, 
                  y=est,
                  ymin = lower95,
                  ymax = upper95),
              alpha = 0.3)+
  geom_line(data = pred_df,
            aes(x=beta, 
                y=est),
            lwd = 1.1)+
  geom_abline(intercept = 0, slope = 1, lwd = 0.5, linetype = "dashed")+
  guides(linetype=guide_legend(keywidth = 2, keyheight = 1))+
  theme_bw()+
  facet_grid(Q~nstep, 
             labeller = labeller(Q = dose.labs, nstep = nstep.labs)) +
  ylab("Estimated coefficient")+
  xlab("Expected coefficient")+
  theme(
    plot.title = element_blank(),
    axis.title=element_text(size=10, face="bold"),
    axis.text=element_text(size=10),
    legend.text = element_text(size=10),
    legend.title = element_text(size=10, face="bold"),
    strip.text = element_text(size=10, face="bold"))

g1
png(here("./0_Ecology_submission/Figure/Evaluation_supplemental_1.png"), height = 14, width = 18, units = "cm", res = 600)
g1
dev.off()


################################################################################
############################# Correlation  #####################################
################################################################################

data_rsf_parameter_q_range = readRDS(here("./0_Ecology_submission/Data/simulation/data_rsf_parameter_q_range.rds"))
data_rsf_parameter_q_range <- data_rsf_parameter_q_range %>% filter(repet < 40)
beta_correlation = data_rsf_parameter_q_range %>%
  group_by(repet, Q, nstep, t) %>%
  summarise(beta_mean_t = mean(beta_habitat, na.rm = TRUE),
            beta = beta) %>% 
  distinct() %>%
  ungroup() %>%
  drop_na() %>%
  group_by(repet, Q, nstep) %>%
  summarise(corr = cor(beta,beta_mean_t)) %>%
  ungroup() %>%
  group_by(Q, nstep) %>%
  summarise(correlation_mean = mean(corr, na.rm = TRUE),
            sd.corr = sd(corr, na.rm = TRUE),
            n.corr = n()) %>%
  mutate(se.corr = sd.corr / sqrt(n.corr),
         correlation_low = correlation_mean - qt(1 - (0.05 / 2), n.corr - 1) * se.corr,
         correlation_up = correlation_mean + qt(1 - (0.05 / 2), n.corr - 1) * se.corr)

g1 = ggplot(beta_correlation) +
  geom_ribbon( aes(x=Q, y=correlation_mean, ymin = correlation_low,
                   ymax = correlation_up, group = factor(nstep)), alpha = 0.5)+
  geom_line(aes(x=Q, y=correlation_mean, linetype = factor(nstep)), lwd = 0.5)+
  ylim(0,1)+
  theme_bw()+
  theme(
    plot.title = element_blank(),
    axis.title=element_text(size=10, face="bold"),
    axis.text=element_text(size=10),
    legend.text = element_text(size=10),
    legend.title = element_blank(),
    legend.position = c(0.5, 0.2),
    legend.background = element_rect(fill = "transparent"))+
  scale_color_viridis(discrete = T) +
  scale_fill_viridis(discrete = T)+guides(fill = "none")+
  ylab("Correlation")+
  xlab("Wiggliness parameter Q")+
  scale_linetype(labels = c("Frequent change scenario", "Rare change scenario"))

g1

png(here("./0_Ecology_submission/Figure/Evaluation_supplemental_3.png"), height = 8, width = 8.5, units = "cm", res = 600)
g1
dev.off()


library(dynamichazard)
library(gridExtra)
library(grid)
library(fields)
library(here)
library(raster)
# library(tictoc)
library(ggplot2)
library(localGibbs)
library(dplyr)
library(amt)
library(adehabitatHR)
library(dplyr)
library(tidyr)
library(splines)
library(aTSA)
library(raster)
library(amt)
data_mvt_simulated = data.frame()

r = 3  # For each repet, we simulate n_ind individuals over 500 time steps
  print(r)
  for (s in 1:length(step_vec)){
    nstep = step_vec[s]
    
    simulated_data = readRDS(here(paste0("./0_Ecology_submission/Data/simulation/simulated_data_nstep",nstep,"_repet_", r)))
    habitat_raster = readRDS(here(paste0("./0_Ecology_submission/Data/simulation/habitat_raster",nstep,"_repet_", r)))
  
    data_track_rsf <- simulated_data %>% 
      make_track(x, y, step, 
                 id = id,
                 all_cols=T) %>% 
      extract_covariates(habitat_raster)
    
    data_track_rsf = data.frame(data_track_rsf)
    
    data_track_rsf_mean <- data_track_rsf %>% 
      group_by(t_) %>% 
      mutate(mean_use = mean(layer),
             low_use = mean(layer)-2*sd(layer),
             up_use = mean(layer)+2*sd(layer)) %>% 
      filter(id == 2)
    
    p1 = plot_grid(
      ggplot(data_track_rsf %>% filter(id < 2))+
        geom_line(aes(x=t_, y=beta)),
      ggplot(data_track_rsf %>% filter(id == 2))+
        geom_point(aes(x=t_, y=layer)),
      ggplot(data_track_rsf)+
        geom_point(aes(x=t_, y=layer)),
      ggplot(data_track_rsf_mean)+
        geom_line(aes(x=t_, y=mean_use))+
        geom_ribbon(aes(x=t_, ymin = low_use, ymax = up_use), alpha = 0.5),
    ncol = 1)
    png(here(paste0("./0_Ecology_submission/Figure/Evaluation_supplemental_id1_",nstep,".png")), height = 24, width = 18, units = "cm", res = 600)
    print(p1)
    dev.off() 
    
    # Available locations
    n_control = 100
    coords = subset(simulated_data, id == 2)[,c("x","y")]
    data2 = data.frame("name" = subset(simulated_data, id == 2)$id)
    xy = SpatialPointsDataFrame(coords = coords,data = data2)
    kud_id1 <- kernelUD(xy[,1], h="href")
    HR_id1 <- getverticeshr(kud_id1, percent = 99, unout = "km2") # from 50% UD
    
    # Make track data
    data_track <- data_track_rsf_mean %>% 
      mutate(case_ = TRUE)
    
    data_track_rd <- random_points(HR_id1, n=n_control*length(data_track$y_))
    data_track_rd = rbind(data_track_rd,
                          data.frame("case_" = data_track$case_,
                                     "x_" = data_track$x_,
                                     "y_" = data_track$y_))
    
    rsf_false = data.frame(data_track_rd %>% 
                             filter(case_ == FALSE),
                           "t_" = rep(data_track$t_, n_control))
    rsf_true = data.frame(data_track)
    
    
    rsf_false <- rsf_false %>% 
      make_track(x_, y_, t_, 
                 id = i,
                 all_cols=T) %>% 
      extract_covariates(habitat_raster) %>% 
      drop_na()
    data_track_rsf = rbind(
      data.frame(rsf_false),
      rsf_true %>% mutate(layer = mean_use) %>% dplyr::select(x_,y_,t_,case_,layer))

    
    
    # Time-varying RSF
    BY = 1
    dd_fit_SMA <-ddhazard(Surv(t_, case_) ~ layer,
                          data_track_rsf,
                          by = BY,
                          max_T = max(data_track_rsf$t_),
                          Q_0 = diag(Q0, 2),
                          Q = diag(Q, 2),
                          control = ddhazard_control(est_Q_0 = TRUE, eps = 0.01))
    
    var_tab = data.frame(dd_fit_SMA$state_vars)
    df_dd = data.frame(t=seq(1,max(data_track_rsf$t_)+BY, by = BY),
                       beta_habitat = dd_fit_SMA$state_vecs[,2],
                       beta_habitat_var = as.numeric(var_tab[2,seq(2,length(names(var_tab)),2)]))
    
    ci <- df_dd$beta_habitat+sqrt(df_dd$beta_habitat_var)%*%t(qnorm(c(0.025,0.5,0.975)))
    dimnames(ci)[[2]]<-c("habitat_lower95", "habitat_est", "habitat_upper95")
    stat_res1 <- residuals(dd_fit_SMA, type = "std_space_error")
    
    df_dd <- cbind(df_dd,ci, data.frame("residual_intercept" = c(NA,stat_res1$residuals[,1]),
                                        "residual_layer" = c(NA,stat_res1$residuals[,2])))
    
    
  
    p2 = plot_grid(
      ggplot(data_track_rsf %>% filter(case_ == TRUE))+
        geom_point(aes(x=t_, y=layer)),
      ggplot(data_track_rsf %>% filter(case_ == FALSE))+
        geom_line(aes(x=t_, y=layer)),
      ggplot(subset(simulated_data, id == 2))+
        geom_ribbon(data = df_dd, aes(x=t, y=habitat_est, ymin = habitat_lower95,
                                      ymax = habitat_upper95), alpha = 0.5)+
        geom_line(data = df_dd, aes(x=t, y=habitat_est), lwd = 1.1)+
        geom_line(aes(x=step, y=beta), lwd = 0.7),
      ncol = 1)
    
    png(here(paste0("./0_Ecology_submission/Figure/Evaluation_supplemental_id2_",nstep,".png")), height = 24, width = 18, units = "cm", res = 600)
    print(p2)
    dev.off() 
    
    
    

    
    
  }



  
  
  ################################################################################
  ####################### ANALYSIS INDIVIDUAL LEVEL ##############################
  ################################################################################
  # 
  # 
  # g2 = ggplot(data_rsf_parameter %>% filter(repet == 1) %>% filter(id < 21))+
  #   # geom_ribbon( aes(x=t, y=beta_habitat , ymin = habitat_lower95 ,
  #   #                  ymax = habitat_upper95 , fill = factor(id)), alpha = 0.5)+
  #   geom_line(aes(x=t, y=beta), lwd = 1.2)+
  #   geom_line(aes(x=t, y=beta_habitat, group = factor(id)), lwd = 0.2) +
  #   theme_bw()+
  #   theme(strip.text = element_text(size=10, face="bold"),
  #         plot.title = element_blank(),
  #         axis.title=element_text(size=10, face="bold"),
  #         axis.text=element_text(size=10),
  #         legend.text = element_text(size=10),
  #         legend.title = element_blank())+
  #   facet_grid(Q~nstep, 
  #              labeller = labeller(Q = dose.labs, nstep = nstep.labs)) +
  #   ylab("Habitat selection strength")+
  #   xlab("Time")+
  #   geom_hline(yintercept = 0, linetype = "dashed", lwd = 1.1)
  # 
  # g2
  # png(here("./0_Ecology_submission/Figure/Evaluation_supplemental_2.png"), height = 12, width = 18, units = "cm", res = 600)
  # g2
  # dev.off() 
  # 
  # # INDEX MEASURE = LINEAR REGRESSION
  # 
  # model_evaluation_id = glmmTMB(beta_mean_t ~ beta*factor(Q)*factor(nstep) + (1|repet) + (1|id),
  #                               data_rsf_parameter)
  # summary(model_evaluation_id)
  # 
  # newdata <- expand.grid(
  #   beta =  seq(min(data_rsf_parameter_mean$beta, na.rm = T),max(data_rsf_parameter_mean$beta, na.rm = T), 0.1),
  #   Q = c(0.1,2),
  #   nstep  = c(20,250),
  #   repet = 1,
  #   id = 1)
  # preds_SR <-predict(model_evaluation_id,newdata,type="response", re.form = NA, se = T)
  # ci <- preds_SR$fit+(preds_SR$se)%*%t(qnorm(c(0.025,0.5,0.975)))
  # dimnames(ci)[[2]]<-c("lower95", "est", "upper95")
  # SR = (ci) # level = 0
  # pred_df <- cbind(newdata,SR)
  # 
  # g1 = ggplot()+
  #   geom_point(data = data_rsf_parameter_mean %>%  filter(row_number() %% 50 == 1),
  #              aes(x=beta, y = beta_mean_t))+
  #   geom_ribbon(data = pred_df,
  #               aes(x=beta, 
  #                   y=est,
  #                   ymin = lower95,
  #                   ymax = upper95),
  #               alpha = 0.3)+
  #   geom_line(data = pred_df,
  #             aes(x=beta, 
  #                 y=est),
  #             lwd = 1.1)+
  #   geom_abline(intercept = 0, slope = 1, lwd = 0.5, linetype = "dashed")+
  #   guides(linetype=guide_legend(keywidth = 2, keyheight = 1))+
  #   theme_bw()+
  #   facet_grid(factor(Q)~factor(nstep), 
  #              labeller = labeller(Q = dose.labs, nstep = nstep.labs)) +  # scale_color_viridis(discrete = T) +
  #   # scale_fill_viridis(discrete = T)+guides(fill = "none")+
  #   ylab("Estimated habitat selection")+
  #   xlab("Expected habitat selection")+
  #   theme(
  #     plot.title = element_blank(),
  #     axis.title=element_text(size=20, face="bold"),
  #     axis.text=element_text(size=18),
  #     legend.text = element_text(size=18),
  #     legend.title = element_text(size=20, face="bold"))
  
  