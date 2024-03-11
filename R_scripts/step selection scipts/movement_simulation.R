
# Useful function to ggplot raster
gplot_data <- function(x, maxpixels = 200000)  {
  x <- raster::sampleRegular(x, maxpixels, asRaster = TRUE)
  coords <- raster::xyFromCell(x, seq_len(raster::ncell(x)))
  
  dat <- utils::stack(as.data.frame(raster::getValues(x)))
  names(dat) <- c('value', 'variable')
  
  dat <- dplyr::as.tbl(data.frame(coords, dat))
  
  if (!is.null(levels(x))) {
    dat <- dplyr::left_join(dat, levels(x)[[1]],
                            by = c("value" = "ID"))
  }
  dat
}





# Function used to simulate the movement of (n_ind) individuals over (nstep) time-steps on 1 landscape
mvt_simulation = function(nstep, n_phasis, n_ind, beta_min, beta_max){
  
  # Expected strength of selection
  data = data.frame("t" = 1:(nstep*n_phasis),
                    "beta" = rep(round(runif(n_phasis,beta_min,beta_max),2), each = nstep))
  mod_beta = lm(beta~bs(t, df = 20), data)
  beta = c(predict(mod_beta, newdata = data.frame("t" = 1:(nstep*n_phasis))))
 
  # Radius used for localGibbs simulation
   allr <- 100
   
  # Landscape simulation
  landscape_extent <- c(0,500,0,500)
  habitat_raster <- simRaster(rho=50, lim=landscape_extent, res=1)
  habitat_matrix <- raster::as.matrix(habitat_raster)
  n_row <- nrow(habitat_raster) 
  n_col <- ncol(habitat_raster)
  cov <- array(c(habitat_matrix),dim=c(n_row,n_col,1))
  
  # To collect simulated locations
  xylist <- vector("list",n_ind)
  for(i in 1:n_ind){
    xylist[[i]] <- matrix(NA,nrow=(n_phasis*nstep),ncol=2)
  }
  simulated_data = data.frame()
  
  # Simulate the movement of (n_ind) individuals over (nstep) time-steps on 1 landscape
  for (i in 1:n_ind){
    
    
    for (p in 1:(n_phasis*nstep)){
      
      if (p == 1){
        xylist[[i]][p,] <- 
          simLG(nbObs=2, beta=beta[p], allr=allr,
                covlist=list(habitat_raster),
                npts=1000,norm=FALSE)[2,]
      } else {
        
        xylist[[i]][p,] <- 
          simLG(nbObs=2, beta=beta[p], allr=allr,
                covlist=list(habitat_raster),
                xy0=xylist[[i]][(p-1),],
                npts=1000,norm=FALSE)[2,]
      }
    }
    mvt_data = data.frame("id" = i,
                          "step" = 1:(nstep*n_phasis),
                          "x" = xylist[[i]][,1], 
                          "y" = xylist[[i]][,2],
                          "phasis" = n_phasis,
                          "beta" = beta)
    simulated_data = rbind(simulated_data,mvt_data)
    
    
  }  
  
  return(list(simulated_data, habitat_raster))
}



# Function used to estimate the time-varying habitat selection of the (n_ind) individuals simulated on habitat_raster
tRSF = function(simulated_data, BY, n_ind, Q0, Q, habitat_raster){
  data_tRSF = data.frame()
  for (i in 1:n_ind){
    
    mvt_data = simulated_data %>% filter(id == i)
    
    # Available locations
    n_control = 100
    coords = mvt_data[,c("x","y")]
    data2 = data.frame("name" = mvt_data$id)
    xy = SpatialPointsDataFrame(coords = coords,data = data2)
    kud_id1 <- kernelUD(xy[,1], h="href")
    HR_id1 <- getverticeshr(kud_id1, percent = 99, unout = "km2") # from 50% UD
    
    # Make track data
    data_track <- mvt_data %>% 
      mutate(case_ = TRUE)  %>% 
      make_track(x,y,step, 
                 id = i,
                 all_cols=T) 
    
    data_track_rd <- random_points(HR_id1, n=n_control*length(data_track$y_))
    data_track_rd = rbind(data_track_rd,
                          data.frame("case_" = data_track$case_,
                                     "x_" = data_track$x_,
                                     "y_" = data_track$y_))
    
    rsf_false = data.frame(data_track_rd %>% 
                             filter(case_ == FALSE),
                           "t_" = rep(data_track$t_, n_control))
    rsf_true = data.frame(data_track_rd %>% 
                            filter(case_ == TRUE),
                          "t_" = data_track$t_)
    
    data_track_rsf = rbind(rsf_true,rsf_false)
    data_track_rsf <- data_track_rsf[order(data_track_rsf$t_, -data_track_rsf$case_),]
    
    
    data_track_rsf <- data_track_rsf %>% 
      make_track(x_, y_, t_, 
                 id = i,
                 all_cols=T) %>% 
      extract_covariates(habitat_raster) %>% 
      drop_na()
    data_track_rsf = data.frame(data_track_rsf)
    
    
    
    # Time-varying RSF
    BY = BY
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
    
    
    
    ggplot(mvt_data)+
      geom_ribbon(data = df_dd, aes(x=t, y=habitat_est, ymin = habitat_lower95,
                                    ymax = habitat_upper95), alpha = 0.5)+
      geom_line(data = df_dd, aes(x=t, y=habitat_est), lwd = 1.1)+
      geom_line(aes(x=step, y=beta), lwd = 0.7) +
      theme_bw()
    
    
    data = left_join(df_dd, mvt_data, by = c("t" = "step"))
    
    
    data_tRSF = rbind(data_tRSF, data)   
    
    
  }
  
  return(data_tRSF)
}  








