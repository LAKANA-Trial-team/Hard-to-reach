

#### Hard-to-reach functions

v=function(dat){min(dat,na.rm = T)==max(dat,na.rm = T)}

makeHelpMatrix = function(survey_data,fullData,variableofinterest){



  userDF = data.frame("VillageID"=NA,"username"=NA,variableofinterest=NA,"started_time_datetime" = NA )
  names(userDF) = c("VillageID","username",variableofinterest,"started_time_datetime")


  for(ID in unique(survey_data$VillageID)[which(!is.na(unique(survey_data$VillageID)))]){
    for(user in unique(survey_data$user)[which(!is.na(unique(survey_data$user)))]){



      intermid =  survey_data[which(survey_data$VillageID==ID & survey_data$username==user
      ),c("VillageID","username",variableofinterest,"started_time_datetime")]


      userDF = rbind.data.frame(userDF,intermid)

    }

  }



  userDF = na.omit(userDF)
  userDF$started_time_datetime = as_datetime(userDF$started_time_datetime)

  userDF = userDF[order(userDF$started_time_datetime),]

  prevRaters = c()


  villages = cbind.data.frame(unique(survey_data$VillageID),1 )
  names(villages) = c("VillageID","placeholder")

  for(user in unique(fullData$username)){

    a<-data[which(fullData$username==user),c("VillageID",variableofinterest,"started_time_datetime")]
    #print(user)
    if(any(duplicated(a$VillageID)))
      break

    existingNames = names(villages)
    villages = merge(villages,data[which(data$username==user),c("VillageID",variableofinterest,"started_time_datetime")],by="VillageID",all.x=T)
    names(villages) = c(existingNames,user,paste0(user,"--start_time") )
  }


  villages$moreThanOneAssessment = rowSums(!is.na(villages[,-which(grepl("start_time",names(villages)))]))-2



  raterResultDF2 = data.frame("rater1"=NA,"rater2"=NA,"Kappa"=NA,
                              "Agreement"=NA,"Disagreement"=NA)



  return(list("results"=raterResultDF2,
              "Matrix"=villages))


}


computeQkhat <- function(data, subjects = "subjects", raters = "raters"){
  k <- length(unique(data[[raters]]))
  share <- 0 # Proportion shared raters
  
  RR <- data[[raters]]
  SS <- data[[subjects]]
  tabRxS <- table(RR, SS)
  uSub <- ncol(tabRxS) # Number of unique subjects
  khat <- uSub/ sum(1/colSums(tabRxS)) # harmonic mean number of raters per subject
  for(i in 1:uSub){
    k_s <- colSums(tabRxS)[i]
    
    for(j in (1:uSub)[-i]){
      k_sprime <- colSums(tabRxS)[j]
      k_s.sprime <- sum(RR[SS == i] %in% RR[SS == j])
      share <- share + (k_s.sprime / (k_s*k_sprime))/(uSub * (uSub-1))
    }
  }
  Q <- round(1/khat - share, 3)
  names(Q) <- "Q"
  
  return(list(Q = Q, khat = khat))
}

## Function to compute only khat (saves time when Q is not needed)
computeKhat <- function(data, subjects = "subjects", raters = "raters"){
  k <- length(unique(data[[raters]]))
  share <- 0 # Proportion shared raters
  
  RR <- data[[raters]]
  SS <- data[[subjects]]
  tabRxS <- table(RR, SS)
  uSub <- ncol(tabRxS) # Number of unique subjects
  khat <- uSub/ sum(1/colSums(tabRxS)) # harmonic mean number of raters per subject
  
  return(khat)
}






getICC <- function(icc_var) {
  
  
  for(variableOfInterest in c(icc_var)) {
    
    
    interData = data[which(data[,variableOfInterest]!="---"),]
    
    interData[,variableOfInterest] = ifelse(is.na(interData[,variableOfInterest]),
                                            0
                                            ,interData[,variableOfInterest]) %>% as.numeric()
    
    agreement = makeHelpMatrix(survey_data = interData,fullData = data,variableOfInterest)
    twoOrMore = agreement$Matrix[which(agreement$Matrix$moreThanOneAssessment>1),]
    
  }
  
  twoOrMoreICC = agreement$Matrix[which(agreement$Matrix$moreThanOneAssessment>1),]
  #twoOrMoreICC = agreement$Matrix
  
  twoOrMoreICC = twoOrMoreICC[,-which(grepl("start_time",names(twoOrMoreICC)))]
  
  x.df <- data.frame(stack(twoOrMoreICC[,-c(1,2,ncol(twoOrMoreICC))]), subs = rep(paste("S", 1:nrow(twoOrMoreICC), sep = ""), dim(twoOrMoreICC[,-c(1,2,ncol(twoOrMoreICC))])[2]) )
  
  data = na.omit(x.df)
  
  data = data[which(data[,1]!="---"),]
  data = data[which(data[,1]!="99"),]
  data[,1] = as.numeric(data[,1])
  
  names(data) = c("Y","rater","subject")
  
  Y = "Y" 
  subjects = "subject" 
  raters = "rater"
  level = .95
  k = NULL
  khat = NULL 
  Q = NULL
  estimator = "MLE"
  
  ## Number of raters
  if(is.null(k)){
    k <- length(unique(data[[raters]]))
  }
  
  ## Check type of design
  # Balanced or unbalanced
  if(length(unique(rowSums(table(data[[subjects]], by = data[[raters]])))) == 1){
    balanced <- "TRUE"
  } else {
    balanced <- "FALSE"
  }
  # Complete or incomplete
  if(unique(colSums(table(data[[subjects]], by = data[[raters]])) == 
            length(unique(data[[subjects]])))){
    complete <- "TRUE" 
  } else {
    complete <- "FALSE"
  }
  # One-Way or Two-Way
  if(all(rowSums(table(data$rater, by = data$subject)) == 1)){
    twoWay <- "FALSE"
  } else {
    twoWay <- "TRUE"
  }
  
  if(is.null(khat) | is.null(Q)){
    ## Decide on values for khat and q 
    if(balanced == T & complete == T){ 
      khat <- k
      Q <- 0 
    } else {
      if(balanced == T & complete == F){
        khat <- unique(rowSums(table(data[[subjects]], by = data[[raters]])))
        Q <- computeQkhat(data, subjects = subjects, raters = raters)$Q
      } else {
        if(balanced == F & complete == F){
          Qkhat <- computeQkhat(data, subjects = subjects, raters = raters)
          khat <- Qkhat$khat
          Q <- Qkhat$Q
        } else {
          if(twoWay == F){
            khat <- computeKhat(data, subjects = subjects, raters = raters)
            Q <- 1/k # But not needed, since sigmaR cannot be distinguished
          }
        }
      }
    }
  }
  
  ## ICC, sigma and SD names, for indexing and renaming
  ICCnames <- c("ICCq1", "ICCak", "ICCakhat",
                "ICCc1", "ICCck", "ICCqkhat")
  SDnames <- c("SD_s", "SD_r", "SD_sr") 
  sigmanames <- c("S_s", "S_r", "S_sr") 
  
  ## Variable names of output
  outnames <-  c(ICCnames, 
                 paste0(rep(ICCnames, each = 2), c(".l", ".u")), 
                 paste0(ICCnames, "_se"), 
                 SDnames, 
                 paste0(rep(SDnames, each = 2), c(".l", ".u")), 
                 paste0(SDnames, "_se"), 
                 sigmanames, 
                 paste0(rep(sigmanames, each = 2), c(".l", ".u")), 
                 paste0(sigmanames, "_se"),
                 "Q", "khat", "k", "time")
  
  if(estimator == "MCMC"){
    ## Estimate two-way model using BRMS
    modForm <- paste(Y, "~ 1 + (1|", subjects, ") + (1 |", raters, ")")
    ## Estimate model
    brmOUT <- brms::brm(as.formula(modForm),
                        data   = data, 
                        warmup = 500, 
                        iter   = 1000, 
                        chains = 3, 
                        inits  = "random",
                        cores  = 3)
    # IF non-converged: Return NA for everything 
    if(any( abs(brms::rhat(brmOUT)[2:4] - 1) > .1)){ 
      MCMC <- rep(NA, times = length(outnames))
      names(MCMC) <- outnames 
    } else {
      ## If converged: Give me results 
      ## Extract posterior distribution of SDs
      SDs <- rstan::extract(brmOUT$fit, c(paste0("sd_", subjects, "__Intercept"),
                                          paste0("sd_",   raters, "__Intercept"),
                                          "sigma"))
      # List SDs to later get MAPs
      names(SDs) <- c("SD_s", "SD_r", "SD_sr")
      
      ## Convert to variances 
      S_s <- SDs[["SD_s"]]^2
      S_r <- SDs[["SD_r"]]^2  
      S_sr <- SDs[["SD_sr"]]^2
      
      # List variances to later get MAPs
      sigmas <- list(S_s = S_s, S_r = S_r, S_sr = S_sr)
      
      ## Obtain ICCs
      ICCq1 <- as.numeric(S_s / (S_s +  Q*S_r + S_sr))
      ICCak <- as.numeric(S_s / (S_s + (S_r + S_sr)/k))
      ICCakhat <- as.numeric(S_s / (S_s + (S_r + S_sr)/khat))
      ICCc1 <- as.numeric(S_s / (S_s + S_sr))
      ICCck <- as.numeric(S_s / (S_s + S_sr/k))
      ICCqkhat <- as.numeric(S_s / (S_s + Q*S_r + S_sr/khat))
      
      
      ICCs <- list(ICCq1 = ICCq1, ICCak = ICCak, ICCakhat = ICCakhat,
                   ICCc1 = ICCc1, ICCck = ICCck, ICCqkhat = ICCqkhat)
      
      ## Confidence levels 
      ICC_cis <-  do.call(rbind, lapply(ICCs, quantile, probs = c((1-level)/2, level + (1-level)/2)))
      sigma_cis <- do.call(rbind, lapply(sigmas, quantile, probs = c((1-level)/2, level + (1-level)/2)))
      
      ## SEs (posterior SDs)
      ICC_ses <- unlist(lapply(ICCs, sd))
      sigma_ses <- unlist(lapply(sigmas, sd))
      
      ## Point estimates (last, to not overwrite sigmas, SDs and ICCs sooner)
      # function to estimate posterior modes
      Mode <- function(x) {
        d <- density(x)
        d$x[which.max(d$y)]
      }
      # MAPs 
      sigmas <- mapply(Mode, sigmas) # Variances
      ICCs <- mapply(Mode, ICCs) # ICCs 
      
      # combine results
      ICCs <- cbind(ICCs, ICC_cis, ICC_ses)
      colnames(ICCs) <- c("ICC", "lower", "upper", "se")
      sigmas <- cbind(sigmas, sigma_cis, sigma_ses)
      colnames(ICCs) <- c("variance", "lower", "upper", "se")
      out <- list(ICCs = ICCs, 
                  sigmas = sigmas, 
                  Q = Q, khat = khat, k = k)
      
    }
  }
  
  if(estimator == "MLE"){
    ### Estimate two-way model using lme4 (random-effects model)
    ## Define model
    modForm <- paste(Y, "~ 1 + (1|", subjects, ") + (1 |", raters, ")")
    ## Estimate model
    mod   <- lme4::lmer(as.formula(modForm), data = data)
    ## Check convergence
    # checkConv <- function(mod) { 
    #   warn <- mod@optinfo$conv$lme4$messages
    #   !is.null(warn) && grepl('failed to converge', warn) 
    # }
    # if(checkConv(mod)){
    #   # If nonconverged: Return NAs for everything
    #   MLE4 <- rep(NA, times = length(outnames))
    #   names(MLE4) <- outnames
    # } else {
    
    # If converged: Give results
    ## Extract variances
    S_s  <- lme4::VarCorr(mod)[[subjects]][1, 1]  
    S_r  <- lme4::VarCorr(mod)[[raters]][1, 1]
    S_sr  <- sigma(mod)^2 
    
    ## Compute ICC point estimates
    ICCq1 <- S_s / (S_s + Q*S_r + S_sr)
    ICCak <- S_s / (S_s + (S_r + S_sr)/k)
    ICCakhat <- S_s / (S_s + (S_r + S_sr)/khat)
    ICCc1 <- S_s / (S_s + S_sr) 
    ICCck <- S_s / (S_s + (S_sr)/k) 
    ICCqkhat <- S_s / (S_s + Q*S_r + S_sr/khat)
    
    ## List all (and create SDs)
    sigmas <- c(S_s = S_s, S_r = S_r, S_sr = S_sr)
    ICCs <- c(ICCq1 = ICCq1, ICCak = ICCak, ICCakhat = ICCakhat,
              ICCc1 = ICCc1, ICCck = ICCck, ICCqkhat = ICCqkhat)
    
    ## Asymptotic vcov matrix of sigmas
    suppressWarnings(ACOV <- merDeriv::vcov.lmerMod(mod, full = TRUE))
    Sidx <- grep(pattern = subjects, colnames(ACOV), fixed = TRUE) 
    Ridx <- grep(pattern = raters, colnames(ACOV), fixed = TRUE)
    SRidx <- which(colnames(ACOV) == "residual")
    idx      <- c(   Sidx  ,  Ridx  ,  SRidx  )
    newNames <- c("subject", "rater", "interaction")
    VCOV <- ACOV[idx, idx]
    dimnames(VCOV) <- list(newNames, newNames)
    vars <- c(subject = S_s, rater = S_r, interaction = S_sr)
    
    ## CIs and SEs of ICCs using asymptotic vcov matrix
    ## All info of all ICCs in one list
    ICCdefs <- c("subject / (subject + rater + interaction)", 
                 "subject / (subject + (rater + interaction)/k)",
                 "subject / (subject + (rater + interaction)/khat)", 
                 "subject / (subject + interaction)",
                 "subject / (subject + interaction/k)",
                 "subject / (subject + Q*rater + interaction/khat)"
    )
    names(ICCdefs) <- ICCnames
    ICCs_dm <- do.call("rbind", lapply(ICCdefs, FUN = function(x){
      car::deltaMethod(vars, vcov. = VCOV, level = level,g. = x)
    }))
    
    ICC_ses <- ICCs_dm[,"SE"]
    names(ICC_ses) <- paste0(ICCnames, "_se")
    sigma_ses <- do.call("rbind", lapply(newNames, FUN = function(x){
      car::deltaMethod(vars, vcov. = VCOV, level = level,g. = x)
    }))$SE 
    names(sigma_ses) <- paste0(names(sigmas), "_se")
    
    ## Monte-Carlo CIs of variances and ICCs
    dimnames(VCOV) <- list(names(sigmas), names(sigmas))
    sigma_mcCIs <- semTools::monteCarloCI(expr = c(S_s = 'S_s', S_r = "S_r", S_sr = "S_sr"),
                                          coefs = sigmas, ACM = VCOV)
    ICC_mcCIs <- semTools::monteCarloCI(expr = c(ICCq1_ci = paste0("S_s / (S_s + ", Q, "*S_r + S_sr)"), 
                                                 ICCak_ci = paste0("S_s / (S_s + (S_r + S_sr)/", k, ")"),
                                                 ICCakhat_ci = paste0("S_s / (S_s + (S_r + S_sr)/", khat, ")"), 
                                                 ICCc1_ci = "S_s / (S_s + S_sr)",
                                                 ICCck_ci = paste0("S_s / (S_s + S_sr/", k, ")"),
                                                 ICCqkhat_ci = paste0("S_s / (S_s + ", Q, "*S_r + S_sr/", khat, ")")),
                                        coefs = sigmas, ACM = VCOV)
    
    ## Results in matrices
    ICCs <- cbind(ICC_mcCIs, ICC_ses)
    sigmas <- cbind(sigma_mcCIs, sigma_ses)
    dimnames(ICCs) <- list(outnames[1:6], c("ICC", "lower", "upper", "se"))
    colnames(sigmas) <- c("variance", "lower", "upper", "se")
    
    out <- list(ICCs = ICCs, sigmas = sigmas, Q = Q, khat = khat, k = k)
  }
  
  
  
  return(out)
  
  
}
