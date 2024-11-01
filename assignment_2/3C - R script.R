
# _____________________________________________________________________________
#
#    Advanced Methods in Impact Assessment Workshop
#    Day 3: Measuring Program Impacts: Diff-in-Diff and Instrumental Variables
#    Exercise 3C
# _____________________________________________________________________________


# clear memory ----------------------------------------------------------------
  rm(list=ls())


# packages --------------------------------------------------------------------
  # (remove "#" from the lines below if packages were not installed before) 
    #install.packages("readstata13")    # read stata .dta files
    #install.packages("ggplot2")        # create nice plots and figures
    #install.packages("MatchIt")        # propensity score matching
     install.packages("AER")            # 2SLS


  library("readstata13")
  library("ggplot2")
  library("MatchIt")
  library("AER")
  
  
  
# load data -------------------------------------------------------------------
  df <- read.csv("VDSA_Prod_Data_Ref.csv")
  
  # drop 2012 data
  df <- subset(df, sur_yr != 2012)
  
  
  
# Q. 1 ------------------------------------------------------------------------
  
  # ttest with observations from 2011
  t.test(lny~irr, data = subset(df, sur_yr == 2011))
  
  
  
# Manipulate data for DiD -----------------------------------------------------
  
  # create a temporary dataframe with the output of 2010
  td10 <- subset(df, sur_yr == 2010)
  td10$lny10 <- td10$lny
  td10 <- td10[,c("prcl_id", "lny10")]
  
  # create dataset of 2011 observations
  df11 <- subset(df, sur_yr == 2011)
  df11$lny11 <- df11$lny
  
  # merge back info from 2010 (keep only data matched in both years)
  df11 <- merge(df11, td10, by = "prcl_id")
  
  # create variable of difference
  df11$lny1011 <- df11$lny11 - df11$lny10
  
  
# Q. 2 ------------------------------------------------------------------------
  
  # ttest between irrigated and non irrigated
  t.test(lny1011~irr, data = df11)
  

# Manipulate data for DiD regression ------------------------------------------
  
  df$irr0 <- ifelse(df$irr == 1 & df$sur_yr == 2011, df$irr, 0)
  df$irr11 <- ave(df$irr0, df$prcl_id, FUN=max)
  
  df$vdumsur_yr_2011 <- ifelse(df$sur_yr == 2011, 1, 0)
  df$vdumsur_yr_2010 <- ifelse(df$sur_yr == 2010, 1, 0)
  
  df$irr11_yr = df$irr11*df$vdumsur_yr_2011
  
  
  dd2 <- lm(lny ~ irr11 + irr11_yr + vdumsur_yr_2011, data = df)
  summary(dd2)
  
  
# Q. 4 ------------------------------------------------------------------------
  
  dd4 <- lm(lny ~ irr11 + irr11_yr + vdumsur_yr_2011 +
                  lnl + lnf + lnm + lnp +
                  ageH + genderH + sizehh +
                  lnaindex + lnlindex + lntot_acre + lndist,
            data = df)
  summary(dd4)
  
  
#______________________________________________________________________________  
#                          Propensity Score Matching
#______________________________________________________________________________
  
  df10 <- subset(df, sur_yr == 2010)
  
  # clean data to exlude variables with NAs (matchit cannot handle it well)
  match.covs <- c("irr11", "lnl", "lnf", "lnm", "lnp",
                  "ageH", "genderH", "sizehh", "lnaindex",
                  "lntot_acre", "prcl_id")
  
  # run matching
  mm <- matchit(irr11 ~ lnl + lnf + lnm + lnp +
                        ageH + genderH + sizehh +
                        lntot_acre + lnaindex,
                  data = df10[,match.covs], distance = "logit",
                  discard = "control", replace = T)
  
  # get matched data
  md <- match.data(mm)
  
  # clean it to only include an indicator of matching
  md$matched <- 1
  md <- md[,c("prcl_id", "matched")]
  
  # make matched-panel data frame (mpdf)
  mpdf <- merge(df, md, by = "prcl_id")
  

  
# Q. 5 ------------------------------------------------------------------------
  # ols DiD with matched data
  dd5 <- lm(lny ~ irr11 + irr11_yr + vdumsur_yr_2011 +
                  lnl + lnf + lnm + lnp +
                  ageH + genderH + sizehh +
                  lnaindex + lnlindex + lntot_acre,
            data = mpdf)
  summary(dd5)

  
  
  
#______________________________________________________________________________  
#                                    IV 
#______________________________________________________________________________

  
  
# Q. 7 ------------------------------------------------------------------------
  iv7 <- lm(lny ~ irr + 
                   lnl + lnf + lnm + lnp +
                   ageH + genderH + sizehh +
                   lnaindex + lnlindex + lntot_acre + lndist,
             data = df)
  summary(iv7)
  
  
  
# IV regression ---------------------------------------------------------------
  
  # create IV
  df$IV_landrain <- df$tot_acre*df$rain 
  
  # exclude obs with NAs
  ivdf <- subset(df, is.na(IV_landrain) == FALSE)
 
  # 1st stage
  st1 <- lm(irr ~ IV_landrain +
                  lnl + lnf + lnm + lnp +
                  ageH + genderH + sizehh +
                  lnaindex + lnlindex + lntot_acre + lndist,
            data = ivdf)
  summary(st1)
  ivdf$irrhat <- st1$fitted.values
  
  
  
  # 2nd stage
  st2 <- lm(lny ~ irrhat +
                  lnl + lnf + lnm + lnp +
                  ageH + genderH + sizehh +
                  lnaindex + lnlindex + lntot_acre + lndist,
            data = ivdf)
  summary(st2)

  
  
# 2SLS regression -----------------------------------------------------------
  iv.model <- (ivreg(lny  ~ irr + 
                       lnl + lnf + lnm + lnp +
                       ageH + genderH + sizehh +
                       lnaindex + lnlindex + lntot_acre + lndist |
                       IV_landrain +
                       lnl + lnf + lnm + lnp +
                       ageH + genderH + sizehh +
                       lnaindex + lnlindex + lntot_acre + lndist,
                     data=ivdf))
  summary(iv.model)
  
  
  
# Q. 11 -----------------------------------------------------------------------

  summary(iv.model, diagnostics = TRUE)


  
# Q. 12 -----------------------------------------------------------------------

  iv.model2 <- (ivreg(lny  ~ irr + 
                        lnl + lnf + lnm + lnp +
                        ageH + genderH + sizehh +
                        lnaindex + lnlindex + lntot_acre + lndist |
                        IV_landrain + rain + tot_acre +
                        lnl + lnf + lnm + lnp +
                        ageH + genderH + sizehh +
                        lnaindex + lnlindex + lntot_acre + lndist,
                     data=ivdf))
  summary(iv.model2, diagnostics = T)
  