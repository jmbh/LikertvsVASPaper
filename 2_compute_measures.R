# jonashaslbeck@gmail.com; June 7th, 2024

# ----------------------------------------------------
# -------- What is happening here? -------------------
# ----------------------------------------------------

# Here we compute summary statistics for visualization
# and further modeling

# ----------------------------------------------------
# -------- Loading Packages --------------------------
# ----------------------------------------------------

# Data wrangling
library(plyr)
library(readr)
# Analysis
library(e1071)

source("0_aux_Functions.R") # For bimodality and IEB calculation


# ----------------------------------------------------
# -------- Loading Data ------------------------------
# ----------------------------------------------------

data_Lik_cmb <- readRDS("Files/data_ESM_Lik.RDS")
data_VAS_cmb <- readRDS("Files/data_ESM_VAS.RDS")
sel <- c(4:17) # Those are the variables

# External validation
out_BSI <- readRDS("Files/data_BSI.RDS")
out_DASS <- readRDS("Files/data_DASS.RDS")

# ----------------------------------------------------
# -------- For each subject: Order by Time -----------
# ----------------------------------------------------

# This is needed to determine the dropout time per subject
# Currently this is not ordered because we combined morning/midday/evening from different files

# ----- For Likert -----
u_subj <- unique(data_Lik_cmb$Name)
l_coll <- list()
for(i in 1:length(u_subj)) l_coll[[i]] <- data_Lik_cmb[data_Lik_cmb$Name == u_subj[i], ][order(data_Lik_cmb$RespTime[data_Lik_cmb$Name == u_subj[i]]), ]
data_Lik_cmb_ord <- do.call(rbind, l_coll)

# ----- For VAS -----
u_subj <- unique(data_VAS_cmb$Name)
l_coll <- list()
for(i in 1:length(u_subj)) l_coll[[i]] <- data_VAS_cmb[data_VAS_cmb$Name == u_subj[i], ][order(data_VAS_cmb$RespTime[data_VAS_cmb$Name == u_subj[i]]), ]
data_VAS_cmb_ord <- do.call(rbind, l_coll)

# ----------------------------------------------------
# -------- Delete Extra Rows -------------------------
# ----------------------------------------------------

# Somehow there are actual rows in the data where no measurement took place
# I delete them, so they don't corrupt the NA-analysis that comes next
data_Lik_cmb_ord_F <- ddply(data_Lik_cmb_ord, .(Name), function(x) x[!is.na(x$RespTime), ])
data_VAS_cmb_ord_F <- ddply(data_VAS_cmb_ord, .(Name), function(x) x[!is.na(x$RespTime), ])


# ----------------------------------------------------
# -------- Data Exclusion: Minimum of 20 data points -
# ----------------------------------------------------

# Simply look at rows (which makes sense after removing empty rows, see just above)
# ----- Likert -----
Lik_nrow <- ddply(data_Lik_cmb_ord_F, .(Name), nrow)
# ----- VAS -----
VAS_nrow <- ddply(data_VAS_cmb_ord_F, .(Name), nrow)

# Look at NAs
# ----- Likert -----
Lik_NA <- ddply(data_Lik_cmb_ord_F, .(Name), function(x) sum(!is.na(x$Happy)))
# ----- VAS -----
VAS_NA <- ddply(data_VAS_cmb_ord_F, .(Name), function(x) sum(!is.na(x$Happy)))

# Save for inference
Lik_NA_prop <- Lik_NA
Lik_NA_prop[, 2] <- 1 - Lik_NA_prop[, 2]/84


VAS_NA_prop <- VAS_NA
VAS_NA_prop[, 2] <- 1 - VAS_NA_prop[, 2]/84
colnames(Lik_NA_prop)[2] <- colnames(VAS_NA_prop)[2] <- "prop_missing"
list_NA <- list("Lik"=Lik_NA_prop, "VAS"=VAS_NA_prop)
saveRDS(list_NA, "Files/NA_prop.RDS")

# ----- SUBSETTING for future analysis ------
ind_Lik <- Lik_nrow[, 1][Lik_NA[, 2] >= 20] # at least 20
ind_VAS <- VAS_nrow[, 1][VAS_NA[, 2] >= 19]

# Subset
data_Lik <- data_Lik_cmb_ord_F[data_Lik_cmb_ord_F$Name %in% ind_Lik, ]
data_VAS <- data_VAS_cmb_ord_F[data_VAS_cmb_ord_F$Name %in% ind_VAS, ]


# ----------------------------------------------------
# -------- Exclude Pathological Subject 72704 --------
# ----------------------------------------------------
# Subject 72704: Looking at the results, this person
# filled in the same response in for each question which
# is entirely implausible, so we exclude this person

# # Illustrate:
# data_VAS_72704 <- data_VAS[data_VAS$Name == 72704, ]
# plot(unlist(data_VAS_72704[, 9]), type="l", xlab="", ylab="Time", main="Subject 72704", font.main=1)
# lines(unlist(data_VAS_72704[, 10]), col="red")
# lines(unlist(data_VAS_72704[, 11]), col="blue")
# lines(unlist(data_VAS_72704[, 12]), col="orange")


# Kick that person out
data_VAS <- data_VAS[data_VAS$Name != 72704, ]


# ----------------------------------------------------
# -------- Save ESM data after Exclusion -------------
# ----------------------------------------------------

# ------ Save Data ------
saveRDS(data_Lik, "Files/data_ESM_Lik_incl.RDS")
saveRDS(data_VAS, "Files/data_ESM_VAS_incl.RDS")

data_Lik <- readRDS("Files/data_ESM_Lik_incl.RDS")
data_VAS <- readRDS("Files/data_ESM_VAS_incl.RDS")

# ----------------------------------------------------
# -------- Compute Dropout ---------------------------
# ----------------------------------------------------

# This requires that time points are ordered within subjects
# (which is what we did above)

# Likert
Lik_LastM <- ddply(data_Lik_cmb_ord, .(Name), function(x) {
  v_noNA <- which(!is.na((x$Happy)))
  lnoNA <- length(v_noNA)
  if(lnoNA==0) 0 else v_noNA[length(v_noNA)]
} )
# VAS
VAS_LastM <- ddply(data_VAS_cmb_ord, .(Name), function(x) {
  v_noNA <- which(!is.na((x$Happy)))
  lnoNA <- length(v_noNA)
  if(lnoNA==0) 0 else v_noNA[length(v_noNA)]
} )

colnames(Lik_LastM)[2] <- colnames(VAS_LastM)[2] <- "Last_Measure"

# ----- Save -----
l_Dropout <- list("Lik"=Lik_LastM, "VAS"=VAS_LastM)
saveRDS(l_Dropout, "Files/Dropout.RDS")


# ----------------------------------------------------
# -------- Compute Duration --------------------------
# ----------------------------------------------------


# ----- Per Subject -----
# Likert
Lik_Dur_mu <- ddply(data_Lik, .(Name), function(x) {
  v_dur <- as.numeric(x$Duration)
  c(mean(v_dur[x$SurveyType==1], na.rm=TRUE), # morning
    mean(v_dur[x$SurveyType==2], na.rm=TRUE), # during day
    mean(v_dur[x$SurveyType==3], na.rm=TRUE)) # evening
})

# VAS
VAS_Dur_mu <- ddply(data_VAS, .(Name), function(x) {
  v_dur <- as.numeric(x$Duration)
  c(mean(v_dur[x$SurveyType==1], na.rm=TRUE), # morning
    mean(v_dur[x$SurveyType==2], na.rm=TRUE), # during day
    mean(v_dur[x$SurveyType==3], na.rm=TRUE)) # evening
})
colnames(Lik_Dur_mu)[2:4] <- colnames(VAS_Dur_mu)[2:4] <- c("Dur_Morning", "Dur_Day", "Dur_Evening")

# ----- Save -----
l_Duration <- list("Lik"=Lik_Dur_mu, "VAS"=VAS_Dur_mu)
saveRDS(l_Duration, "Files/Duration.RDS")

# ----------------------------------------------------
# -------- Compute Measures --------------------------
# ----------------------------------------------------

# Here we compute various characteristics of the data distribution across the two groups
# which comprise the main analysis of the paper

# data_VAS$Happy[data_VAS$Name==unique(data_VAS$Name)[1]]
# hist(data_VAS$Happy[data_VAS$Name==unique(data_VAS$Name)[1]], breaks=20)
# abline(v=seq(0, 100, length=7+1), lty=2)


# Only get response variables
sel <- 4:17

# List to collect all measures
l_WPM <- list()

# -------- (1) Within-person means: Mean & SD ------------
# ----- Likert -----
l_WPM$Means$Lik <- ddply(data_Lik, .(Name), function(x) {
  apply((x[, sel]-1)/6, 2, function(y) mean(y, na.rm=TRUE))
})[, -1]
# ----- VAS -----
l_WPM$Means$VAS <- ddply(data_VAS, .(Name), function(x) {
  apply(x[, sel]/100, 2, function(y) mean(y, na.rm=TRUE))
})[, -1]
# ----- VAS mapped to Likert -----
l_WPM$Means$VAS2Lik <- ddply(data_VAS, .(Name), function(x) {
  apply(x[, sel], 2, function(y) {
    y_2Lik <- (cut(x=y, breaks=seq(0, 100, length=8), include.lowest=TRUE, labels=FALSE) - 1) / 6
    mean(y_2Lik, na.rm=TRUE)
  })
})[, -1]


# -------- (2) Within-person variance: Mean & SD ---------
# ----- Likert -----
l_WPM$SDs$Lik <- ddply(data_Lik, .(Name), function(x) {
  apply((x[, sel]-1)/6, 2, function(y) sd(y, na.rm=TRUE))
})[, -1]
# ----- VAS -----
l_WPM$SDs$VAS <- ddply(data_VAS, .(Name), function(x) {
  apply(x[, sel]/100, 2, function(y) sd(y, na.rm=TRUE))
})[, -1]
# ----- VAS mapped to Likert -----
l_WPM$SDs$VAS2Lik <- ddply(data_VAS, .(Name), function(x) {
  apply(x[, sel], 2, function(y) {
    y_2Lik <- (cut(x=y, breaks=seq(0, 100, length=8), include.lowest=TRUE, labels=FALSE) - 1) / 6
    sd(y_2Lik, na.rm=TRUE)
  })
})[, -1]


# -------- (3) RMSSD ----------------
# ----- Likert -----
l_WPM$RMSSD$Lik <- ddply(data_Lik, .(Name), function(x) {
  apply((x[, sel]-1)/6, 2, function(y) {
    
    # Make lagged matrix, induce NAs for missing beeps
    lagged_mat <- cbind(y[-1], y[-length(y)])
    # Delete overnight
    daydiff <- x$RespTime[-1] - x$RespTime[-length(y)]
    lagged_mat[daydiff!=0, ] <- NA
    
    # get number of rows we can use
    rows_NA <- apply(lagged_mat, 1, function(x) any(is.na(x)))
    # Requirement: At least 20 rows to compute the correlation
    ind_NA <- sum(!rows_NA) < 20
    
    if(ind_NA) {
      NA
    } else {
      diff <- lagged_mat[, 1]-lagged_mat[, 2]
      sqrt(mean((diff)^2, na.rm=TRUE))
    }
    
  })
})[, -1]
# ----- VAS -----
l_WPM$RMSSD$VAS <- ddply(data_VAS, .(Name), function(x) {
  apply(x[,sel]/100, 2, function(y) {
    # Make lagged matrix, induce NAs for missing beeps
    lagged_mat <- cbind(y[-1], y[-length(y)])
    # Delete overnight
    daydiff <- x$RespTime[-1] - x$RespTime[-length(y)]
    lagged_mat[daydiff!=0, ] <- NA
    
    # get number of rows we can use
    rows_NA <- apply(lagged_mat, 1, function(x) any(is.na(x)))
    # Requirement: At least 20 rows to compute the correlation
    ind_NA <- sum(!rows_NA) < 20
    
    if(ind_NA) {
      NA
    } else {
      diff <- lagged_mat[, 1]-lagged_mat[, 2]
      sqrt(mean((diff)^2, na.rm=TRUE))
    }
  })
})[, -1]


# ----- VAS mapped to Likert -----
l_WPM$RMSSD$VAS2Lik <- ddply(data_VAS, .(Name), function(x) {
  apply(x[,sel], 2, function(y) {
    y_2Lik <- (cut(x=y, breaks=seq(0, 100, length=8), include.lowest=TRUE, labels=FALSE) - 1) / 6
    
    # Make lagged matrix, induce NAs for missing beeps
    lagged_mat <- cbind(y_2Lik[-1], y_2Lik[-length(y)])
    # Delete overnight
    daydiff <- x$RespTime[-1] - x$RespTime[-length(y)]
    lagged_mat[daydiff!=0, ] <- NA
    
    # get number of rows we can use
    rows_NA <- apply(lagged_mat, 1, function(x) any(is.na(x)))
    # Requirement: At least 20 rows to compute the correlation
    ind_NA <- sum(!rows_NA) < 20
    
    if(ind_NA) {
      NA
    } else {
      diff <- lagged_mat[, 1]-lagged_mat[, 2]
      sqrt(mean((diff)^2, na.rm=TRUE))
    }
    
  })
})[, -1]


# -------- (4) Autocorrelations ----------------
# ----- Likert -----
l_WPM$AR$Lik <- ddply(data_Lik, .(Name), function(x) {
  apply((x[,sel]-1)/6, 2, function(y) {
    
    # Make lagged matrix, induce NAs for missing beeps
    lagged_mat <- cbind(y[-1], y[-length(y)])
    # Delete overnight
    daydiff <- x$RespTime[-1] - x$RespTime[-length(y)]
    lagged_mat[daydiff!=0, ] <- NA
    
    # get number of rows we can use
    rows_NA <- apply(lagged_mat, 1, function(x) any(is.na(x)))
    # Requirement: At least 20 rows to compute the correlation
    ind_NA <- sum(!rows_NA) < 20
    
    if(ind_NA) {
      NA
    } else {
      cor(lagged_mat[, 1], lagged_mat[, 2], use="complete.obs")
    }
  })
  
})[, -1]


# ----- VAS -----
l_WPM$AR$VAS <- ddply(data_VAS, .(Name), function(x) {
  apply(x[,sel]/100, 2, function(y) {
    # Make lagged matrix, induce NAs for missing beeps
    lagged_mat <- cbind(y[-1], y[-length(y)])
    # Delete overnight
    daydiff <- x$RespTime[-1] - x$RespTime[-length(y)]
    lagged_mat[daydiff!=0, ] <- NA
    
    # get number of rows we can use
    rows_NA <- apply(lagged_mat, 1, function(x) any(is.na(x)))
    # Requirement: At least 20 rows to compute the correlation
    ind_NA <- sum(!rows_NA) < 20
    
    if(ind_NA) {
      NA
    } else {
      cor(lagged_mat[, 1], lagged_mat[, 2], use="complete.obs")
    }
  })
})[, -1]
# ----- VAS mapped to Likert -----
l_WPM$AR$VAS2Lik <- ddply(data_VAS, .(Name), function(x) {
  apply(x[,sel], 2, function(y) {
    
    # Map to Likert
    y_2Lik <- (cut(x=y, breaks=seq(0, 100, length=8), include.lowest=TRUE, labels=FALSE) - 1) / 6
    # Make lagged matrix, induce NAs for missing beeps
    lagged_mat <- cbind(y_2Lik[-1], y[-length(y_2Lik)])
    # Delete overnight
    daydiff <- x$RespTime[-1] - x$RespTime[-length(y_2Lik)]
    lagged_mat[daydiff!=0, ] <- NA
    
    # get number of rows we can use
    rows_NA <- apply(lagged_mat, 1, function(x) any(is.na(x)))
    # Requirement: At least 20 rows to compute the correlation
    ind_NA <- sum(!rows_NA) < 20
    
    if(ind_NA) {
      NA
    } else {
      cor(lagged_mat[, 1], lagged_mat[, 2], use="complete.obs")
    }
  })
})[, -1]


# -------- (5) Correlations between items ----------------
# ----- Likert -----
l_WPM$CorLag0$Lik <- ddply(data_Lik, .(Name), function(x) {
  corm <- cor(x[, sel], use="complete.obs")
  diag(corm) <- NA
  colMeans(abs(corm), na.rm=TRUE)
})[, -1]
# ----- VAS -----

data_VAS_88414 <- data_VAS[data_VAS$Name==88414, ]

l_WPM$CorLag0$VAS <- ddply(data_VAS, .(Name), function(x) {
  corm <- cor(x[, sel], use="complete.obs")
  diag(corm) <- NA
  colMeans(abs(corm), na.rm=TRUE)
})[, -1]
# ----- VAS mapped to Likert -----
l_WPM$CorLag0$VAS2Lik <- ddply(data_VAS, .(Name), function(x) {
  y_2Lik <- apply(x[, sel], 2, function(y) {
    (cut(x=y, breaks=seq(0, 100, length=8), include.lowest=TRUE, labels=FALSE) - 1) / 6
  })
  corm <- cor(y_2Lik, use="complete.obs")
  diag(corm) <- NA
  colMeans(abs(corm), na.rm=TRUE)
})[, -1]



# -------- (6) Bimodality --------------------------------
set.seed(1)
# ----- Likert -----
l_WPM$Modality$Lik <- ddply(data_Lik, .(Name), function(x) {
  apply(x[,sel], 2, function(y) {
    prop_modecat <- (max(table(y))/length(y)) * 1.5 # to avoid getting bimodality from a single datapoint
    noise <- prop_modecat
    v_M <- rep(NA, 10)
    for(i in 1:10) v_M[i] <- DensMMdet(X=na.omit(y), adjust=2, noise=noise)$M
    tb <- table(v_M)
    as.numeric(names(tb)[which.max(tb)]) > 1 # multimodal?
  })
})[, -1]
# ----- VAS -----
l_WPM$Modality$VAS <- ddply(data_VAS, .(Name), function(x) {
  apply(x[,sel], 2, function(y) {
    base_noise <- 0.035
    prop_modecat <- (max(table(y))/length(y)) * 1.5
    noise_adj0 <- base_noise + prop_modecat / 5
    max_th_range <- 100
    noise <- noise_adj0 * max_th_range
    v_M <- rep(NA, 10)
    for(i in 1:10) v_M[i] <- DensMMdet(X=na.omit(y), adjust=2, noise=noise)$M
    tb <- table(v_M)
    as.numeric(names(tb)[which.max(tb)]) > 1 # multimodal?
  })
})[, -1]
# ----- VAS mapped to Likert -----
l_WPM$Modality$VAS2Lik <- ddply(data_VAS, .(Name), function(x) {
  apply(x[,sel], 2, function(y) {
    y_res <- (cut(x=y, breaks=seq(0, 100, length=8), include.lowest=TRUE, labels=FALSE) - 1) / 6
    prop_modecat <- (max(table(y_res))/length(y_res)) * 1.5 # to avoid getting bimodality from a single datapoint
    noise <- prop_modecat
    v_M <- rep(NA, 10)
    for(i in 1:10) v_M[i] <- DensMMdet(X=na.omit(y_res), adjust=2, noise=noise)$M
    tb <- table(v_M)
    as.numeric(names(tb)[which.max(tb)]) > 1 # multimodal?
  })
})[, -1]

# Calculate proportions of bimodality
MM_Lik <- as.numeric(unlist(l_WPM$Mod$Lik))
round(sum(MM_Lik) / length(MM_Lik), 4)# Likert
MM_VAS <- as.numeric(unlist(l_WPM$Mod$VAS))
round(sum(MM_VAS) / length(MM_VAS), 4) # VAS

# MM_VAS2Lik <- as.numeric(unlist(l_WPM$Mod$VAS2Lik))
# sum(MM_VAS2Lik) / length(MM_VAS2Lik) # VAS2LIK


# -------- (7) Skew --------------------------------
# ----- Likert -----
l_WPM$Skew$Lik <- ddply(data_Lik, .(Name), function(x) {
  apply((x[,sel]-1)/6, 2, function(y) skewness(y, na.rm=TRUE))
})[, -1]
# ----- VAS -----
l_WPM$Skew$VAS <- ddply(data_VAS, .(Name), function(x) {
  apply(x[,sel]/100, 2, function(y) skewness(y, na.rm=TRUE))
})[, -1]
# ----- VAS mapped to Likert -----
l_WPM$Skew$VAS2Lik <- ddply(data_VAS, .(Name), function(x) {
  apply(x[,sel], 2, function(y) {
    y_2Lik <- (cut(x=y, breaks=seq(0, 100, length=8), include.lowest=TRUE, labels=FALSE) - 1) / 6
    skewness(y_2Lik, na.rm=TRUE)
  })
})[, -1]

# -------- (8) Initial Elevation Bias --------------------------------

# ---- Call for Likert and VAS -----
# ----- Likert -----
data_Lik_norm <- data_Lik
data_Lik_norm[, sel] <- (data_Lik[, sel] - 1) /6
IEB_Lik <- f_IEB(data_Lik_norm, sel=sel)
l_WPM$IEB$Lik <- IEB_Lik
# ----- VAS -----
data_VAS_norm <- data_VAS
data_VAS_norm[, sel] <- data_VAS[, sel] / 100
IEB_VAS <- f_IEB(data_VAS_norm, sel=sel)
l_WPM$IEB$VAS <- IEB_VAS
# ----- VAS mapped to Likert -----
data_Lik2VAS_norm <- data_VAS
data_Lik2VAS_norm[, sel] <- sapply(data_VAS[, sel], function(y) {
  (cut(x=y, breaks=seq(0, 100, length=8), include.lowest=TRUE, labels=FALSE)-1)/6
})
data_Lik2VAS_norm[, 1] <- unlist(data_VAS[, 1])
data_Lik2VAS_norm <- as.data.frame(data_Lik2VAS_norm)
IEB_VAS2Lik <- f_IEB(data_Lik2VAS_norm, sel=sel)
l_WPM$IEB$VAS2Lik <- IEB_VAS2Lik

# ---- Save -----
saveRDS(l_WPM, "Files/AggData.RDS")


# ------------------------------------------------------------------------
# -------- External validation: Relate to BSI Scores ---------------------
# ------------------------------------------------------------------------

# Subset those that are included
out_BSI_Lik <- out_BSI[out_BSI$Name %in% data_Lik$Name, ]
out_BSI_VAS <- out_BSI[out_BSI$Name %in% data_VAS$Name, ]

data_cor_Lik <- cbind(l_WPM$Means$Lik, out_BSI_Lik$BSI)
data_cor_VAS <- cbind(l_WPM$Means$VAS, out_BSI_VAS$BSI)
data_cor_VAS2Lik <- cbind(l_WPM$Means$VAS2Lik, out_BSI_VAS$BSI)

# ----- Save -----
l_BSIcor <- list("Likert"=data_cor_Lik, 
                 "VAS"=data_cor_VAS, 
                 "VAS2Likert"=data_cor_VAS2Lik)
saveRDS(l_BSIcor, "Files/data_BSI_with_ESM.RDS")


# ------------------------------------------------------------------------
# -------- External validation: Relate to DASS Scores --------------------
# ------------------------------------------------------------------------

# Subset those that are included
out_DASS_Lik <- out_DASS[out_DASS$Name %in% data_Lik$Name, ]
out_DASS_VAS <- out_DASS[out_DASS$Name %in% data_VAS$Name, ]

# Stress
cor_Lik_S <- cbind(l_WPM$Means$Lik, out_DASS_Lik$DASS_S)
cor_VAS_S <- cbind(l_WPM$Means$VAS, out_DASS_VAS$DASS_S)
cor_VAS2Lik_S <- cbind(l_WPM$Means$VAS2Lik, out_DASS_VAS$DASS_S)
# Anxiety
cor_Lik_A <- cbind(l_WPM$Means$Lik, out_DASS_Lik$DASS_A)
cor_VAS_A <- cbind(l_WPM$Means$VAS, out_DASS_VAS$DASS_A)
cor_VAS2Lik_A <- cbind(l_WPM$Means$VAS2Lik, out_DASS_VAS$DASS_A)
# Depression
cor_Lik_D <- cbind(l_WPM$Means$Lik, out_DASS_Lik$DASS_D)
cor_VAS_D <- cbind(l_WPM$Means$VAS, out_DASS_VAS$DASS_D)
cor_VAS2Lik_D <- cbind(l_WPM$Means$VAS2Lik, out_DASS_VAS$DASS_D)


# ----- Save -----
l_DASScor <- list("cor_Lik_S" = cor_Lik_S, 
                  "cor_VAS_S" = cor_VAS_S, 
                  "cor_VAS2Lik_S" = cor_VAS2Lik_S,
                  "cor_Lik_A" = cor_Lik_A, 
                  "cor_VAS_A" = cor_VAS_A,
                  "cor_VAS2Lik_A" = cor_VAS2Lik_A,
                  "cor_Lik_D" = cor_Lik_D, 
                  "cor_VAS_D" = cor_VAS_D, 
                  "cor_VAS2Lik_D" = cor_VAS2Lik_D)
saveRDS(l_DASScor, "Files/data_DASS_with_ESM.RDS")


# ----------------------------------------------------
# -------- Within-person Means -----------------------
# ----------------------------------------------------

data_Lik <- readRDS("Files/data_ESM_Lik_incl.RDS")
data_VAS <- readRDS("Files/data_ESM_VAS_incl.RDS")

# ----- Compute Mean and SD of cors in two groups -----

p <- 14
# Likert
u_subj_Lik <- unique(data_Lik$Name)
n_subj_Lik <- length(u_subj_Lik)
a_cors_Lik <- array(NA, dim=c(p, p, n_subj_Lik))
for(i in 1:n_subj_Lik) a_cors_Lik[, , i] <- cor(data_Lik[data_Lik$Name==u_subj_Lik[i], 4:17], use="complete.obs")
# Average network
cors_av_Lik <- apply(a_cors_Lik, 1:2, function(x) mean(x, na.rm=TRUE))
cors_sd_Lik <- apply(a_cors_Lik, 1:2, function(x) sd(x, na.rm=TRUE))

# Likert
u_subj_VAS <- unique(data_VAS$Name)
n_subj_VAS <- length(u_subj_VAS)
a_cors_VAS <- array(NA, dim=c(p, p, n_subj_VAS))
for(i in 1:n_subj_VAS) a_cors_VAS[, , i] <- cor(data_VAS[data_VAS$Name==u_subj_VAS[i], 4:17], use="complete.obs")
# Average network
cors_av_VAS <- apply(a_cors_VAS, 1:2, function(x) mean(x, na.rm=TRUE))
cors_sd_VAS <- apply(a_cors_VAS, 1:2, function(x) sd(x, na.rm=TRUE))

l_cors_av <- list("cors_av_Lik" = cors_av_Lik, 
                  "cors_av_VAS" = cors_av_VAS)
saveRDS(l_cors_av, file="Files/l_cors_av.RDS")


# ----- Make Permutation test: H0: correlations are equal across groups -----

# Empirical group differences
cors_diff <- cors_av_Lik - cors_av_VAS

# Construct sampling distributions
Nperm <- 500
a_sampdistH0 <- array(NA, dim=c(p, p, Nperm))
N_Lik <- dim(a_cors_Lik)[3]
N_VAS <- dim(a_cors_VAS)[3]
N_tot <- N_Lik + N_VAS
a_cors_cmb <- array(NA, dim=c(p, p, N_Lik+N_VAS))
a_cors_cmb[, , 1:N_Lik] <- a_cors_Lik
a_cors_cmb[, , (N_Lik+1):N_tot] <- a_cors_VAS

set.seed(1)
for(i in 1:Nperm) {
  # Permute
  a_cors_cmb_perm <- a_cors_cmb[, , sample(1:N_tot, replace=FALSE)]
  m_mean_G1 <- apply(a_cors_cmb_perm[, , 1:N_Lik], 1:2, function(x) mean(x, na.rm=TRUE))
  m_mean_G2 <- apply(a_cors_cmb_perm[, , (N_Lik+1):N_tot], 1:2,function(x) mean(x, na.rm=TRUE))
  a_sampdistH0[, , i] <- m_mean_G1 - m_mean_G2
}

# Get test-statistic
hist(a_sampdistH0[1, 2, ], breaks=20)

# Compute p-values
a_pval <- matrix(NA, p, p)
for(i in 1:p) {
  for(j in 1:p) {
    a_pval[i, j] <- mean( abs(a_sampdistH0[i, j, ]) > abs(cors_diff[i,j]))
  }
}

# Save
saveRDS(a_pval, file="Files/a_pval_Cor_PTest.RDS")





