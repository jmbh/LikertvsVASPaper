# jonashaslbeck@gmail.com; May 30th, 2024

# ----------------------------------------------------
# -------- What is happening here? -------------------
# ----------------------------------------------------

# Take inputs from various places and plot figures

# ----------------------------------------------------
# -------- Loading Packages --------------------------
# ----------------------------------------------------

# Plotting
library(RColorBrewer)
library(scales)
library(HDInterval)
library(dplyr)
library(tidyr)

source("0_aux_Functions.R")

# ----------------------------------------------------
# -------- Global Settings ---------------------------
# ----------------------------------------------------

vFig <- "June7th"
sel <- 4:17 # indicating cols of data
p <- length(sel)

cols <- brewer.pal(3, "Set1")

# ----------------------------------------------------
# -------- Loading Processed Files -------------------
# ----------------------------------------------------

# Aggregate statistics
l_WPM <- readRDS("Files/AggData.RDS")
table(is.na(l_WPM$RMSSD$VAS$Happy))
table(is.na(l_WPM$RMSSD$Lik$Happy))


# data to compute correlations between ESM means and BSI
l_BSIcor <- readRDS("Files/data_BSI_with_ESM.RDS")
# data to compute correlations between ESM means and DASS Subscales
l_DASScor <- readRDS("Files/data_DASS_with_ESM.RDS")

# Main data [for sanity checks]
data_Lik <- readRDS("Files/data_ESM_Lik_incl.RDS")
data_VAS <- readRDS("Files/data_ESM_VAS_incl.RDS")

# Load Results from: Multilevel Models
load("Files/figuredata.Rdata")
saveRDS(figuredata, file="Files/figuredata.RDS")
ModOut <- readRDS(file="Files/figuredata.RDS")
pars <- rbind(ModOut[[1]], ModOut[[2]])
# NEW: for EMMs
load("Files/EMM_EMV.Rdata")
saveRDS(EMM_EMV, "Files/EMM_EMV.RDS")
EMM_EMV <- readRDS("Files/EMM_EMV.RDS")

# NEW: New EMMs (for VAS2Likert)
load("Files/EMM_Mapping.Rdata")
saveRDS(EMM_Mapping, "Files/EMM_Mapping.RDS")
EMM_Mapping <- readRDS("Files/EMM_Mapping.RDS")

# Load Models for External validation
load("Files/Correlations_BSI_summary_stats.Rdata")
load("Files/Correlations_DASS_A_summary_stats.Rdata")
load("Files/Correlations_DASS_D_summary_stats.Rdata")
load("Files/Correlations_DASS_S_summary_stats.Rdata")
Model_BSI <- Correlations_BSI_summary_stats
Model_DASS_A <- Correlations_DASS_A_summary_stats
Model_DASS_D <- Correlations_DASS_D_summary_stats
Model_DASS_S <- Correlations_DASS_S_summary_stats

# Analyses Correlation matrices
l_cors_av <- readRDS(file="Files/l_cors_av.RDS")
cors_av_Lik <- l_cors_av$cors_av_Lik
cors_av_VAS <- l_cors_av$cors_av_VAS
a_pval_Cor_PTest <- readRDS(file="Files/a_pval_Cor_PTest.RDS")

# Get Names
names <- colnames(data_VAS)

# ----- Some Descriptives ------
N_Lik <- length(unique(data_Lik$Name))
N_VAS <- length(unique(data_VAS$Name))



# ----------------------------------------------------
# -------- Sanity Check: Plot Means vs. BSI ----------
# ----------------------------------------------------

pdf(paste0("Figures/SanityCheck_Cors_BSI_", vFig, ".pdf"), width=5, height=25)

# --- Layout ---
lmat <- matrix(1:(15*2), ncol=2, byrow=T)
layout(mat=lmat, heights = c(0.2, rep(1, 14)))

# --- Plot Labels ---
plotLabels("Likert")
plotLabels("VAS")

# --- Plot Data ---
par(mar=c(4,4.5 ,2,1))
for(i in 1:14) {
  # Likert
  plot(l_BSIcor$Lik[, i], l_BSIcor$Lik[,15], 
       ylab=colnames(l_BSIcor$Lik)[i], xlab="BSI", xlim=c(0,1), 
       ylim=c(40, 180),
       pch=19)  
  cor_i <- cor(l_BSIcor$Lik[, i], l_BSIcor$Lik[,15], use="complete.obs")
  title(main=paste0("Cor = ", round(cor_i, 2)), font.main=1)
  abline(lm(l_BSIcor$Lik[,15]~l_BSIcor$Lik[, i]), col="red")
  
  # VAS
  plot(l_BSIcor$VAS[, i], l_BSIcor$VAS[,15], 
       ylab=colnames(l_BSIcor$Lik)[i], xlab="BSI", xlim=c(0,1), 
       ylim=c(40, 180),
       pch=19)  
  cor_i <- cor(l_BSIcor$VAS[, i], l_BSIcor$VAS[,15], use="complete.obs")
  title(main=paste0("Cor = ", round(cor_i, 2)), font.main=1)
  abline(lm(l_BSIcor$VAS[,15]~l_BSIcor$VAS[, i]), col="red")
  
} # end for: variables

dev.off()

# ----------------------------------------------------
# -------- Main Results Figures ----------------------
# ----------------------------------------------------

# ---------- Plotting function: with variation ------------

plotFig <- function(data,
                    title,
                    include = 1:2,
                    ymin=0,
                    ymax=1,
                    basejit = 0.24,
                    ylab="Normalized Scale",
                    plotData=TRUE,
                    plotQuant=TRUE,
                    location="mean",
                    legend=FALSE,
                    legendLoc="topright",
                    zeroline=FALSE) {
  
  cols <- brewer.pal(3, "Set1")
  if(3 %in% include) jit <- basejit*c(1, 0, -1) else jit <- basejit*c(-0.75, 0.75)
  alpha_p <- 0.2
  bar <- 0.075
  mean_cex <- 1.5
  point_cex <- 1
  bar_lwd <- 2.5
  
  v_pch <- c(19,15,17)
  
  Ns <- c(N_Lik, N_VAS, N_VAS)[include]
  
  # Canvas etc.
  par(mar=c(6,4,2.5,0.5))
  plot.new()
  plot.window(xlim=c(1,14), ylim=c(ymin,ymax))
  axis(1, labels=names[sel], at=1:14, las=2)
  axis(2, las=2)
  title(ylab=ylab, line=2.5)
  title(main=title, font.main=1, adj=0, cex.main=1.4)
  # title(main="Data", font.main=1, adj=0, line=-1)
  abline(v=seq(1.5, 13.5, length=14-1), lty=2, col="lightgrey")
  if(legend) legend(legendLoc, legend=c("Likert Scale",
                                        "Visual Analogue Scale",
                                        "VAS mapped to Likert")[include],
                    text.col=cols[include], bty='n', cex=1, pch=c(19,15,17), col=cols)
  if(zeroline) abline(h=0, lty=1, col="grey")
  
  # Data
  if(plotData) {
    for(i in 1:p) {
      for(j in include) {
        points(rep(i-jit[j], Ns[j]), data[[j]][ ,i],
               col=scales::alpha(cols[j], alpha=alpha_p),
               cex=point_cex, pch=v_pch[j])
      }
    }
  }
  
  # Compute CIs + plot
  if(plotQuant) {
    for(j in include) {
      qntl_Lik <- apply(data[[j]], 2, function(x) quantile(x, probs = c(0.05, 0.95), na.rm = TRUE))
      segments(1:p-jit[j], qntl_Lik[1,] , 1:p-jit[j], qntl_Lik[2,], col="black", lwd=bar_lwd)
      segments(1:p-jit[j]-bar, qntl_Lik[1,], 1:p-jit[j]+bar, qntl_Lik[1,], col="black", lwd=bar_lwd)
      segments(1:p-jit[j]-bar, qntl_Lik[2,], 1:p-jit[j]+bar, qntl_Lik[2,], col="black", lwd=bar_lwd)
      segments(1:p-jit[j], qntl_Lik[1,] , 1:p-jit[j], qntl_Lik[2,], col=cols[j], lwd=1)
      segments(1:p-jit[j]-bar, qntl_Lik[1,], 1:p-jit[j]+bar, qntl_Lik[1,], col=cols[j], lwd=1)
      segments(1:p-jit[j]-bar, qntl_Lik[2,], 1:p-jit[j]+bar, qntl_Lik[2,], col=cols[j], lwd=1)
    }
  }
  # Compute + plot means
  if(location=="median") {
    for(j in include) {
      
      # browser()
      points(1:p-jit[j], apply(data[[j]], 2, function(x) median(x, na.rm=T)), cex=mean_cex, col="black", pch=v_pch[j])
      points(1:p-jit[j], apply(data[[j]], 2, function(x) median(x, na.rm=T)), cex=mean_cex-.35, col=cols[j], pch=v_pch[j])
    }
  } else {
    for(j in include) {
      points(1:p-jit[j], apply(data[[j]], 2, function(x) mean(x, na.rm=T)), cex=mean_cex, col="black", pch=v_pch[j])
      points(1:p-jit[j], apply(data[[j]], 2, function(x) mean(x, na.rm=T)), cex=mean_cex-.35, col=cols[j], pch=v_pch[j])
    }
  }
  
} # eoF


# ---------- Plotting function: External Validation [From Model] ------------

plotFig_extVal <- function(Model, 
                           legend=TRUE, 
                           main=NULL, 
                           jit = 0.15) {
  
  cols <- brewer.pal(3, "Set1")
  
  # Canvas etc.
  par(mar=c(6.1,4,2.5,0.5))
  plot.new()
  plot.window(xlim=c(1,16), ylim=c(-1,1))
  axis(1, labels=c(names[sel], "", ""), at=1:16, las=2)
  axis(1, labels="Average Abs", at=15, las=2, font=2)
  axis(1, labels="Difference", at=16, las=2, font=2)
  axis(2, las=2)
  title(ylab="Pearson's Correlation", line=2.5)
  title(main=main, font.main=1, adj=0)
  # Box for average
  box_h <- 0.5
  rect(15-box_h, -1, 16+box_h, 1, col="#e6e6e6", border=FALSE)
  # Legend
  if(legend) legend(6, -.5, legend=c("Likert Scale",
                                     "Visual Analogue Scale"),
                    text.col=cols, bty='n', cex=1, pch=c(19,15,17), col=cols)
  abline(h=seq(-1, 1, length=5), lty=2, col="grey")
  
  # Reorder Items
  ord <- c(7, 5, 10, 13, 12, 11, 6, 2, 4, 1, 8, 3, 9, 14, 15) # because the model output is ordered alphabetically
  cex_p <- 1
  
  ### Likert:
  # Medians
  points((1:15)-jit, Model$Median[Model$Format == "Likert"][ord], col="black", pch=19, cex=cex_p)
  points((1:15)-jit, Model$Median[Model$Format == "Likert"][ord], col=cols[1], pch=19, cex=cex_p-.1)
  # HDIs
  for(i in 1:15) {
    low <- Model$HDI_Low[Model$Format == "Likert"][ord][i]
    high <- Model$HDI_High[Model$Format == "Likert"][ord][i]
    segments(i-jit, low, i-jit, high, col=cols[1])
  }
  ### VAS:
  # Medians
  points((1:15)+jit, Model$Median[Model$Format == "VAS"][ord], col="black", pch=15, cex=cex_p)
  points((1:15)+jit, Model$Median[Model$Format == "VAS"][ord], col=cols[2], pch=15, cex=cex_p-.1)
  # HDIs
  for(i in 1:15) {
    low <- Model$HDI_Low[Model$Format == "VAS"][ord][i]
    high <- Model$HDI_High[Model$Format == "VAS"][ord][i]
    segments(i+jit, low, i+jit, high, col=cols[2])
  }
  # Add Median+HDI for difference
  points(16, Model$Median[Model$Format == "Diff"], pch=19)
  segments(16, Model$HDI_Low[Model$Format == "Diff"], 16, Model$HDI_High[Model$Format == "Diff"], pch=19)
  
  
} # eoF


# ---------- Plotting function: External Validation + VAS2Likert ------------

plotFig_extVal_VAS2Lik <- function(Model,
                                   Model2,
                                   legend=TRUE, 
                                   main=NULL, 
                                   jit = 0.15) {
  
  cols <- brewer.pal(3, "Set1")
  
  # Canvas etc.
  par(mar=c(6.1,4,2.5,0.5))
  plot.new()
  plot.window(xlim=c(1,17), ylim=c(-1,1))
  cex.axis=0.8
  axis(1, labels=c(names[sel], "", "", ""), at=1:17, las=2, cex.axis=cex.axis)
  axis(1, labels="Average Abs", at=15, las=2, font=2, cex.axis=cex.axis)
  axis(1, labels="VAS - Likert", at=16, las=2, font=2, cex.axis=cex.axis)
  axis(1, labels="VAS - VAS2Lik", at=17, las=2, font=2, cex.axis=cex.axis)
  axis(2, las=2)
  title(ylab="Pearson's Correlation", line=2.5)
  title(main=main, font.main=1, adj=0)
  # Box for average
  box_h <- 0.5
  rect(15-box_h, -1, 17+box_h, 1, col="#e6e6e6", border=FALSE)
  # Legend
  if(legend) legend(6, -.5, legend=c("Likert Scale",
                                     "Visual Analogue Scale", 
                                     "VAS Mapped to Likert"),
                    text.col=cols, bty='n', cex=1, pch=c(19,15,17), col=cols)
  abline(h=seq(-1, 1, length=5), lty=2, col="grey")
  
  # Reorder Items
  ord <- c(7, 5, 10, 13, 12, 11, 6, 2, 4, 1, 8, 3, 9, 14, 15) # because the model output is ordered alphabetically
  cex_p <- 1
  
  ### Likert:
  # Medians
  points((1:15)-jit, Model$Median[Model$Format == "Likert"][ord], col="black", pch=19, cex=cex_p)
  points((1:15)-jit, Model$Median[Model$Format == "Likert"][ord], col=cols[1], pch=19, cex=cex_p-.1)
  # HDIs
  for(i in 1:15) {
    low <- Model$HDI_Low[Model$Format == "Likert"][ord][i]
    high <- Model$HDI_High[Model$Format == "Likert"][ord][i]
    segments(i-jit, low, i-jit, high, col=cols[1])
  }
  
  ### VAS:
  # Medians
  points((1:15)+jit, Model$Median[Model$Format == "VAS"][ord], col="black", pch=15, cex=cex_p)
  points((1:15)+jit, Model$Median[Model$Format == "VAS"][ord], col=cols[2], pch=15, cex=cex_p-.1)
  # HDIs
  for(i in 1:15) {
    low <- Model$HDI_Low[Model$Format == "VAS"][ord][i]
    high <- Model$HDI_High[Model$Format == "VAS"][ord][i]
    segments(i+jit, low, i+jit, high, col=cols[2])
  }
  
  # Add Median+HDI for difference
  points(16, Model$Median[Model$Format == "Diff"], pch=19)
  segments(16, Model$HDI_Low[Model$Format == "Diff"], 16, Model$HDI_High[Model$Format == "Diff"], pch=19)
  
  # Add: EMM for VAS2Lik
  points(15, EMM_Mapping$VAS2Lik[EMM_Mapping$Estimate=="Median" & EMM_Mapping$Model==Model2], pch=17, col=cols[3])
  segments(15, EMM_Mapping$VAS2Lik[EMM_Mapping$Estimate=="Lower" & EMM_Mapping$Model==Model2], 
           15, EMM_Mapping$VAS2Lik[EMM_Mapping$Estimate=="Upper" & EMM_Mapping$Model==Model2], col=cols[3])
  
  # Add: EEM: VAS - VAS2Lik
  points(17, EMM_Mapping$DiffVASVAS2Lik[EMM_Mapping$Estimate=="Median" & EMM_Mapping$Model==Model2], pch=17, col="black")
  segments(17, EMM_Mapping$DiffVASVAS2Lik[EMM_Mapping$Estimate=="Lower" & EMM_Mapping$Model==Model2], 
           17, EMM_Mapping$DiffVASVAS2Lik[EMM_Mapping$Estimate=="Upper" & EMM_Mapping$Model==Model2], col="black")
  
} # eoF



# ---------- Plotting function: External Validation [From Data] ------------
# We still use this for a figure in the appendix (I think)

plotFig_extVal_Data <- function(data1,
                                data2=NULL,
                                outcome,
                                legend=FALSE,
                                cols = brewer.pal(3, "Set1"),
                                add=FALSE, 
                                pch=19) {
  
  if(add == FALSE) {
    par(mar=c(6,4,2.5,0.5))
    plot.new()
    plot.window(xlim=c(1,14), ylim=c(-1,1))
    axis(1, labels=names[sel], at=1:14, las=2)
    axis(2, las=2)
    title(ylab="Pearson's Correlation", line=2.5)
    title(main=paste0("Correlation of Means with ", outcome), font.main=1, adj=0)
    if(legend) legend("bottomright", legend=c("Likert Scale",
                                              "Visual Analogue Scale"),
                      text.col=cols, bty='n', cex=1, pch=c(19,15,17), col=cols)
    abline(h=seq(-1, 1, length=5), lty=2, col="#e8e8e8")
  }
  # abline(h=0, lty=2, col="grey", lwd=2)
  
  # Compute Correlations
  
  
  cex_p <- 1
  
  jit <- 0
  # Likert
  corm_Lik_S <- cor(data1, use='complete.obs')[15, -15]
  points((1:14)-jit, corm_Lik_S, col="black", pch=pch, cex=cex_p)
  points((1:14)-jit, corm_Lik_S, col=cols[1], pch=pch, cex=cex_p-.1)
  # VAS
  if(!is.null(data2)) {
    corm_VAS_S <- cor(data2, use='complete.obs')[15, -15]
    points((1:14)-jit, corm_VAS_S, col="black", pch=pch, cex=cex_p)
    points((1:14)-jit, corm_VAS_S, col=cols[2], pch=pch, cex=cex_p-.1)
  }
} # eoF


# ----------------------------------------------------------
# -------- Make Panels with Multilevel results [OLD ]-------
# ----------------------------------------------------------

PlotPars <- function(Model="Means", xlim=c(-2,2)) {
  
  # Setup Layout
  squeeze <- 2.5
  par(mar=c(4+squeeze,6,2+squeeze,1))
  plot.new()
  plot.window(xlim=xlim, ylim=c(0,1))
  axis(1)
  y_seq <- seq(0.9, 0.1, length=4)
  axis(2, at=y_seq, labels=c("Intercept", "VAS", "Valence", "VAS:Valence"), las=2)
  abline(h=y_seq, col="grey", lty=3)
  abline(v=0)
  title(xlab="Coefficient", line=2.5)
  title(main="Posteriors of Fixed Effects", font.main=1, line=1, cex=0.9)
  
  u_pars <- unique(pars$.variable)
  
  # Plot
  for(i in 1:4) {
    # Mean
    pars_ij <- pars$.value[pars$.variable==u_pars[i] &  pars$Model==Model]
    points(median(pars_ij), y_seq[i], pch=19, cex=1.1)
    
    # HDI
    hdi_ints <- hdi(pars_ij, credMass = 0.9)
    segments(hdi_ints[1], y_seq[i], hdi_ints[2], y_seq[i], lwd=2.5)
  }
  
} # eoF


# ----------------------------------------------------------------
# -------- Make Panels with EMMs from Bayes ML-models ------------
# ----------------------------------------------------------------

# table(EMM_EMV$Model)

PlotParsEMM <- function(Model="Mean", ylim=c(-.1, .6)) {
  
  # Setup Layout
  # par(mar=c(4,6,2,3))
  squeeze <- 1.5
  par(mar=c(6,5,2.5+squeeze,2))
  plot.new()
  plot.window(xlim=c(1-.25,3.25), ylim=ylim)
  axis(1, 1:3, labels=FALSE)
  axis(1, 1, labels="Likert", col.axis=cols[1], las=2)
  axis(1, 2, labels="VAS", col.axis=cols[2], las=2)
  axis(1, 3, labels="Difference", col.axis="black", las=2)
  axis(2, las=2)
  title(ylab="Grand Mean", line=3)
  title(main="Estimated Marginal Means", font.main=1, cex.main=1, line=1)
  # Grey background
  rect(1-.25, ylim[1], 3.25, ylim[2], col="#e8e8e8", border=FALSE)
  abline(h=0, col="black", lty=2) # Zero-line
  # xw
  # Get mean component, for given Model
  pars <- EMM_EMV[EMM_EMV$Model == Model & EMM_EMV$Component == "Mean", ]
  
  ## Plotting data
  vdis <- (ylim[2]-ylim[1]) * 0.2
  # Plot Likert:
  points(1, pars$Likert[1], pch=19, cex=1.2, col=cols[1])
  text(1, pars$Likert[1]+vdis, round(pars$Likert[1],2), col=cols[1])
  segments(1, pars$Likert[2], 1, pars$Likert[3], lwd=3, col=cols[1])
  # Plot VAS:
  points(2, pars$VAS[1], pch=15, cex=1.2, col=cols[2])
  text(2, pars$VAS[1]+vdis, round(pars$VAS[1],2), col=cols[2])
  segments(2, pars$VAS[2], 2, pars$VAS[3], lwd=3, col=cols[2])
  # Plot Diff:
  points(3, pars$Diff[1], pch=19, cex=1.2)
  text(3, pars$Diff[1]+vdis, round(pars$Diff[1],2))
  segments(3, pars$Diff[2], 3, pars$Diff[3], lwd=3)
  
} # eoF


# ----------------------------------------------------------------
# -------- Make Panels with EMMs incl VAS2Likert -----------------
# ----------------------------------------------------------------

PlotParsEMM_VAS2Lik <- function(Model="MappingMean", 
                                ylim=c(-.1, .6)) {
  
  squeeze <- 1.5
  par(mar=c(6,5,2.5+squeeze,2))
  plot.new()
  plot.window(xlim=c(1-.25,5.25), ylim=ylim)
  axis(1, 1:5, labels=FALSE)
  cex.axis=0.8
  axis(1, 1, labels="Likert", col.axis=cols[1], las=2, cex.axis=cex.axis)
  axis(1, 2, labels="VAS", col.axis=cols[2], las=2, cex.axis=cex.axis)
  axis(1, 3, labels="VAS - Lik", col.axis="black", las=2, cex.axis=cex.axis)
  axis(1, 4, labels="VAS2Lik", col.axis=cols[3], las=2, cex.axis=cex.axis)
  axis(1, 5, labels="VAS - VAS2Lik", col.axis="black", las=2, cex.axis=cex.axis)
  
  axis(2, las=2)
  title(ylab="Grand Mean", line=3)
  title(main="Estimated Marginal Means", font.main=1, cex.main=1, line=1)
  # Grey background
  rect(1-.25, ylim[1], 5.25, ylim[2], col="#e8e8e8", border=FALSE)
  abline(h=0, col="black", lty=2) # Zero-line
  # Get mean component, for given Model
  pars <- EMM_Mapping[EMM_Mapping$Model == Model, ]
  
  ## Plotting data
  vdis <- (ylim[2]-ylim[1]) * 0.2
  # Plot Likert:
  points(1, pars$Likert[pars$Estimate=="Median"], pch=19, cex=1.2, col=cols[1])
  text(1, pars$Likert[pars$Estimate=="Median"]+vdis, round(pars$Likert[pars$Estimate=="Median"],2), col=cols[1])
  segments(1, pars$Likert[pars$Estimate=="Lower"], 1, pars$Likert[pars$Estimate=="Upper"], lwd=3, col=cols[1])
  # Plot VAS:
  points(2, pars$VAS[pars$Estimate=="Median"], pch=19, cex=1.2, col=cols[2])
  text(2, pars$VAS[pars$Estimate=="Median"]+vdis, round(pars$VAS[pars$Estimate=="Median"],2), col=cols[2])
  segments(2, pars$VAS[pars$Estimate=="Lower"], 2, pars$VAS[pars$Estimate=="Upper"], lwd=3, col=cols[2])
  # Plot Diff:
  points(3, pars$DiffVASLik[pars$Estimate=="Median"], pch=19, cex=1.2, col="black")
  text(3, pars$DiffVASLik[pars$Estimate=="Median"]+vdis, round(pars$DiffVASLik[pars$Estimate=="Median"],2), col="black")
  segments(3, pars$DiffVASLik[pars$Estimate=="Lower"], 3, pars$DiffVASLik[pars$Estimate=="Upper"], lwd=3, col="black")
  # Plot VAS2Likert:
  points(4, pars$VAS2Lik[pars$Estimate=="Median"], pch=19, cex=1.2, col=cols[3])
  text(4, pars$VAS2Lik[pars$Estimate=="Median"]+vdis, round(pars$VAS2Lik[pars$Estimate=="Median"],2), col=cols[3])
  segments(4, pars$VAS2Lik[pars$Estimate=="Lower"], 4, pars$VAS2Lik[pars$Estimate=="Upper"], lwd=3, col=cols[3])
  # Plot Diff:
  points(5, pars$DiffVASVAS2Lik[pars$Estimate=="Median"], pch=19, cex=1.2, col="black")
  text(5, pars$DiffVASVAS2Lik[pars$Estimate=="Median"]+vdis, round(pars$DiffVASVAS2Lik[pars$Estimate=="Median"],2), col="black")
  segments(5, pars$DiffVASVAS2Lik[pars$Estimate=="Lower"], 5, pars$DiffVASVAS2Lik[pars$Estimate=="Upper"], lwd=3, col="black")
  
} # eoF

# ----------------------------------------------------
# -------- Figures Main Text -------------------------
# ----------------------------------------------------


# ---- Main results Part 1 (Univariate) ----
pdf(paste0("Figures/Fig_Res_New_P1_", vFig, ".pdf"), width=7, height = 10*0.75)
# --- Make Layout ---
lmat <- matrix(1:6, ncol=2, byrow = TRUE)
layout(lmat, widths = c(.6, .25))
# --- Means ---
plotFig(data=l_WPM$Means, title="A. Within-person Means", legend=TRUE, plotQuant=FALSE)
PlotParsEMM(Model="Mean", ylim=c(-.1, .4))
# --- SDs ---
plotFig(data=l_WPM$SDs, title="B. Within-person Standard Deviations", ymax=0.4, plotQuant=FALSE)
PlotParsEMM(Model="SD", ylim=c(-.1, .25))
# --- Skewness ---
plotFig(data=l_WPM$Skew, title="C. Within-person Skewness", 
        ymin=-4, ymax=9, zeroline=TRUE, plotQuant=FALSE)
PlotParsEMM(Model="Skew", ylim=c(-1, 2))
dev.off()

### Specific values for paper
EMM_EMV[EMM_EMV$Model=="Skew" & EMM_EMV$Component=="Mean", ]
EMM_EMV[EMM_EMV$Model=="Skew" & EMM_EMV$Component=="Var", ]


# ---- Main results Part 2 (Correlations) ----
pdf(paste0("Figures/Fig_Res_New_P2_", vFig, ".pdf"), width=7, height = 6.5)
# --- Make Layout ---
lmat <- rbind(c(rep(1, 7), rep(2, 3)), 
              c(rep(3, 5), rep(4, 5)))
layout(lmat, heights = c(.475, .6))
# --- Abs Correlation ---
plotFig(data=l_WPM$CorLag0, title="Within-person Mean Absolute Correlations", 
        ymin=0, ymax=0.7, plotQuant=FALSE, legend=TRUE, legendLoc = "topleft")
PlotParsEMM(Model="CorLag0", ylim=c(-.1, .4))
# ---- Inspect Means ---- Note: Make Heatplots instead
PlotHeat(data=cors_av_Lik, 
         names=names[4:17], cex=0.5,
         title="Mean Within-Person Correlations (Likert)")
PlotHeat(data=cors_av_VAS, 
         names=names[4:17], cex=0.5,
         title="Mean Within-Person Correlations (VAS)")
dev.off()

# Results for paper: correlation between correlation matrices
cors <- cor(cors_av_Lik[upper.tri(cors_av_Lik)], cors_av_VAS[upper.tri(cors_av_VAS)])
round(cors, 3)
diff <- (cors_av_Lik[upper.tri(cors_av_Lik)] - cors_av_VAS[upper.tri(cors_av_VAS)])
round(mean(abs(diff)), 3)


# ---- Main results Part 3 (Dynamics) ----
sc <- 1.1
pdf(paste0("Figures/Fig_Res_New_P3_", vFig, ".pdf"), width=7*sc*1.2, height = 10*0.6*sc)
# --- Make Layout ---
lmat <- matrix(1:4, ncol=2, byrow = TRUE)
layout(lmat, widths = c(.6, .25))
# --- RMSSD ---
plotFig(data=l_WPM$RMSSD, title="A. Within-person RMSSD", 
        legend=TRUE, legendLoc = "top",
        ymin=0, ymax=0.5, plotQuant=FALSE)
PlotParsEMM(Model="RMSSD", ylim=c(-.1, .25))
# --- Autocorrelations ---
plotFig(data=l_WPM$AR, title="B. Within-person Autocorrelation", 
        ymax=1, ymin=-.3, zeroline=TRUE, plotQuant=FALSE)
PlotParsEMM(Model="AR", ylim=c(-.2, .8))
dev.off()

### Specific values for paper
EMM_EMV[EMM_EMV$Model=="AR" & EMM_EMV$Component=="Mean", ]
EMM_EMV[EMM_EMV$Model=="AR" & EMM_EMV$Component=="Var", ]



# ---- External Validation: BSI + DASS ----
sc <- 1.5
pdf(paste0("Figures/Fig_ExtVal", vFig, ".pdf"), width = 6*sc, height = 5*sc)

par(mfrow=c(2,2))
# --- BSI ---
plotFig_extVal(Model = Model_BSI, legend=T, 
               main = "General Psychopathology (BSI)")
# --- DASS: Stress ---
plotFig_extVal(Model = Model_DASS_S, legend=F, 
               main = "Stress (DASS)")
# --- DASS: Anxiety ---
plotFig_extVal(Model = Model_DASS_A, legend=F, 
               main = "Anxiety (DASS)")
# --- DASS: Depression ---
plotFig_extVal(Model = Model_DASS_D, legend=F, 
               main = "Depression (DASS)")

dev.off()


# --- Specific values for paper ---
round(Model_BSI$Median[Model_BSI$Format == "VAS"][15] / Model_BSI$Median[Model_BSI$Format == "Likert"][15], 2)
round(Model_DASS_S$Median[Model_DASS_S$Format == "VAS"][15] / Model_DASS_S$Median[Model_DASS_S$Format == "Likert"][15], 2)
round(Model_DASS_A$Median[Model_DASS_A$Format == "VAS"][15] / Model_DASS_A$Median[Model_DASS_A$Format == "Likert"][15], 2)
round(Model_DASS_D$Median[Model_DASS_D$Format == "VAS"][15] / Model_DASS_D$Median[Model_DASS_D$Format == "Likert"][15], 2)



# ----------------------------------------------------
# -------- Figures Appendix --------------------------
# ----------------------------------------------------

# ----- Figure: Cor Matrices: Differences + P-value distribution ------
sc <- 1.3
pdf(paste0("Figures/Fig_App_CorMat_DiffandTest_", vFig, ".pdf"), width=7*sc*1.2, height = 10*0.38*sc)
par(mfrow=c(1,2))

# Differences Heatplot
par(mar=c(4,4.3,3,1))
PlotHeat(data=cors_av_Lik-cors_av_VAS, 
         pvals = a_pval_Cor_PTest,
         alpha = 0.10,
         names=names[4:17], cex=0.5,
         title="Difference: Likert - VAS", 
         mar=c(5.5,5.5,1,0))

# P-value distribution
par(mar=c(4,4.3,3,1))
hist(a_pval_Cor_PTest[upper.tri(a_pval_Cor_PTest)], breaks=seq(0, 1, length=20), axes=FALSE, main="", 
     xlab="")
title(xlab="P-value in Permutation Test", line=2)
axis(1)
axis(2, las=2)
abline(h=(14*13/2)/20, col="orange", lwd=2)
title(main = "Distribution of p-values", font.main=1, line=.8)
legend("center", text.col="orange", legend=c("Distribution under H0"), bty="n")
dev.off()

# Reporting for Paper: p-values
a_sig <- a_pval_Cor_PTest[upper.tri(a_pval_Cor_PTest)] < 0.10
sum(a_sig) # number of significant at alpha=0.10
round(sum(a_sig) / (14*13/2), 3)
(14*13/2) * 0.10

# ----- Figure: Cor Matrices: Scatter Plot + Eigen Decomposition ------
sc <- 1.3
pdf(paste0("Figures/Fig_App_CorMat_ScatterandEigen_", vFig, ".pdf"), width=7*sc*1.2, height = 10*0.38*sc)
par(mfrow=c(1,2))

# Plot Diffs against each other
v_cors_av_Lik <- cors_av_Lik[upper.tri(cors_av_Lik)]
v_cors_av_VAS <- cors_av_VAS[upper.tri(cors_av_VAS)]
par(mar=c(4,4.3,3,1))
plot.new()
plot.window(xlim=c(-.5, .75), ylim=c(-.5, .75))
axis(1, seq(-.5, .75, length=6))
axis(2, seq(-.5, .75, length=6), las=2)
title(xlab="Correlations Likert", line=2.5)
title(ylab="Correlations VAS")
abline(v=0, lty=2, col="lightgrey")
abline(h=0, lty=2, col="lightgrey")
abline(0,1, col="grey")
points(v_cors_av_Lik, v_cors_av_VAS)
title(main="Distribution of Correlations across Scales", font.main=1)

# Eigen values
eigen_Lik <- eigen(cors_av_Lik)$value
eigen_VAS <- eigen(cors_av_VAS)$value
cols <- brewer.pal(3, "Set1")
plot.new()
plot.window(xlim=c(1, 14), ylim=c(0,5))
axis(1, 1:14, cex.axis=0.9)
axis(2, las=2)
title(main="Eigenvalues", font.main=1)
title(ylab="Value", line=2.5)
title(xlab="Order", line=2.5)
lines(eigen_Lik, col=cols[1], lwd=2)
lines(eigen_VAS, col=cols[2], lwd=2)
legend("center", text.col=cols[1:2], legend=c("Likert", "Visual Analogue Scale"), bty="n")

dev.off()




# ------ Figure: IEB -------

pdf(paste0("Figures/Fig_Res_IEB_", vFig, ".pdf"), width=9, height = 10*0.4)
# --- Make Layout ---
lmat <- matrix(1:2, ncol=2, byrow = TRUE)
layout(lmat, widths = c(.6, .4))
plotFig(data=l_WPM$IEB, title="Initial Elevation Bias", 
        ymin=-0.6, ymax=0.6, zeroline=TRUE, plotQuant=FALSE)
PlotParsEMM(Model="IEB", ylim=c(-.2, .8)) # TODO: THIS IS WEIRD!
dev.off()


# ------ Figure: External Validation with VAS2Likert added -------
sc <- 1.5
pdf(paste0("Figures/Fig_ExtVal", vFig, "_VAS2Likert.pdf"), width = 6*sc, height = 5*sc)

par(mfrow=c(2,2))

# --- BSI ---
plotFig_extVal_VAS2Lik(Model = Model_BSI, legend=F, 
                       Model2="MappingBSI",
                       main = "General Psychopathology (BSI)", 
                       jit = .23)
plotFig_extVal_Data(data1 = l_BSIcor$VAS2Likert, 
                    add=TRUE,
                    outcome = "MappingBSI",
                    cols = brewer.pal(3, "Set1")[3], 
                    pch=17)
legend("bottomright", 
       legend=c("Likert Scale",
                "Visual Analogue Scale",
                "VAS mapped to Likert"),
       text.col=cols, bty='n', cex=1, pch=c(19,15,17), col=cols)

# --- DASS: Stress ---
plotFig_extVal_VAS2Lik(Model = Model_DASS_S, legend=F, 
                       Model2="MappingDASS_S",
                       main = "Stress (DASS)", 
                       jit = .23)
plotFig_extVal_Data(data1 = l_DASScor$cor_VAS2Lik_S, 
                    add=TRUE,
                    outcome = "",
                    cols = brewer.pal(3, "Set1")[3], 
                    pch=17)

# --- DASS: Anxiety ---
plotFig_extVal_VAS2Lik(Model = Model_DASS_A, legend=F, 
                       Model2="MappingDASS_A",
                       main = "Anxiety (DASS)", 
                       jit = .23)
plotFig_extVal_Data(data1 = l_DASScor$cor_VAS2Lik_A, 
                    add=TRUE,
                    outcome = "",
                    cols = brewer.pal(3, "Set1")[3], 
                    pch=17)
# --- DASS: Depression ---
plotFig_extVal_VAS2Lik(Model = Model_DASS_D, legend=F, 
                       Model2="MappingDASS_D",
                       main = "Depression (DASS)", 
                       jit = .23)
plotFig_extVal_Data(data1 = l_DASScor$cor_VAS2Lik_S, 
                    add=TRUE,
                    outcome = "",
                    cols = brewer.pal(3, "Set1")[3], 
                    pch=17)
dev.off()


# ------ Figure: VAS2Likert for Means -------
sc <- 1.4
pdf(paste0("Figures/Fig_VAS2Lik_Means_", vFig, ".pdf"), width=7*sc, height = 3.5*sc)
# --- Make Layout ---
lmat <- matrix(1:2, ncol=2, byrow = TRUE)
layout(lmat, widths = c(.6, .3))
# --- VAS2Likert Means ---
plotFig(data=l_WPM$Means, title="Means - VAS mapped to Likert", 
        legend=TRUE, include=1:3)
PlotParsEMM_VAS2Lik(Model="MappingMean")
dev.off()

# ------ Figure: VAS2Likert for Skew -------
sc <- 1.4
pdf(paste0("Figures/Fig_VAS2Lik_Skew_", vFig, ".pdf"), width=7*sc, height = 3.5*sc)
# --- Make Layout ---
lmat <- matrix(1:2, ncol=2, byrow = TRUE)
layout(lmat, widths = c(.6, .3))
# --- VAS2Likert Means ---
plotFig(data=l_WPM$Skew, title="Skew - VAS mapped to Likert", 
        legend=TRUE, include=1:3, 
        ymin=-2,
        ymax=10, 
        legendLoc = "topleft")
PlotParsEMM_VAS2Lik(Model="MappingSkew", ylim=c(-2, 4))
dev.off()

# ------ Figure: VAS2Likert for Cors -------
sc <- 1.4
pdf(paste0("Figures/Fig_VAS2Lik_Cors_", vFig, ".pdf"), width=7*sc, height = 3.5*sc)
# --- Make Layout ---
lmat <- matrix(1:2, ncol=2, byrow = TRUE)
layout(lmat, widths = c(.6, .3))
# --- VAS2Likert Means ---
plotFig(data=l_WPM$CorLag0, title="Abs Cor Lag 0 - VAS mapped to Likert", 
        legend=TRUE, include=1:3, 
        ymin=0,
        ymax=.8)
PlotParsEMM_VAS2Lik(Model="MappingCorLag0")
dev.off()

# ------ Figure: VAS2Likert for AR1 -------
sc <- 1.4
pdf(paste0("Figures/Fig_VAS2Lik_AR1_", vFig, ".pdf"), width=7*sc, height = 3.5*sc)
# --- Make Layout ---
lmat <- matrix(1:2, ncol=2, byrow = TRUE)
layout(lmat, widths = c(.6, .3))
# --- VAS2Likert Means ---
plotFig(data=l_WPM$AR, title="AR-1 - VAS mapped to Likert", 
        legend=TRUE, include=1:3, 
        ymin=-0.2,
        ymax=1, 
        legendLoc = "top")
PlotParsEMM_VAS2Lik(Model="MappingAR", ylim=c(-.1, .5))
dev.off()



# ----------------------------------------------------
# -------- Illustration Figure: Thresholding ---------
# ----------------------------------------------------

# --- Get one VAS distribution ---
u_subj_VAS <- unique(data_VAS$Name)
data_VAS_i <- data_VAS[data_VAS$Name == u_subj_VAS[i], ]
data_VAS_i$Happy
# --- Get one VAS distribution ---
y_2Lik <- cut(x=data_VAS_i$Happy, breaks=seq(0, 100, length=8), include.lowest=TRUE, labels=FALSE)

# --- Plot ---
sc <- 1.1
pdf("Figures/Fig_Illu_thresh.pdf", width=5*sc, height = 6*sc)
par(mfrow=c(2,1), mar=c(3,4,2,1))
hist(data_VAS_i$Happy, breaks=seq(0, 100, length=101), 
     main="VAS Data", xlab="", axes=FALSE, font.main=1)
abline(v=seq(0, 100, length=8), lty=2, col="grey")
hist(data_VAS_i$Happy, breaks=seq(0, 100, length=101), 
     main="VAS Data", xlab="", axes=FALSE, font.main=1, add=TRUE)
axis(1)
axis(2, las=2)
barplot(table(y_2Lik), axes=FALSE)
# hist(y_2Lik, main="", xlab="", axes=FALSE)
# axis(1)
axis(2, las=2)
title(ylab="Frequency")
dev.off()




# ----------------------------------------------------
# -------- Sanity: Look at all time series plots -----
# ----------------------------------------------------

# I made this check, but not yet with the complete data
# Just checking, to make sure no anomalies

u_subj_VAS <- unique(data_VAS$Name)
u_subj_Lik <- unique(data_Lik$Name)


pdf(paste0("Figures/SanityCheck_Likert_", vFig, ".pdf"), width = 7, height = 5)
for(i in 1:length(u_subj_Lik)) {
  
  # Subset data
  data_Lik_i <- data_Lik[data_Lik$Name == u_subj_Lik[i], ] # get data from one person
  par(mfrow=c(3,5))
  for(j in 4:17) {
    par(mar=c(4,3,2,1))
    plot.new()
    plot.window(xlim=c(1,84), ylim=c(1,7))
    axis(1)
    axis(2, las=2)
    lines(data_Lik_i[, j], col=j-3)
    title(main=paste0(u_subj_Lik[i], "; Item = ", colnames(data_Lik)[j]), font.main=1, cex.main=0.7)
  }
  # Empty
  plot.new()
  plot.window(xlim=c(1,84), ylim=c(1,7))
  
} # end loop
dev.off()


pdf(paste0("Figures/SanityCheck_VAS_", vFig, ".pdf"), width = 7, height = 5)
for(i in 1:length(u_subj_VAS)) {
  
  # Subset data
  data_VAS_i <- data_VAS[data_VAS$Name == u_subj_VAS[i], ]
  par(mfrow=c(3,5))
  for(j in 4:17) {
    par(mar=c(4,3,2,1))
    plot.new()
    plot.window(xlim=c(1,84), ylim=c(0,100))
    axis(1)
    axis(2, las=2)
    lines(data_VAS_i[, j], col=j-3)
    title(main=paste0(u_subj_VAS[i], "; Item = ", colnames(data_VAS)[j]), font.main=1, cex.main=0.7)
  }
  # Empty
  plot.new()
  plot.window(xlim=c(1,84), ylim=c(1,7))
  
} # end loop
dev.off()



