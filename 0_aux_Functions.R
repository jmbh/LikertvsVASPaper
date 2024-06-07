# jonashaslbeck@gmail.com; April 8th, 2024

# ----------------------------------------------------
# -------- What is happening here? -------------------
# ----------------------------------------------------

# Some aux functions used in the analysis


# ----------------------------------------------------
# -------- Function to Plot Labels -----------------
# ----------------------------------------------------

plotLabels <- function(tex, srt=0, x=0.5, y=0.5, cex=1.2) {
  par(mar=rep(0, 4))
  plot.new()
  plot.window(xlim=c(0, 1), ylim=c(0, 1))
  text(x, y, adj=0.3, labels=tex, srt=srt, cex=cex)
}


# ----------------------------------------------------
# -------- Function to make Heatplot -----------------
# ----------------------------------------------------

PlotHeat <- function(data, 
                     pvals = NULL,
                     alpha = NULL,
                     names, 
                     title=NULL, 
                     cex=0.7, 
                     mar=c(5,5,1,0)) {
  
  # -- Prep data --
  p <- ncol(data)
  
  # data_show <- t(data)
  data_show <- data[p:1, ]
  
  # -- Make color gradient --
  color.gradient <- function(x, colors=c("#7E2F8E", "white", "#4DBD33"), colsteps=201) {
    return( colorRampPalette(colors) (colsteps) [ findInterval(x, seq(min(x),max(x), length.out=colsteps)) ] )
  }
  x <- 1:201
  grad <- color.gradient(x)
  
  # -- Make canvas --
  par(mar=mar)
  plot.new()
  plot.window(xlim=c(0,1), ylim=c(0, 1))
  title(main=title, font.main=1, line=-1.25)
  
  # Auxiliary plotting variables
  sfm <- 1/(p*2)
  seq_mp_x <- seq(0, 1, length=p+1)[-(p+1)] + sfm
  
  # Plot Axes
  axis(2, labels = names[p:1][-p], at=seq_mp_x[-p], cex.axis=0.8, las=2)
  axis(1, labels = names[-p], at=seq_mp_x[-p], las=2, cex.axis=0.8)
  
  # Perform significance test
  if(!is.null(pvals)) {
    m_sig <- pvals < alpha
    m_sig <- m_sig[p:1, ]
  } else {
    m_sig <- matrix(FALSE, 14, 14)
  }
  
  # Plot Data
  for(i in 1:(p-1)) {
    for(j in 1:(p-i)) {
      rect(xleft = seq_mp_x[i]-sfm,
           ybottom = seq_mp_x[j]-sfm,
           xright = seq_mp_x[i]+sfm,
           ytop = seq_mp_x[j]+sfm,
           col = grad[data_show[j, i] * 100*(1/0.60) + 101], border="grey")
      
      # Make text
      if(m_sig[j, i]) txtadd <- "*" else txtadd <- ""
      show_text <- paste0(round(data_show[j,i] , 2), txtadd)
      text(seq_mp_x[i], seq_mp_x[j], show_text, cex=cex, col="black")
      # } # end if
    }
  }
  
} # eoF


# ------------------------------------------------------
# -------- MM Detection via GMM + roots ----------------
# ------------------------------------------------------
# Taken from:
# https://github.com/jmbh/ModalitySkewnessPaper/blob/main/aux_Functions.R

DensMMdet <- function(X, adjust=3, noise=0.4, n = 100, plot=FALSE) {
  
  # Add noise
  Xn <- X + rnorm(length(X), 0, noise)
  
  # Density Estimation
  den <- density(Xn, bw="SJ", adjust=adjust, n=n)
  if(plot) plot(den)
  
  # Compute number of reversals
  ni <- length(den$y) 
  
  diff <- den$y[-ni] - den$y[-1]
  
  sign_reversals <- sign(diff[-(ni-1)]) != sign(diff[-1])
  Nrev <- sum(sign_reversals)
  
  Modality <- ceiling(Nrev/2) # since between each mode there is another reversal
  
  outlist <- list("M" = Modality,
                  "den_x" = den$x,
                  "den_y" = den$y)
  
  return(outlist)
  
} # eoF


# ----------------------------------------------------
# -------- Computing Initial Elevation Bias (IEB) ----
# ----------------------------------------------------


f_IEB <- function(data, sel, cutoff=8) {
  
  u_subj <- unique(data$Name)
  
  out <- t(sapply(u_subj, function(x) {
    # Subset subjects
    subj_j <- data[data$Name==x, ]
    
    # Subset variables for fixed subject j
    out_j <- sapply(sel, function(y) {
      mean_initial <- mean(unlist(subj_j[1:cutoff, y]), na.rm=TRUE)
      mean_later <- mean(unlist(subj_j[(cutoff+1):84, y]), na.rm=TRUE)
      bias <- mean_initial - mean_later # so: positive = IEB
      return(bias)
    })
    return(out_j)
  }))
  return(out)
  
} # eof









