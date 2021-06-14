library(irrCAC)

func_irr <- function(alignment) {
  #
  irrstat <- data.frame(song=character(), transcriber=character(), group=character(),
                        coef=double(), stderr=double(), pval = double(), coef_lb=double(), coef_ub=double(),
                        criticalval=double(), benchmark=character(), benchmark_lb=double(), benchmark_ub=double(),
                        N=double(), K=double(), R=double(), averagePID = double())
  
  #
  al <- 0.05
  be <- 0.05
  benchmarking <- data.frame(scale=character(), description=character(), lb=double(), ub=double())
  benchmarking <- rbind(benchmarking, data.frame(scale='Altman', description='Very Good', lb=0.8, ub=1.0))
  benchmarking <- rbind(benchmarking, data.frame(scale='Altman', description='Good', lb=0.6, ub=0.8))
  benchmarking <- rbind(benchmarking, data.frame(scale='Altman', description='Moderate', lb=0.4, ub=0.6))
  benchmarking <- rbind(benchmarking, data.frame(scale='Altman', description='Fair', lb=0.2, ub=0.4))
  benchmarking <- rbind(benchmarking, data.frame(scale='Altman', description='Poor', lb=0.0, ub=0.2))
  
  #
  uniqelem <- unique(strsplit(paste(alignment, collapse = ""), "")[[1]])
  rating <- matrix(0, nrow = nchar(alignment[1]), ncol = length(uniqelem))
  seqmat <- lapply(alignment, function(x) strsplit(x, "")[[1]])
  
  for (n in 1:dim(rating)[1]) {
    for (k in 1:length(uniqelem)) {
      rating[n, k] <- sum(unlist(lapply(seqmat, function(x) x[n] == uniqelem[k])))
    }
  }
  
  #
  irr <- fleiss.kappa.dist(rating, weights="unweighed", conflev=0.95, N=Inf)
  
  #
  N <- nchar(alignment[1])
  R <- rowSums(rating)[1]
  p_emp <- colSums(rating)/(R * N)
  
  mean_null <- -1/(N*(R - 1))
  var_null <- 2/(N*R*(R - 1)*sum(p_emp*(1 - p_emp))^2) * (sum(p_emp*(1 - p_emp))^2 - sum(p_emp*(1 - p_emp)*(1 - 2*p_emp)))
  
  z <- (irr$coeff - mean_null)/sqrt(var_null)
  pval <- 1 - pnorm(z, mean = 0, sd = 1)
  
  #
  numrow <- nrow(irrstat)
  irrstat[numrow + 1, ]$coef <- irr$coeff
  irrstat[numrow + 1, ]$stderr <- irr$stderr
  irrstat[numrow + 1, ]$pval <- pval
  irrstat[numrow + 1, ]$coef_lb <- qnorm(al/2, irr$coeff, irr$stderr)
  irrstat[numrow + 1, ]$coef_ub <- qnorm(1 - al/2, irr$coeff, irr$stderr)
  
  #
  criticalval <- qnorm(be, irr$coeff, irr$stderr)
  
  if (criticalval == 1) {
    idx <- 1
  } else if(criticalval < 0) {
    idx <- nrow(benchmarking)
  } else {
    idx <- benchmarking$lb <= criticalval & criticalval < benchmarking$ub
  }
  
  #
  irrstat[numrow + 1, ]$criticalval <- criticalval
  irrstat[numrow + 1, ]$benchmark <- benchmarking[idx, ]$description;
  irrstat[numrow + 1, ]$benchmark_lb <- benchmarking[idx, ]$lb;
  irrstat[numrow + 1, ]$benchmark_ub <- benchmarking[idx, ]$ub;
  
  irrstat[numrow + 1, ]$N <- dim(rating)[1]
  irrstat[numrow + 1, ]$K <- dim(rating)[2]
  irrstat[numrow + 1, ]$R <- length(alignment)
  
  #
  return(irrstat)
}