## useful_functions.R

#input the marriage market choice set data and create additional 
#variables that will be used throughout the analyses
createVariables <- function(market) {
  market$rexog.gs <- createExogamyTerms(market$raceh, market$racew, symmetric=TRUE)
  market$rexog <- createExogamyTerms(market$raceh, market$racew, symmetric=FALSE)
  market$nexog <- createExogamyTerms(factor(market$bplh==1, c(FALSE,TRUE), c("Imm","Nat")), 
                                     factor(market$bplw==1, c(FALSE,TRUE), c("Imm","Nat")))
  #create scalar educational terms
  market$educh.s <- as.numeric(market$educh)
  market$educw.s <- as.numeric(market$educw)
  
  #hypergamy/hypogamy
  market$edhyper <- market$educh > market$educw
  market$edhypo <- market$educh < market$educw
  
  #create boundary crossing terms
  #market$ed.diff <- abs(market$educh.s - market$educw.s)
  #market$edcross1 <- market$ed.diff>0
  #market$edcross2 <- market$ed.diff>1
  #market$edcross3 <- market$ed.diff>2
  market$edcross_hs <- (market$educh>="HS" & market$educw<"HS") | (market$educw>="HS" & market$educh<"HS")
  market$edcross_sc <- (market$educh>="SC" & market$educw<"SC") | (market$educw>="SC" & market$educh<"SC")
  market$edcross_c <- (market$educh>="C" & market$educw<"C") | (market$educw>="C" & market$educh<"C")
  
  
  market$bendog <- market$bplh==market$bplw
  market$lendog <- market$langh==market$langw

  return(market)
}

#made a function for pooling models with bife
#tests indicate that for models with a few variables, pooling with the survival function is faster
#but for modles with many variables bife is faster. however the bife results were about 10% higher in magnitude
#on aver age with a few outliers that were 52% and 35% higher - so I think I will stick with the 
#standard method
poolChoiceModel_bife <- function(formula, datasets) {
  if(!require(bife)) {
    stop("bife package must be installed.")
  }
  models <- lapply(datasets, function(dataset) {bife(formula, data=dataset)})
  
  m <- length(models)
  b <- sapply(models, coef)
  se <- sapply(models, function(model) {summary(model)$cm[,2]})
  bic <- sapply(models, function(model) {(model$deviance-model$null_deviance)+length(model$coef)*log(nrow(model$data))})
  
  b.pool <- apply(b,1,mean)
  between.var <- apply(b,1,var)
  within.var <- apply(se^2,1,mean)
  se.pool <- sqrt(within.var+between.var+between.var/m)
  z.pool <- b.pool/se.pool
  pvalue.pool <- (1-pnorm(abs(z.pool)))*2
  
  return(list(coefficients=data.frame(b.pool,se.pool,z.pool,pvalue.pool,within.var,between.var),
              bic=mean(bic)))
}

#a better model with parallel processing possibility
poolChoiceModel <- function (formula, datasets, parallel=TRUE) 
{
  if (!require(survival)) {
    stop("survival package must be installed.")
  }
  if(parallel) {
    models <- mclapply(datasets, function(dataset) {
      clogit(formula, data = dataset)
    })
  } else {
    models <- lapply(datasets, function(dataset) {
      clogit(formula, data = dataset)
    })
  }
  m <- length(models)
  b <- sapply(models, coef)
  se <- sapply(models, function(model) {
    summary(model)$coef[, 3]
  })
  bic <- sapply(models, function(model) {
    diff(-2 * model$loglik) + length(model$coef) * log(sum(model$y[, 
                                                                   2]))
  })
  b.pool <- apply(b, 1, mean)
  between.var <- apply(b, 1, var)
  within.var <- apply(se^2, 1, mean)
  se.pool <- sqrt(within.var + between.var + between.var/m)
  z.pool <- b.pool/se.pool
  pvalue.pool <- (1 - pnorm(abs(z.pool))) * 2
  return(list(coefficients = data.frame(b.pool, se.pool, z.pool, 
                                        pvalue.pool, within.var, between.var), bic = bic))
}

#for conversion to texreg
convertModel <- function(model) {
  tr <- createTexreg(
    coef.names = rownames(model$coef), 
    coef = model$coef$b.pool, 
    se = model$coef$se.pool, 
    pvalues = model$coef$pvalue.pool,
    gof.names = c("BIC (null)"), 
    gof = c(mean(model$bic)), 
    gof.decimal = c(F)
  )
}
