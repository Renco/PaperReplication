#This code replicates the work of Lettau and Ludvigson 2001b
# data_preparation --------------------------------------------------------
require(data.table)
require(lubridate)
require(quantmod)
require(ggplot2)
require(Matrix)
setwd("/Users/renco/Dropbox/Projects/LettauLudvigson2001b")
sample <- 'original' # full data span or orginal data span
if(sample == 'original'){
  #Fama French factors 
  ff_factors <- read.table("FF_factor.txt",header = TRUE)
  ff_factors <- data.table(ff_factors)
  ff_factors[,("Mkt.RF") := .(Mkt.RF + RF)]
  names(ff_factors)[2] <- "Rvw"
  ff_factors[,c("RF"):=NULL]
  
  month.2.quarter <- function(month){
    date.string <- paste(month,"01",sep = "")
    date <- ymd(date.string)
    #quarter <- paste(year(date),"0",quarter(date),sep="")
    quarter <- as.yearqtr(ymd(date.string))
    return(quarter)
  }
  #month.2.quarter(198603) #for test
  
  quarter.return <- function(return){
    #return should be a vector of monthly return
    temp <- 100 * (prod(return/100 + 1) - 1)  
    #temp <- sum(return)
    return(temp)
  }
  
  ff_factors[,"quarter":=.(month.2.quarter(month))]
  ff_factors <- ff_factors[,lapply(.SD, quarter.return), by = .(quarter)]
  ff_factors <- ff_factors[quarter >= "1963 Q3" & quarter <= "1998 Q3"]
  ff_factors[, "month" := NULL]
  #cay
  cay_data <- read.table("cay.txt",header=TRUE)
  cay_data <- data.table(cay_data)
  cay <- cay_data[1:.N-1,.(Quarter,cay)]
  cay <- cay[Quarter >= 196302 & Quarter <= 199802] #lag cay
  #cay <- cay[Quarter >= 196303 & Quarter <= 199803]
  cay[,("quarter"):=as.yearqtr(parse_date_time(Quarter,orders="Yq"))]
  cay[,("Quarter"):=NULL]
  #demean cay
  cay[,("cay"):=.(cay - mean(cay))]
  #delta y
  delta_y <- cay_data[1:.N-1,.(Quarter,labor.income.y)]
  delta_y <- delta_y[Quarter >= 196303 & Quarter <= 199803]
  #delta_y[,("d_y"):=c(0,diff(labor.income.y)) * 100]
  delta_y[,("d_y"):=c(0,diff(labor.income.y)) ]
  delta_y[,"labor.income.y":=NULL]
  delta_y <- delta_y[1:(.N-1)]
  #delta_y <- delta_y[2:.N]
  delta_y[,("quarter"):=as.yearqtr(parse_date_time(Quarter,orders="Yq"))]
  delta_y[,("Quarter"):=NULL]
  #the data for labor income growth only goes to second quarater 
  #of 1998
  #d_c consumption growth
  d_c <- cay_data[1:.N-1,.(Quarter,consumption.c)]
  d_c <- d_c[Quarter >= 196303 & Quarter <= 199803]
  # d_c[,("d_c"):=c(diff(consumption.c),0) * 100]
  # d_c <- d_c[1:(.N-1)]
  d_c[,("d_c"):=c(0,diff(consumption.c)) * 100]
  d_c <- d_c[2:(.N)]
  d_c[,("quarter"):=as.yearqtr(parse_date_time(Quarter,orders="Yq"))]
  d_c[,c("Quarter","consumption.c"):=NULL]
  
  
  #25 FF portfolios
  ff_ports <- read.table("25_Portfolios.txt",header=T)
  ff_ports <- data.table(ff_ports)
  ff_ports[,"quarter":=.(month.2.quarter(month))]
  ff_ports <- ff_ports[,lapply(.SD, quarter.return), by = .(quarter)]
  ff_ports[, "month" := NULL]
  ff_ports <- ff_ports[quarter >= "1963 Q3" & quarter <= "1998 Q3"]
  
  
  # #price index
  # p.i <- read.table("price_index.csv",header=TRUE,sep=",")
  # p.i <- data.table(p.i)
  # #p.i[,("DATE"):= ymd(DATE)]
  # p.i[,("quarter"):=as.yearqtr(DATE,format = "%Y-%m-%d")]
  # p.i[,("inflation"):= c(0,diff(log(CPIAUCSL)))]
  # p.i[,c("DATE","CPIAUCSL"):=NULL]
  # p.i <- p.i[,lapply(.SD,quarter.return), by =.(quarter)]
  # p.i[,("inflation"):=.(inflation * 100)]
  # p.i <- p.i[quarter >= "1963 Q3" & quarter <= "1998 Q3"]
  # 
  # #calculate real return
  # deflate <- function(vec,inf.vec){
  #   return(vec - inf.vec)
  # }
  # 
  # #deflate for FF portfolios and FF factors
  # ff_ports[,2:26] <- lapply(ff_ports[,2:26],deflate,inf.vec = p.i[,inflation])
  # ff_factors[,2:4] <- lapply(ff_factors[,2:4],deflate,inf.vec = p.i[,inflation])
  
  pst.mean <- ff_factors[,1:2]
  pst.mean[,("rolling.mean"):=rep(0,dim(ff_factors)[1])]
  for(j in 1:dim(pst.mean)[1]){
    pst.mean[j,3] <- colMeans(pst.mean[1:j,2])
  }
  #temp.plot(pst.mean,3)
  pst.mean[,("rolling.mean"):=.(rolling.mean - mean(rolling.mean))]

}else if (sample == "full"){
  
  #Fama French factors 
  ff_factors <- read.table("FF_factor.txt",header = TRUE)
  ff_factors <- data.table(ff_factors)
  ff_factors[,("Mkt.RF") := .(Mkt.RF + RF)]
  names(ff_factors)[2] <- "Rvw"
  ff_factors[,c("RF"):=NULL]
  ff_factors[,"quarter":=.(month.2.quarter(month))]
  ff_factors <- ff_factors[,lapply(.SD, quarter.return), by = .(quarter)]
  ff_factors <- ff_factors[quarter >= "1963 Q3" & quarter <= "2015 Q3"]
  ff_factors[, "month" := NULL]
  #cay
  cay_data <- read.table("cay_current.txt",header=TRUE)
  cay_data <- data.table(cay_data)
  cay <- cay_data[,.(date,cay)] #cay
  #cay <- cay_data[,.(date,cay_MS)] #Markov Switching cay
  cay <- cay[date >= 196302 & date <= 201502] #lag cay
  cay[,("quarter"):=as.yearqtr(parse_date_time(date,orders="Yq"))]
  cay[,("date"):=NULL]
  #demean cay
  cay[,("cay"):=.(cay - mean(cay))]
  
  #delta y
  delta_y <- cay_data[,.(date,y)]
  delta_y <- delta_y[date >= 196303 & date <= 201503]
  #let y_{t+1} - y_t be t's income growth rate 
  delta_y[,("d_y"):=c(diff(y),0) ]
  delta_y[,"y":=NULL]
  delta_y <- delta_y[1:(.N-1)]
  delta_y[,("quarter"):=as.yearqtr(parse_date_time(date,orders="Yq"))]
  delta_y[,("date"):=NULL]

  #d_c consumption growth
  d_c <- cay_data[,.(date,c)]
  d_c <- d_c[date >= 196303 & date <= 201503]
  d_c[,("d_c"):=c(0,diff(c)) * 100]
  d_c <- d_c[2:(.N)]
  d_c[,("quarter"):=as.yearqtr(parse_date_time(date,orders="Yq"))]
  d_c[,c("date","c"):=NULL]
  
  #25 FF portfolios
  ff_ports <- read.table("25_Portfolios.txt",header=T)
  ff_ports <- data.table(ff_ports)
  ff_ports[,"quarter":=.(month.2.quarter(month))]
  ff_ports <- ff_ports[,lapply(.SD, quarter.return), by = .(quarter)]
  ff_ports[, "month" := NULL]
  ff_ports <- ff_ports[quarter >= "1963 Q3" & quarter <= "2015 Q3"]
  
  #pst.mean 
  pst.mean <- ff_factors[,1:2]
  pst.mean[,("rolling.mean"):=rep(0,dim(ff_factors)[1])]
  for(j in 1:dim(pst.mean)[1]){
    pst.mean[j,3] <- colMeans(pst.mean[1:j,2])
  }
  #temp.plot(pst.mean,3)
  pst.mean[,("rolling.mean"):=.(rolling.mean - mean(rolling.mean))]
  
  
}


# Exploratory Plots -------------------------------------------------------
#cay 
temp.plot <- function(x,column){
  temp <- xts(x[[column]],x[,quarter])
  chartSeries(temp,name = names(x)[i])
}

# temp.plot(cay,1)
# temp.plot(delta_y,2)
# for(i in 2:4){
#   temp.plot(ff_factors,i)
# }
# require(reshape2)
# require(ggplot2)
# temp.df <- melt(ff_factors,id.vars = "quarter")
# #temp.df[,"quarter"]
# #time series plot for FF factors
# ggplot(temp.df, aes(x=quarter,y=value,colour = variable)) +
#   scale_x_yearqtr(format = "%YQ%q") + geom_line()
# 
# 
# #25 portfolios
# temp.df <- melt(ff_ports,id.vars= "quarter")
# ggplot(data = temp.df, aes(x=quarter,y=value))  +
#   facet_wrap(~variable,scales="free") +
#   geom_line() + scale_x_yearqtr(format = "%YQ%q")

# Fama-Macbeth procedure --------------------------------------------------
require(Matrix)
fama.macbeth <- function(port.df,factor.df){
  #ports.df is the data.frame for the portfolio's return
  #factors.df is the data.frame for the factors used 
  
  ##time series regression for beta
  beta.fit <-list()
  port.num <- dim(port.df)[2]
  time.span <- dim(factor.df)[1]
  factor.num <- dim(factor.df)[2]
  beta.mat <- matrix(rep(0,port.num * factor.num), ncol = port.num)
  for(i in 1:25){
    y <- port.df[[i]]
    x <- data.matrix(factor.df)
    temp.fit <- lm(y~x )
    beta.fit[[i]] <- temp.fit
    beta.mat[,i] <- temp.fit$coefficients[2:(factor.num+1)]
  }
  
  # for (i in 1:25){
  #   print(summary(beta.fit))
  # }
  # 
  ##cross-sectional regression for premiums
  premium.fit <- list()
  port.matrix <- data.matrix(port.df)[1:time.span,]
  fit.ret.mat <- matrix(rep(0, port.num * time.span),ncol=port.num)
  premium.mat <- matrix(rep(0,time.span * (1 + factor.num)),ncol = (factor.num+1))
  for(j in 1:time.span){
    y <- port.matrix[j,]
    x <- t(beta.mat)
    cross.fit <- lm(y~x)
    premium.fit[[j]] <- cross.fit
    premium.mat[j,] <- cross.fit$coefficients
    fit.ret.mat[j,] <- cross.fit$fitted.values
  }
    #premium.vec stores the estimates for \labmda's
    premium.vec <- colMeans(premium.mat)
    # for(i in 1:time.span){
    #   print(summary(premium.fit[[i]]))
    # }
    
    fitted.ret <- colMeans(fit.ret.mat)
    
    ##Jaganathan Wang R^2 
    R.mean.vec <- colMeans(port.df)
    #cross-sectional residuals 
    residual.mat <- matrix(rep(0,time.span * port.num), 
                              ncol = port.num)
    for(j in 1:time.span){
      residual.mat[j,] <- premium.fit[[j]]$residuals
    }
    residual.vec <- colMeans(residual.mat)
    R.sq <- ( var(R.mean.vec) - var(residual.vec) )/var(R.mean.vec)
    
    ##Adjusted R sqaured
    p = length(premium.vec) - 1 
    n = length(fitted.ret)
    adj.R.sq <- R.sq - (1 - R.sq) * p / (n-p-1)
    
    #t-statistics
    std.vec <-  sqrt(1/time.span * apply(premium.mat,2,var))
    ##Shanken's correction 
    S.crtion <- (1 + premium.vec^2 / c(var(premium.mat[,1]),apply(factor.df,2,var)))
    std.vec.S <- std.vec * sqrt(S.crtion)
    tstat <- premium.vec / std.vec
    tstat.S <- premium.vec / std.vec.S
    
    
    beta.mat <- t(beta.mat)
    #time series residuals
    tres.mat <- matrix(rep(0,time.span * port.num), 
                       ncol = port.num)
    for(i in 1:port.num){
      tres.mat[,i] <- beta.fit[[i]]$residuals
    }
    #time series residual covariance matrix
    sigma <- matrix(rep(0,port.num^2), ncol = port.num)
    for(j in 1:time.span){
      temp <- tres.mat[j,]
      temp <- as.matrix(temp,ncol = 1)
      temp <- temp %*% t(temp)
      sigma <- sigma + temp
    }
    sigma <- 1/time.span * sigma
    sigma <- diag(apply(tres.mat,2,var))
    
    #vanilla Fama Macbeth Covariance Matrix
    fm.cov <-   1/time.span * (
      solve(t(beta.mat) %*% beta.mat) %*%
      t(beta.mat) %*%
      sigma %*% beta.mat %*%
      solve(t(beta.mat) %*% beta.mat) +  cov(factor.df) ) 
    tt <- c(tstat[1],premium.vec[-1]/sqrt(diag(fm.cov)))
    
    #Shanken-corrected variance mat
    s.p.vec <- premium.vec[-1] #take out intercept
    S = as.vector(1 + s.p.vec %*% solve(cov(factor.df)) %*% s.p.vec)
    fm.cov.S <- 1/time.span * (
      solve(t(beta.mat) %*% beta.mat) %*%
        t(beta.mat) %*%
        sigma %*% beta.mat %*%
        solve(t(beta.mat) %*% beta.mat) * S +  
        cov(factor.df) ) 
    int.S <- sqrt(1 + (premium.vec[1] / sd(premium.mat[,1]))^2)
    ttt <- c(tstat[1]/int.S, premium.vec[-1]/sqrt(diag(fm.cov.S)))
    #Control for the covariance between factors and zero-beta portfolio
    fm.cov <- cov(premium.mat) / time.span
    shanken.vec <- as.matrix(S.crtion,ncol=1)
    fm.cov.S <- fm.cov * (shanken.vec %*% t(shanken.vec))
    
    
    ##Wald Test for joint signifiance
    k = length(premium.vec) - 1 #only test for  
    fm.cov <- fm.cov[-1,-1]
    fm.cov.S <- fm.cov.S[-1,-1]
    n = dim(port.df)[2]
    R <- diag(k)
    Acov <- fm.cov * n
    b <- as.matrix(premium.vec[-1], ncol = 1)
    q <- R %*% b 
    p <- solve(R %*% Acov %*% t(R))
    W = n * t(q) %*% p %*% q
    test.sig <- 1 - pchisq(W,df=k)
    
    
    ##Wald Test for joint signifiance(Shanken corrected)
    Acov.S <- diag(k)
    Acov.S <- fm.cov.S * n 
    p.S <- solve(R %*% Acov.S %*% t(R))
    W.S = n * t(q) %*% p.S %*% q
    test.sig.S <- 1 - pchisq(W.S,df=k)
    
    
    ##alpha joint test for pricing errors
    cov.alpha <- 1/time.span * cov(residual.mat)
    # temp <- as.matrix(residual.vec,ncol=1)
    # cov.alpha <- 1/time.span * temp %*% t(temp)
    # cov.alpha <- matrix(rep(0,port.num^2), ncol = port.num)
    # for(j in 1:time.span){
    #   temp <- as.matrix(residual.mat[j,], ncol = 1)
    #   cov.alpha <- cov.alpha + temp %*% t(temp)
    # }
    # cov.alpha <- 1/time.span * cov.alpha #estimate
    # cov.alpha <- 1/time.span * cov.alpha #time series indepedentcy
    alpha <- as.matrix(residual.vec, ncol = 1)
    # alpha.test <- S * t(alpha) %*% 
    #             solve(cov.alpha,tol = 1e-20) %*% alpha
    alpha.test <- 1/S * t(alpha) %*% 
      solve(Matrix(cov.alpha),alpha)
    alpha.test <- as.numeric(alpha.test)
    alpha.df <- port.num - dim(factor.df)[2]
    alpha.sig <- 1 - pchisq(alpha.test, df = alpha.df)
    
    ##Report
    result <- list()
    names(premium.vec) <- c("R_0",names(factor.df))
    names(R.sq) <- "R squared"
    names(tstat) <- c("R_0",names(factor.df))
    names(tstat.S) <- c("R_0",names(factor.df))
    result[[1]] <- premium.vec #estimates of premiums 
    result[[2]] <- R.sq # R square for the overall fitting 
    result[[3]] <- tstat #original t statistics 
    result[[4]] <- tstat.S #Shanken corrected t statistics 
    result[[5]] <- fitted.ret #fitted value from the model
    result[[6]] <- adj.R.sq
    result[[7]] <- test.sig
    result[[8]] <- test.sig.S
    result[[9]] <- c(alpha.sig,alpha.test)
    names(result) <- c("premium","R squared",
                       "t statistics",
                       "Shanken t statistics",
                       "fitted values",
                       "Adjusted R squared",
                       "Joint Significance",
                       "Joint Significane Shanken Corrected",
                       "Alpha Joint Test")
    return(result)
}

# Fitted Graph ------------------------------------------------------------
fit.plot <- function(port_ret,result){
  plot(port_ret,result[[5]],xlim=c(2,6) ,ylim=c(2,6))
  abline(a = 0, b = 1)
}

ff_ret <- colMeans(ff_ports[,2:26])

# Table 1 -----------------------------------------------------------------
report <- list()
fig1.df <- data.frame(ff_ret)
data.len <- dim(cay)[1]

##CAPM
port.df <- ff_ports[,2:26]
factor.df <- ff_factors[,2]
print("CAPM Model")
result <- fama.macbeth(port.df,factor.df)
fit.plot(ff_ret,result)
fig1.df <- cbind(fig1.df,result[[5]])
report[[1]] <- result

#Jaganathan and Wang  #this looks different than LL
income.len <- dim(delta_y)[1]
if(sample == "original") {
  factor.df <- cbind(ff_factors[1:data.len-1,2],delta_y[,"d_y"])
  port.df <- ff_ports[2:data.len,2:26]
} else if (sample == "full"){
  factor.df <- cbind(ff_factors[1:income.len,2],delta_y[,"d_y"])
  port.df <- ff_ports[1:income.len,2:26]
}
print("MODEL 2")
fama.macbeth(port.df,factor.df)

##Fama French model 3
port.df <- ff_ports[,2:26]
factor.df <- ff_factors[,2:4]
print("Fama Macbeth 3 Factor Model")
result <- fama.macbeth(port.df,factor.df)
fit.plot(ff_ret,result)
fig1.df <- cbind(fig1.df,result[[5]])
report[[3]] <- result

##cay CAPM model 4
port.df <- ff_ports[,2:26]
factor.df <- ff_factors[,2]
factor.df[,("cay"):=cay[,"cay"]]
factor.df[,("cayRvw"):=.(cay * Rvw)]
print("MODEL 4")
fama.macbeth(port.df,factor.df)

##model 5
factor.df[,("cay"):=NULL]
print("MODEL 5")
fama.macbeth(port.df,factor.df)
fit.plot(ff_ret,result)

##model 6 cay income CAPM
factor.df <- ff_factors[2:data.len,2]
factor.df[,("cay"):=cay[2:data.len,"cay"]]
factor.df[,("cayRvw"):=.(cay * Rvw)]
factor.df <- factor.df[1:income.len]
factor.df[,("d_y"):=delta_y[,"d_y"]]
factor.df[,("cay.d_y"):= .(cay * d_y)]
port.df <- ff_ports[2:data.len,2:26]
print("cay scaling income CAPM")
result <- fama.macbeth(port.df,factor.df)
fit.plot(ff_ret,result)
report[[4]] <- result 

##model 7
factor.df[,("cay"):=NULL]
print("MODEL 7")
fama.macbeth(port.df,factor.df)


# Table 3 -----------------------------------------------------------------
data.len <- dim(cay)[1]
##model 1
#consumption goes to 1998 quarter 2 for original data
factor.df <- d_c[,1]
# if (sample == "original"){
#   port.df <- ff_ports[1:(data.len-1),2:26] 
# } else if (sample == "full"){
#   port.df <- ff_ports[2:(data.len),2:26] 
# }
port.df <- ff_ports[2:(data.len),2:26] 
print("CCAPM Model")
result <- fama.macbeth(port.df,factor.df)
fit.plot(ff_ret,result)
fig1.df <- cbind(fig1.df,result[[5]])
report[[2]] <- result

#moddel 2
#port.df <- ff_ports[1:(data.len-1),2:26]
port.df <- ff_ports[2:(data.len),2:26]
factor.df <- d_c[,1]
factor.df[,("cay"):=cay[2:(data.len),"cay"]]
factor.df[,("cayd_c"):=.(cay * d_c)]
print("cay scaling CCAPM model")
result <- fama.macbeth(port.df,factor.df)
report[[5]] <- result
#use consumption defined as c_{t+1} - c_{t} as the t+1 th period 
#consuption growth will generate the preferrable result
fit.plot(ff_ret,result)
fig1.df <- cbind(fig1.df,result[[5]])
#model 3 
# factor.df[,("cayd_c"):=NULL]
# print("MODEL 3")
# fama.macbeth(port.df,factor.df)
factor.df[,("cay"):=NULL]
print("MODEL 3")
fama.macbeth(port.df,factor.df)
#doesn't work 

#past mean model scaling consumption 
port.df <- ff_ports[2:(data.len),2:26]
factor.df <- d_c[,1]
factor.df[,("pst.mean"):=pst.mean[2:(data.len),"rolling.mean"]]
factor.df[,("pst.m.d_c"):=.(pst.mean * d_c)]
print("past mean scaling consumption model")
result <- fama.macbeth(port.df,factor.df)
fit.plot(ff_ret,result)

#FF + past.mean 
##Fama French model 3
port.df <- ff_ports[,2:26]
factor.df <- ff_factors[,2:4]
factor.df[,"pst.mean"] <- pst.mean[,"rolling.mean"]
print("past mean as momentum model")
result <- fama.macbeth(port.df,factor.df)
fit.plot(ff_ret,result)



# Figure 1 ----------------------------------------------------------------
names(fig1.df) <- c("Realized Return","CAPM",
                    "Fama French 3 Factor","CCAPM","cay.CCAPM")
port.names <-numeric()
for (i in 1:5){
  for (j in 1:5)
    port.names <- c(port.names,i*10 + j)
}
port.names <- as.character(port.names)
row.names(fig1.df) <- port.names
# port.names[1] <- "sm.growth"
# port.names[25] <- "sm.value"
temp.df <- melt(fig1.df, id.vars = "Realized Return")
names(temp.df)[1] <- "renco"
temp.df[,"port.name"] <- rep(port.names,4)

s.g.text <- fig1.df[1,]
s.g.text <- melt(s.g.text,id.vars = "Realized Return")
s.g.text[,"port.name"] <- "sm.growth"
names(s.g.text)[1] <- "renco"
if(sample == "original"){
  s.g.text[,"x_s"] <- s.g.text[,"renco"] - 0.3
  s.g.text[,"y_s"] <- s.g.text[,"value"] + 1
} else if (sample == "full"){
  s.g.text[,"x_s"] <- s.g.text[,"renco"] - 0.1
  s.g.text[,"y_s"] <- s.g.text[,"value"] + 1
}
s.v.text <- fig1.df[5,]
s.v.text <- melt(s.v.text,id.vars = "Realized Return")
s.v.text[,"port.name"] <- "sm.value"
names(s.v.text)[1] <- "renco"
s.v.text[,"x_s"] <- s.v.text[,"renco"] + 0.3
s.v.text[,"y_s"] <- s.v.text[,"value"] - 1.2
if(sample == "original"){
  rq.text <- data.frame(x = rep(4,4), 
                        y = rep(5.8,4),
                        variable = s.v.text[,"variable"])
  rq.text[,"text"] = c("Rsq = 2%",
                       "Rsq = 79%",
                       "Rsq = 11%",
                       "Rsq = 67%")
} else if (sample == "full"){
  rq.text <- data.frame(x = rep(3.5,4), 
                        y = rep(5,4),
                        variable = s.v.text[,"variable"])
  rq.text[,"text"] = c("Rsq = 8%",
                       "Rsq = 73%",
                       "Rsq = 21%",
                       "Rsq = 54%")
}

require(ggthemes)
if(sample == "original"){
  #pdf("fig1.pdf",width = 10, height = 12)
  ggplot(data = temp.df, 
         aes(x=renco,y=value,label = port.name))  +
    facet_wrap(~variable,scales="free") +
    geom_text(colour = "blue") + xlim(2,6) + ylim(2,6) + 
    geom_abline(intercept = 0, slope = 1) + 
    xlab("Realzied Return (in %)") +
    ylab("Fitted Return (in %)") +
    geom_text(data = s.g.text, aes(x = x_s + 0.4, y = y_s + 0.15,label = port.name) ) +
    geom_segment(data = s.g.text, 
                 aes(x=x_s,xend = renco - 0.05,y=y_s, yend = value + 0.15),
                 arrow = arrow(length = unit(0.15, "cm"),type = "closed")) +
    geom_text(data = s.v.text, aes(x = x_s - 0.2, y = y_s +-0.15,label = port.name) ) +
    geom_segment(data = s.v.text, 
                 aes(x=x_s,xend = renco + 0.05,y=y_s, yend = value - 0.15),
                 arrow = arrow(length = unit(0.15, "cm"),type = "closed")) +
    geom_text(data = rq.text, aes(x=x,y=y,label =text)) + 
    theme_few() + 
    theme(strip.text = element_text(face="bold")) 
  #dev.off()
}else if (sample == "full"){
  pdf("fig1_full.pdf",width = 10, height = 12)
  ggplot(data = temp.df, 
         aes(x=renco,y=value,label = port.name))  +
    facet_wrap(~variable,scales="free") +
    geom_text(colour = "red") + xlim(2,5.5) + ylim(2,5.5) + 
    geom_abline(intercept = 0, slope = 1) + 
    xlab("Realzied Return (in %)") +
    ylab("Fitted Return (in %)") +
    geom_text(data = s.g.text, aes(x = x_s + 0.2, y = y_s + 0.15,label = port.name) ) +
    geom_segment(data = s.g.text, 
                 aes(x=x_s,xend = renco - 0.05,y=y_s - 0.05, yend = value + 0.15),
                 arrow = arrow(length = unit(0.15, "cm"),type = "closed")) +
    geom_text(data = s.v.text, aes(x = x_s - 0.2, y = y_s +-0.15,label = port.name) ) +
    geom_segment(data = s.v.text, 
                 aes(x=x_s,xend = renco + 0.05,y=y_s, yend = value - 0.15),
                 arrow = arrow(length = unit(0.15, "cm"),type = "closed")) +
    geom_text(data = rq.text, aes(x=x,y=y,label =text)) + 
    theme_few() + 
    theme(strip.text = element_text(face="bold")) 
  dev.off()
}

# Bootstrapping ---------------------------------------------------------------------

require(boot) #bootstrapping

fm.boot <- function(ts.df, port.num = 25){
  #ports.df is the data.frame for the portfolio's return
  #factors.df is the data.frame for the factors used 
  
  port.df <- ts.df[,1:port.num]
  factor.df <- ts.df[,-(1:port.num)]
  ##time series regression for beta
  beta.fit <-list()
  port.num <- dim(port.df)[2]
  time.span <- dim(factor.df)[1]
  factor.num <- dim(factor.df)[2]
  beta.mat <- matrix(rep(0,port.num * factor.num), ncol = port.num)
  for(i in 1:25){
    y <- port.df[[i]]
    x <- data.matrix(factor.df)
    temp.fit <- lm(y~x )
    beta.fit[[i]] <- temp.fit
    beta.mat[,i] <- temp.fit$coefficients[2:(factor.num+1)]
  }
  ##cross-sectional regression for each period
  premium.fit <- list()
  port.matrix <- data.matrix(port.df)[1:time.span,]
  fit.ret.mat <- matrix(rep(0, port.num * time.span),ncol=port.num)
  premium.mat <- matrix(rep(0,time.span * (1 + factor.num)),ncol = (factor.num+1))
  for(j in 1:time.span){
    y <- port.matrix[j,]
    x <- t(beta.mat)
    cross.fit <- lm(y~x)
    premium.fit[[j]] <- cross.fit
    premium.mat[j,] <- cross.fit$coefficients
    fit.ret.mat[j,] <- cross.fit$fitted.values
  }
  #premium.vec stores the estimates for \labmda's
  premium.vec <- colMeans(premium.mat)
  fitted.ret <- colMeans(fit.ret.mat)

  #cross-sectional residuals 
  residual.mat <- matrix(rep(0,time.span * port.num), 
                         ncol = port.num)
  for(j in 1:time.span){
    residual.mat[j,] <- premium.fit[[j]]$residuals
  }
  residual.vec <- colMeans(residual.mat)

  
  
  ##alpha joint test for pricing errors
  s.p.vec <- premium.vec[-1] #take out intercept
  S = as.vector(1 + s.p.vec %*% solve(cov(factor.df)) %*% s.p.vec)
  cov.alpha <- 1/time.span * cov(residual.mat)
  alpha <- as.matrix(residual.vec, ncol = 1)
  alpha.test <- 1/S * t(alpha) %*% 
    solve(Matrix(cov.alpha),alpha)
  alpha.test <- as.numeric(alpha.test)

  
  return(alpha.test)
}


detach(package:quantmod)
port.df <- ff_ports[,2:26]
factor.df <- ff_factors[,2:4]
ts.df <- cbind(port.df,factor.df)
result.ff <- tsboot(ts.df, statistic = fm.boot, R = 100, 
                 l = 8, sim = 'fixed', port.num = 25)
#h <- hist(result$t,nclass = 50)
#pdf("bootstrap.pdf",width = 6, height=4)
xfit<-seq(0,max(result.ff$t),length=1000) 
yfit<-dchisq(xfit,df = 22) 
plot(density(result.ff$t),ylim=c(0,max(yfit)),
     main = "",xlab="",col="red",lwd=2,
     xlim=c(0,100))
lines(xfit, yfit, col="blue", lwd=2)
legend("topright", inset=.05, 
       legend= c("Bootstrapped","Theoretical"),
       col=c("red","blue"), lty=c(1,1),horiz=FALSE,
       cex = 1)
#dev.off()
# k <- 25 - 3
# yfit <- yfit*diff(h$mids[1:2])*length(x) 
# lines(xfit, yfit, col="blue", lwd=2)



# Reporting ---------------------------------------------------------------
require(xtable)
rep.mat <- data.frame()
q <- length(report)
#row number
row.num <- as.character(seq(1:q))
row.col <- character()
for(i in 1:length(report)){
  row.col <- c(row.col,row.num[i],"" , "")
}

const.col <- character()
for(i in 1:q){
  const.col <- c(const.col, 
                 as.character(round(report[[i]][[1]][1], digits = 2)),
                 paste("(",as.character(round(report[[i]][[3]][1], digits = 2)),")",sep=""),
                 paste("(",as.character(round(report[[i]][[4]][1], digits = 2)), ")",sep=""))
}


cay.col <- character()
for(i in 1:q){
  if(i < 4){
    cay.col <- c(cay.col,"","","")
  } else{
    cay.col <- c(cay.col,
                 as.character(round(report[[i]][[1]]["cay"], digits = 2)),
                 paste("(",as.character(round(report[[i]][[3]]["cay"], digits = 2)),")",sep=""),
                 paste("(",as.character(round(report[[i]][[4]]["cay"], digits = 2)),")",sep=""))
  }
}

Rvw.col <- character()
for(i in 1:q){
  if(i == 2 | i == 5){
    Rvw.col <- c(Rvw.col,"","","")
  } else{
    Rvw.col <- c(Rvw.col,
                 as.character(round(report[[i]][[1]]["Rvw"], digits = 2)),
                 paste("(",as.character(round(report[[i]][[3]]["Rvw"], digits = 2)),")",sep=""),
                 paste("(",as.character(round(report[[i]][[4]]["Rvw"], digits = 2)),")",sep=""))
  }
}


dy.col <- character()
for(i in 1:q){
  if(i == 4){
    dy.col <- c(dy.col,
                 as.character(round(report[[i]][[1]]["d_y"], digits = 3)),
                 paste("(",as.character(round(report[[i]][[3]]["d_y"], digits = 2)),")",sep=""),
                 paste("(",as.character(round(report[[i]][[4]]["d_y"], digits = 2)),")",sep=""))
  } else{
    dy.col <- c(dy.col,"","","")
  }
} 


dc.col <- character()
for(i in 1:q){
  if(i == 2 | i == 5){
    dc.col <- c(dc.col,
                as.character(round(report[[i]][[1]]["d_c"], digits = 2)),
                paste("(",as.character(round(report[[i]][[3]]["d_c"], digits = 2)),")",sep=""),
                paste("(",as.character(round(report[[i]][[4]]["d_c"], digits = 2)),")",sep=""))
  } else{
    dc.col <- c(dc.col,"","","")
  }
} 

SMB.col <- character()
for(i in 1:q){
  if(i == 3){
    SMB.col <- c(SMB.col,
                as.character(round(report[[i]][[1]]["SMB"], digits = 2)),
                paste("(",as.character(round(report[[i]][[3]]["SMB"], digits = 2)),")",sep=""),
                paste("(",as.character(round(report[[i]][[4]]["SMB"], digits = 2)),")",sep=""))
  } else{
    SMB.col <- c(SMB.col,"","","")
  }
} 

HML.col <- character()
for(i in 1:q){
  if(i == 3){
    HML.col <- c(HML.col,
                 as.character(round(report[[i]][[1]]["HML"], digits = 2)),
                 paste("(",as.character(round(report[[i]][[3]]["HML"], digits = 2)),")",sep=""),
                 paste("(",as.character(round(report[[i]][[4]]["HML"], digits = 2)),")",sep=""))
  } else{
    HML.col <- c(HML.col,"","","")
  }
}

HML.col <- character()
for(i in 1:q){
  if(i == 3){
    HML.col <- c(HML.col,
                 as.character(round(report[[i]][[1]]["HML"], digits = 2)),
                 paste("(",as.character(round(report[[i]][[3]]["HML"], digits = 2)),")",sep=""),
                 paste("(",as.character(round(report[[i]][[4]]["HML"], digits = 2)),")",sep=""))
  } else{
    HML.col <- c(HML.col,"","","")
  }
}

caydc.col <- character()
for(i in 1:q){
  if(i == 5){
    caydc.col <- c(caydc.col,
                 as.character(round(report[[i]][[1]]["cayd_c"], digits = 2)),
                 paste("(",as.character(round(report[[i]][[3]]["cayd_c"], digits = 2)),")",sep=""),
                 paste("(",as.character(round(report[[i]][[4]]["cayd_c"], digits = 2)),")",sep=""))
  } else{
    caydc.col <- c(caydc.col,"","","")
  }
}

caydy.col <- character()
for(i in 1:q){
  if(i == 4){
    caydy.col <- c(caydy.col,
                   as.character(round(report[[i]][[1]]["cay.d_y"], digits = 5)),
                   paste("(",as.character(round(report[[i]][[3]]["cay.d_y"], digits = 2)),")",sep=""),
                   paste("(",as.character(round(report[[i]][[4]]["cay.d_y"], digits = 2)),")",sep=""))
  } else{
    caydy.col <- c(caydy.col,"","","")
  }
}

cayRvw.col <- character()
for(i in 1:q){
  if(i == 4){
    cayRvw.col <- c(cayRvw.col,
                   as.character(round(report[[i]][[1]]["cayRvw"], digits = 2)),
                   paste("(",as.character(round(report[[i]][[3]]["cayRvw"], digits = 2)),")",sep=""),
                   paste("(",as.character(round(report[[i]][[4]]["cayRvw"], digits = 2)),")",sep=""))
  } else{
    cayRvw.col <- c(cayRvw.col,"","","")
  }
}

Rsq.col <- character()
for(i in 1:q){
  Rsq.col <- c(Rsq.col, 
               as.character(round(report[[i]][[2]], digits = 2)),
               as.character(round(report[[i]][[6]], digits = 2)),
               "")
}

chisq.col <- character()
for(i in 1:q){
  chisq.col <- c(chisq.col, 
               as.character(round(report[[i]][[7]], digits = 3)),
               paste("(",as.character(round(report[[i]][[8]], digits = 3)),")",sep=""),
               "")
}

alpha.col <- character()
for(i in 1:q){
  alpha.col <- c(alpha.col, 
                 as.character(round(report[[i]][[9]][2], digits = 3)),
                 paste("(",as.character(round(report[[i]][[9]][1], digits = 3)),")",sep=""),
                 "")
}

rep.mat <- data.frame(row.col, const.col, cay.col, Rvw.col,
                      dy.col, dc.col, SMB.col, HML.col, cayRvw.col, 
                      caydy.col, caydc.col, Rsq.col, chisq.col,
                      alpha.col)


print(xtable(rep.mat), 
      include.rownames = FALSE,
      include.colnames = FALSE)