# health care:
## Boston Scientific(BSX),Pfizer (PFE),Johnson&Johnson(JNJ)

# industrials:
##John Deere (DE),Delta Air lines (DAL),Equifax (EFX)

# information tech:
## Accenture (ACN),nvidia (NVDA),microsoft (MSFT)

# financials:
## nasdaq (NDAQ), morgan stanley (MS), PNC (PNC)

# energy:
## williams company (WMB),halliburton (HAL), Exxon (XOM)

# consumer discretionary:
## Mcdonalds (MCD), nike (NKE), starbucks (SBUX)

# consumer staples:
#walmart (WMT), Pepsi (PEP),sysco (SYY)

# communication services:
## verizon (VZ),netflix (NFLX), disney (DIS)

# utilities:
## pinnacle west (PNW), Xcel energy (XEL), public service enterprise (PEG)

# real estate:
## public storage (PSA), prologis (PLD), regency centers (REG)

# materials:
## PPG industries (PPG), vulcan materials (VMC), international paper (IP)
rm(list=ls())
library(ggplot2)
library(forecast)
library(keras)
library(tensorflow)
library(kernlab)
library(ggplotify)
source("~/Dropbox (University of Oregon)/dataViz/utils.R")

keep = c("BSX","PFE","JNJ","DE","DAL","EFX","ACN","NVDA","MSFT",
         "NDAQ","MS","PNC","WMB","HAL","XOM","MCD","NKE","SBUX","WMT","PEP","SYY",
         "VZ","NFLX","DIS","PNW","XEL","PEG","PSA","PLD","REG","PPG","VMC","IP")
type = c("health care","industrials","information tech","financials","energy",
         "consumer discretionary","consumer staples","communication services",
         "utilities","real estate","materials")
type = rep(type,each=3)

data = read.csv("~/Dropbox (University of Oregon)/stocks/all_stocks_5yr.csv")
data = data[data$Name %in% keep,]
length(unique(data$Name))

### assign stock type to dataframe
data$type = NA
for(i in 1:nrow(data)){
  idx = which(data$Name[i] == keep)
  data$type[i] = type[idx]
}

### market value for each stock
data$x = rep(1:1259,length(keep))
write.csv(data,"~/Downloads/Data_Viz/all_stock_data33.csv")
n_models=2
rmse = array(0,dim=c(length(keep),n_models,259))
gp_uncertainty = matrix(0,nrow=length(keep),ncol=259)
for(k in 1:length(keep)){
  dat = data[data$Name==keep[k],]
  dat$close = normalize(dat$close)
  lin_ps = gp_ps = gp_lb = gp_ub = c()
  for(r in 1001:1258){
    train = dat[1:r,]
    test = dat[r+1,]
    
    # linear regression model
    linear_model = lm(close~x,train)
    lin_ps[r] = predict(linear_model,test)
    rmse[k,1,r-1001] = (test$close - lin_ps[r])^2

    # gaussian process model
    gp = gausspr(close~x,train)
    gp_ps[r] = predict(gp,test)
    gpVar = gausspr(close~x,train,variance.model=T)
    var_pred = predict(gpVar,test,type="sdeviation")
    gp_lb[r] = gp_ps[r] - var_pred
    gp_ub[r] = gp_ps[r] + var_pred
    rmse[k,2,r-1001] = (test$close - gp_ps[r])^2
    gp_uncertainty[k,r-1001] = var_pred
  }
  plot_gp(train_x = train$x,train_y = train$close,test_x = test$x,test_y = test$close,
          gp_predictions = gp_ps,gp_lower = gp_lb,gp_upper = gp_ub,
          lin_predictions = lin_ps,stock = dat$Name[1],stock_type = dat$type[1])
  print("*******************************************************")
  print(paste0("done with stock ",k," out of 33"))
  print("*******************************************************")
}
df = c()
for(i in 1:33){
  for(j in 1:259){
    for(k in 1:2){
      if(k == 1) model = "Linear"
      if(k == 2) model = "GP"
      df = rbind(df,c(rmse[i,k,j],j,model,keep[i],type[i]))
    }
  }
}
df = as.data.frame(df)
colnames(df) = c("error","timepoint","model","stock","stock_type")
df$error=as.numeric(df$error)
df$timepoint=as.numeric(df$timepoint)
write.csv(df,"~/Dropbox (University of Oregon)/dataViz/error.csv",row.names = F)

gp_var = data.frame(uncertainty = c(gp_uncertainty[,1:257]),
                    timepoint = rep(1:257,each=33),
                    stock = rep(keep,257),
                    stock_type = rep(type,257))
write.csv(gp_var,"~/Dropbox (University of Oregon)/dataViz/GP_uncertainty.csv",row.names = F)
