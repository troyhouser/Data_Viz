bitcoin = read.csv("~/Dropbox (University of Oregon)/stocks/bitcoin.csv")
bitcoin$time = nrow(bitcoin):1
bitcoin$Date = as.numeric(gsub(".*/","",bitcoin$Date))
ggplot(bitcoin,aes(x = time,y = Close.Last,col = factor(Date)))+
  geom_line(aes(x=time,y=Close.Last),col="gray",size=3)+
  geom_smooth(size=3)+
  geom_line(aes(x=time,y=c(final_results$fitted+final_results$residuals,final_results$mean)))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        text = element_text(size=20),axis.ticks.x = element_blank(),
        legend.position = "None",axis.text.x = element_blank())+
  labs(y="Market Value \n(US Dollars)",x="time (2019-2024)",title="Bitcoin")+
  scale_color_viridis_d()

train = bitcoin[bitcoin$Date<2024,]
test = bitcoin[bitcoin$Date==2024,]
require(kernlab)
require(e1071)

linear = lm(Close.Last~time,train)
linear_wYear = lm(Close.Last~time*Date,train)
support_vec = svm(Close.Last~time,train)
support_vec_wYear = svm(Close.Last~time*Date,train)
gp = gausspr(Close.Last~time,train)
gp_wYear = gausspr(Close.Last~time*Date,train)

sqrt(mean((predict(linear,test)-test$Close.Last)^2))
sqrt(mean((predict(support_vec,test)-test$Close.Last)^2))
sqrt(mean((predict(gp,test)-test$Close.Last)^2))
sqrt(mean((predict(linear_wYear,test)-test$Close.Last)^2))
sqrt(mean((predict(support_vec_wYear,test)-test$Close.Last)^2))
sqrt(mean((predict(gp_wYear,test)-test$Close.Last)^2))

gp_preds = predict(gp,test)
bitcoin$gp_preds = rev(c(rep(NA,nrow(bitcoin) - length(gp_preds)),gp_preds))
bitcoin$gp_preds = rev(c(rep(NA,nrow(bitcoin)-64),))
get_scaling_factors <- function(data){
  out <- c(mean = mean(data), sd = sd(data))
  return(out)
}

normalize_data <- function(data, scaling_factors, reverse = FALSE) {
  
  if (reverse) temp <- (data * scaling_factors[2]) + scaling_factors[1]
  else temp <- (data - scaling_factors[1]) / scaling_factors[2]
  
  out <- temp %>% as.matrix()
  return(out)
}

pacman::p_load(quantmod, keras, tidyverse, timetk, lubridate, Metrics)

kerasize_data <- function(data, x = TRUE, lag = 12, pred = 12) {
  
  if (x) {
    
    temp <- sapply(
      1:(length(data) - lag - pred + 1)
      ,function(x) data[x:(x + lag - 1), 1]
    ) %>% t()
    
    out <- array(
      temp %>% unlist() %>% as.numeric()
      ,dim = c(nrow(temp), lag, 1)
    )
    
  }  else {
    
    temp <- sapply(
      (1 + lag):(length(data) - pred + 1)
      ,function(x) data[x:(x + lag - 1), 1]
    ) %>% t()
    
    out <- array(
      temp %>% unlist() %>% as.numeric()
      ,dim = c(nrow(temp), pred, 1)
    )
    
  }
  
  return(out)
  
}
kerasize_pred_input <- function(data, lag = 12, pred = 12){
  temp <- data[(length(data) - pred + 1):length(data)]
  temp <- normalize_data(temp, get_scaling_factors(data))
  out <- array(temp, c(1, lag, 1))
  return(out)
}

lstm_build_model <- function(x, y, units = 50, batch = 1, epochs = 20, rate = 0.5, seed = 2137){
  
  lag = dim(x)[2]
  
  lstm_model <- keras_model_sequential()
  
  lstm_model %>%
    layer_lstm(units = units
               ,batch_input_shape = c(batch, lag, 1)
               ,return_sequences = TRUE
               ,stateful = TRUE) %>%
    layer_dropout(rate = rate) %>%
    layer_lstm(units = units
               ,return_sequences = TRUE
               ,stateful = TRUE) %>%
    layer_dropout(rate = rate) %>%
    time_distributed(layer_dense(units = 1))
  
  lstm_model %>%
    compile(loss = 'mae'
            ,optimizer = 'adam'
            ,metrics = 'accuracy')
  
  tensorflow::set_random_seed(seed)
  lstm_model %>% fit(
    x = x
    ,y = y
    ,batch_size = batch
    ,epochs = epochs
    ,verbose = 0
    ,shuffle = FALSE)
  
  out <- list(
    model = lstm_model
    ,x = x
    ,batch = batch
    ,lag = lag
    ,pred = dim(y)[2]
  )
  return(out)
  
}

lstm_forecast <- function(x_test, model, scaling_factors){
  
  batch <- model$batch
  
  temp <- model$model %>%
    predict(x_test, batch_size = batch) %>% 
    .[, , 1] %>%
    normalize_data(scaling_factors = scaling_factors, reverse = TRUE)
  
  out <- list(
    forecast = temp
    ,scaling_factors = scaling_factors
  )
  
  return(out)
  
}

forecast_transform <- function(data, model, forecast){
  
  lag <- model$lag
  
  freq <- periodicity(data)$scale
  frequency <- case_when(
    freq == 'weekly' ~ 52
    ,freq == 'monthly' ~ 12
    , freq == 'quarterly' ~ 4
    ,freq == 'yearly' ~ 1
  )
  
  fitted <- predict(model$model, model$x, batch_size = model$batch) %>% .[, , 1]
  
  if (dim(fitted)[2] > 1) {
    fitted <- c(fitted[, 1], fitted[dim(fitted)[1], 2:dim(fitted)[2]])
  } else {
    fitted <- fitted[, 1]
  }
  
  fitted <- normalize_data(fitted, forecast$scaling_factors, reverse = TRUE)
  
  lstm_forecast <- ts(forecast$forecast, deltat = 1/frequency)
  
  data_trimmed <- data[(model$lag+1):length(data)] %>% ts(deltat = 1/frequency)
  
  
  out <- list(
    model = NULL
    ,method = 'LSTM'
    ,mean = lstm_forecast
    ,x = data_trimmed
    ,fitted = fitted
    ,residuals = as.numeric(data_trimmed) - as.numeric(fitted)
  )
  
  class(out) <- 'forecast'
  
  return(out)
}

scaling_factors <- get_scaling_factors(bitcoin$Close.Last)
data_normalized <- normalize_data(bitcoin$Close.Last, scaling_factors)

x_data <- kerasize_data(data_normalized,  x = TRUE)
y_data <- kerasize_data(data_normalized, x = FALSE)
x_test <- kerasize_pred_input(data_normalized)

model <- lstm_build_model(x_data, y_data)
prediction <- lstm_forecast(x_test, model, scaling_factors)

final_results <- forecast_transform(ts(bitcoin$Close.Last), model, prediction)

save(final_results,"lstm_bitcoin_preds.RData")
