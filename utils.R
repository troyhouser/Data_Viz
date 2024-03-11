sigmoid = function(x){
  1/(1+exp(-x))
}
sigmoid_derivative = function(x){
  x*(1-x)
}
normalize <- function(x, na.rm = TRUE) {
  return((x- min(x)) /(max(x)-min(x)))
}
undo_normalization = function(original,normalized){
  original_range = range(original)
  normalized * (original_range[2] - original_range[1]) + original_range[1]
}
RNN_model = function(train_x,train_y,test_x,test_y,hidden_units,learning_rate = 0.01){
  input_layer_units = 1
  output_units = 1
  
  # initialization
  weights = matrix(rnorm(input_layer_units*hidden_units,mean=0,sd=1),input_layer_units,hidden_units)
  input_bias = runif(hidden_units)
  input_bias_temp = rep(input_bias,1)
  b_input = matrix(input_bias_temp,1,byrow=F)
  output_weights = matrix(rnorm(hidden_units*output_units,mean=0,sd=1),hidden_units,output_units)
  output_bias = runif(output_units)
  output_bias_temp = rep(output_bias,1)
  b_out = matrix(output_bias_temp,1,byrow=F)
  
  # train model
  epochs = length(train_x)
  for(i in 1:epochs){
    
    #forward pass
    hidden_layer_input = train_x[i] %*% weights + b_input
    hidden_layer_activations = sigmoid(hidden_layer_input)
    output_layer_input = hidden_layer_activations %*% output_weights + b_out
    output = sigmoid(output_layer_input)
    
    #backpropagation
    error = train_y[i] - output
    output_slope = sigmoid_derivative(output)
    hidden_layer_slope = sigmoid_derivative(hidden_layer_activations)
    d_output = error * output_slope
    error_at_hidden_layer = d_output %*% t(output_weights)
    d_hidden_layer = error_at_hidden_layer * hidden_layer_slope
    output_weights = output_weights + (t(hidden_layer_activations) %*% d_output) * learning_rate
    b_out = b_out + rowSums(d_output) * learning_rate
    weights = weights + (t(train_x[i]) %*% d_hidden_layer) * learning_rate
    b_input = b_input + rowSums(d_hidden_layer) * learning_rate
  }
  
  # prediction on test data
  predictions = c()
  for(i in 1:length(test_x)){
    hidden_layer_input = test_x[i] %*% weights + b_input
    hidden_layer_activations = sigmoid(hidden_layer_input)
    output_layer_input = hidden_layer_activations %*% output_weights + b_out
    predictions[i] = sigmoid(output_layer_input)
  }
  predictions
}

gaussian_process_model = function(train_x,train_y,test_x,test_y,noise = sqrt(.Machine$double.eps),n_samples = 100){
  distances_between_inputs = plgp::distance(train_x)
  sigma_train = exp(-distances_between_inputs) + diag(noise,ncol(distances_between_inputs))
  distances_between_test = plgp::distance(test_x)
  sigma_test = exp(-distances_between_test) + diag(noise,ncol(distances_between_test))
  distances_test_train = plgp::distance(test_x,train_x)
  sigma_test_train = exp(-distances_test_train)
  precision_matrix = MASS::ginv(sigma_train)
  mean_vector = sigma_test_train %*% precision_matrix %*% train_y
  covariance_matrix = sigma_test - sigma_test_train %*% precision_matrix %*% t(sigma_test_train)
  covariance_matrix = (covariance_matrix + t(covariance_matrix)) / 2
  yy = SimDesign::rmvnorm(n_samples,mean_vector,covariance_matrix)
  predictions = colMeans(yy)
  q1 = mean_vector + qnorm(0.05, 0, sqrt(diag(covariance_matrix)))
  q2 = mean_vector + qnorm(0.95, 0, sqrt(diag(covariance_matrix)))
  list(predictions,q1,q2)
}

particle_filter = function(train_x,train_y,test_x,test_y,sigma_x,sigma_y,n_particles){
  test_y = rep(NA,length(test_y))
  observations = c(train_y,test_y)
  
  # initialization
  X = weights = matrix(rep(0,n_particles*length(observations)),n_particles,length(observations))
  X[,1] = rnorm(n_particles,0,sigma_x)
  weights[,1] = dnorm(observations[1],X[,1],sigma_y)
  weights[,1] = weights[,1] / sum(weights[,1])
  X[,1] = sample(X[,1],replace=T,prob = weights[,1])
  
  for(i in 2:length(observations)){
    X[,i] = rnorm(n_particles,X[,i-1],sigma_x)
    if(!is.na(observations[i])){
      weights[,i] = dnorm(observations[i],X[,i],sigma_y)
      weights[,i] = weights[,i] / sum(weights[,i])
    }else{
      weights[,i] = 1/n_particles
    }
    X[,i] = sample(X[,i],replace=T,prob = weights[,i])
  }
  predictions = mean(X[,(length(train_y)+1):(length(observations))])
  predictions
}

lag_transform = function(x,k=3){
  lagged = c(rep(NA,k),x[1:(length(x) - k)])
  df = as.data.frame(cbind(lagged,x))
  colnames(df) = c(paste0("x-",k),"x")
  df[is.na(df)] = 0
  df
}


plot_gp = function(train_x,train_y,test_x,test_y,gp_predictions,gp_lower,gp_upper,lin_predictions,stock,stock_type){
  df = data.frame(X = c(train_x,test_x),Y = c(train_y,test_y),
                  phase = c(rep("train",1000),rep("test",259)),
                  predictions_gp = c(rep(NA,1000),na.omit(gp_predictions),NA),
                  predictions_lin = c(rep(NA,1000),na.omit(lin_predictions),NA),
                  lower_bound_gp = c(rep(NA,1000),na.omit(gp_lower),NA),
                  upper_bound_gp = c(rep(NA,1000),na.omit(gp_upper),NA),
                  stock = stock, stock_type = stock_type)
  global_size = 20
  ggplot(df,aes(x = X, y = Y, col=phase))+
    geom_line(size=2)+
    geom_line(aes(y = predictions_gp),size=2,col="orange")+
    geom_line(aes(y = predictions_lin),size=2,col="#9C24DC")+
    geom_ribbon(aes(ymin=lower_bound_gp,ymax=upper_bound_gp),alpha=0.2,fill="orange",col="orange")+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black"),
          legend.position = "None",text=element_text(size=global_size),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          axis.line.y = element_blank())+
    labs(x = "time (2012 - 2018)",y = "stock market value (US Dollars)",
         title = paste0("stock: ",stock," (",stock_type,")"))+
    scale_color_manual(values = c("train" = "black","test" = "blue"))+
    geom_point(x=130,y=1.3,col="black",size=4)+
    annotate("text",x=275,y=1.3,col="black",label="training data")+
    geom_point(x=130,y=1.2,col="blue",size=4)+
    annotate("text",x=263,y=1.2,col="blue",label="testing data")+
    geom_point(x=130,y=1.1,col="orange",size=4)+
    annotate("text",x=348,y=1.1,col="orange",label="GP model predictions")+
    geom_point(x=130,y=1.0,col="#9C24DC",size=4)+
    annotate("text",x=383,y=1.0,col="#9C24DC",label="Linear model predictions")+
    geom_segment(aes(x=1001,xend=810,y=lower_bound_gp[1001],yend=1.15),linetype="dotted",col="gray")+
    geom_segment(aes(x=1001,xend=820,y=upper_bound_gp[1001],yend=1.1),linetype="dotted",col="gray")+
    annotate("text",x=815,y=1.1,col="gray",label="GP model prediction \nuncertainty")+
    annotate("segment",x=-85,xend=-90,y=0.8,yend=1.5,arrow = arrow())+
    annotate("segment",x=-85,xend=-90,y=0.7,yend=0,arrow = arrow())+
    annotate("text",x=-115,y=0.75,label="less          more",angle=90,size=8)
}


plot_choices = function(data){
  ggplot(data, aes(x = x1+1, y = x2+1, fill = y)) + 
    geom_tile(color='black', width=1, height=1) +
    theme_bw() +
    coord_equal() +
    xlim(0.5,11.5) +
    ylim(0.5,11.5) + 
    geom_text(aes(x = x1+1, y = x2+1, label = ytext))+
    #ggtitle(paste0('Revealed (t=',trial,")")) +
    #scale_fill_gradientn(name='Payoff', colours = hm.palette(100), values = seq(0, 100, length=9),  rescaler = function(x, ...) x, oob = identity) +
    scale_fill_distiller(palette = "RdYlGn",limits=c(-.05*scalingFactor,1.05*scalingFactor), na.value = 'white', breaks=c(0,.25*scalingFactor,.50*scalingFactor,.75*scalingFactor,1*scalingFactor))+
    labs(fill="Payoff")+
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank(),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          panel.border=element_blank(),
          plot.margin = unit(c(0, 0, 0, 0), "cm"),
          legend.position = 'none')
} 

plot_posterior = function(data){
  ggplot(data, aes(x = X1, y = X2, fill = value)) + 
    geom_tile(color='black', width=1, height=1) +
    theme_bw() +
    xlim(0.5,11.5) +
    ylim(0.5,11.5) + 
    coord_equal() +
    #scale_fill_gradientn(name = "Exp. Payoff", colours = hm.palette(100),values = seq(0, 100, length=9)) +
    scale_fill_distiller(palette = "RdYlGn",limits=c(-.05*scalingFactor,1.05*scalingFactor), na.value = 'white', breaks=c(0,25,50,75,100))+
    labs(fill="Payoff")+
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank(),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          panel.border=element_blank(),
          plot.margin = unit(c(0, 0, 0, 0), "cm"))
}
scalingFactor <- 85
plot_probs = function(data){
  ggplot(data, aes(x = X1, y = X2, fill = value)) + 
    geom_tile(color='black', width=1, height=1) +
    theme_bw() +
    xlim(0.5,11.5) +
    ylim(0.5,11.5) + 
    coord_equal() +
    #ggtitle('Softmax') +
    #scale_fill_gradientn(name='P(choice)',colours = hm.palette(100),values = seq(0, 1, length=9)) +
    scale_fill_distiller(palette = "RdYlGn", na.value = 'white' )+
    labs(fill="P(Choice)")+
    #annotate("text", x = nextChoice[1] + 1, y = nextChoice[2] + 1, label = "X") +
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank(),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          panel.border=element_blank(),
          plot.margin = unit(c(0, 0, 0, 0), "cm"))
}

getlegend = function(plt){
  plot_table = ggplot_gtable(ggplot_build(plt))
  legend_plot = which(sapply(plot_table$grobs,function(x) x$name) == "guide-box")
  plot_table$grobs[[legend_plot]]
}
keep = c("BSX","PFE","JNJ","DE","DAL","EFX","ACN","NVDA","MSFT",
         "NDAQ","MS","PNC","WMB","HAL","XOM","MCD","NKE","SBUX","WMT","PEP","SYY",
         "VZ","NFLX","DIS","PNW","XEL","PEG","PSA","PLD","REG","PPG","VMC","IP")
type = c("health care","industrials","information tech","financials","energy",
         "consumer discretionary","consumer staples","communication services",
         "utilities","real estate","materials")
type = rep(type,each=3)

it_stocks = c("NVDA","MSFT","ACN","ADBE","AMD","AKAM","APH","ANSS","ADI",
              "AMAT","ADSK","AVGO","CDNS","CDW","CTSH","GLW")
re_stocks = c("ARE","AMT","AVB","PSA","CCI","DLR","EQIX","ESS",
              "EXR","FRT","HST","KIM","MAA","REG","SBAC")

all500 = read.csv("all_stocks_5yr.csv")
all500 = all500[all500$Name %in% c(it_stocks,re_stocks),]
all500$type = NA
all500$type[which(all500$Name %in% it_stocks)] = "information tech"
all500$type[which(all500$Name %in% re_stocks)] = "real estate"
each_stock = unique(all500$Name)