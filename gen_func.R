#By: Walker Christensen

# I will only use vgg model and 
# change parameters within this model only
# 
# Some things I'm focusing on are;
#   regularizaiton
#   dropout
#   kernel size
#   unit/ filter sizesS
#   different optimizers(compile function)
#   dense layers at end of transfer learning
#
# This funciton uses generators made
#   by keras make CNN and
#   returns the model to train and test


# Parameters
#
# imres is input shape parameter in 
#   application function
#
# dlayers is the number of dense layers to 
#   add on the end of vgg model (max=5) 
#
# uns is vector of integers that represent the amount of 
#   units to add onto dense layers at end
#
# cominfo is a list of compile information
#   first entry is loss function
#   next entry is type of optimizer
#   next entry is learning rate
#   next entry is metric 

library(keras)
library(dplyr)

BuildModel <- function( imres, dlayers, uns, cominfo,
                        trainsteps, epochs, validsteps )
{
  
  
  # This was my attempt to distinguish if i could use a repeating 
  #   pattern to make differnet models, but these use a lot of other layers
  
  # # model <- application_xception(weights = 'imagenet', include_top = FALSE,input_shape=c(71,71,3))
  # # print('xception')
  # #summary(model)
  # # model <- application_resnet50(weights = 'imagenet', include_top = FALSE,input_shape=c(197,197,3))
  # # print('resnet')
  # # summary(model)
  # # model <- application_inception_resnet_v2(weights = 'imagenet', include_top = FALSE,input_shape=c(139,139,3))
  # # print('iresnet')
  # # summary(model)
  
  
  
  # in-memory model 
  model <- application_vgg16(weights = 'imagenet', include_top = FALSE,input_shape=imres)
  model = freeze_weights(model)
  summary(model)
  # This for loop creates the 
  # ending dense layers for model
  prev = layer_flatten( get_output_at(get_layer(model,index=16),node_index=1))
  print(prev)
  for(i in 1:dlayers)
  {
    if(i != length(dlayers)){
     d1 = layer_dense(units=uns[i],activation = 'relu')(prev)
    }else{
      d1 = layer_dense(units=uns[i],activation = 'sigmoid')(prev)
    }
     prev = d1
  }
  
  k = keras_model(inputs = get_input_at(model,node_index=1),outputs = prev)
  
  if( cominfo[[2]] == 'adam' )
  {
    optimizer = optimizer_adam( cominfo[[3]] )
  }
  else if( cominfo[[2]] == 'rms' )
  {
    optimizer = optimizer_rmsprop( cominfo[[3]] )
  }
  
  k %>% compile(
    loss = cominfo[[1]],
    optimize=optimizer,
    metrics=c(cominfo[[4]])
  )
  summary(k)
  # return model to be tested on futhermore
  return(k)
}