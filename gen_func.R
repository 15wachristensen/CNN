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
# model type is a string either
#   1. vgg16  2.vgg19   3.xception  4. resnet50   5. resnet_v2
# and determines the type of model to be made
# NOTE: different models accept different types of resolutions
#       I must make some errors 
#
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

BuildModel <- function( modeltype, imres, dlayers, uns, cominfo,
                        trainsteps, epochs, validsteps )
{
  
  # in-memory model 
  matchModel <- function( m,imr ){
    if( m == 'vgg16' )
    {
      print('trun')
      mod <- application_vgg16(weights = 'imagenet', include_top = FALSE,input_shape=imr)
    }
    else if( m == 'vgg19' )
    {
      mod <- application_vgg19(weights = 'imagenet', include_top = FALSE,input_shape=imr)
    }
    else if( m == 'xception')
    {
      mod <- application_xception(weights = 'imagenet', include_top = FALSE,input_shape=imr)
    }
    else if( m == 'resnet50' )
    {
      mod <- application_resnet50(weights = 'imagenet', include_top = FALSE,input_shape=imr)
    }
    else if( m == 'resnetv2' )
    {
      mod <- application_inception_resnet_v2(weights = 'imagenet', include_top = FALSE,input_shape=imr)
    }
    return(mod)
  }
  model = matchModel( modeltype,imres )
  summary(model)
  # stop all weights from being trained
  model = freeze_weights(model)
  
  # Use this to see if a layer is a max pool or conv 
  # using typeof()
  #lay = get_layer(model,index = 2)
  # conv = layer_conv_2d()
  # pool = layer_max_pooling_2d()
  # if(typeof(conv) == typeof(lay)){
  #   print(paste('layer is conv'))
  # }
  
  # This for loop creates the 
  # ending dense layers for model
  prev = layer_flatten( get_output_at(get_layer(model,index=16),node_index=1))
  print(prev)
  for(i in 1:dlayers)
  {
    if( i != length(dlayers) )
      {
      d1 = layer_dense(units=uns[i],activation = 'relu')(prev)
    }
    else
    {
      d1 = layer_dense(units=uns[i],activation = 'sigmoid')(prev)
    }
    
    prev = d1
  }
  
  k = keras_model(inputs = get_input_at(model,node_index=1),outputs = prev)
  
  matchOptimizer <- function( c )
  {
    if( c[[2]] == 'adam' )
    {
      opt = optimizer_adam( c[[3]] )
    }
    else if( c[[2]] == 'rms' )
    {
      opt = optimizer_rmsprop( c[[3]] )
    }
    return(opt)
  }
  optimizer=matchOptimizer(cominfo)
  
  k %>% compile(
    loss = cominfo[[1]],
    optimize=optimizer,
    metrics=c(cominfo[[4]])
  )
  
  # return compiled model to be tested on test data
  return(k)
}