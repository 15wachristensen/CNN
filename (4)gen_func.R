#By: Walker Christensen

# This function uses generators made
#   by keras to make CNN and
#   returns the model to train and test


# Parameters
#
#  CNN_transfer_models is a vector of strings either
#   1. vgg16  2.vgg19   3.xception 4.inception 5. resnet50   6. resnet_v2
#   and determines the type of model to be made
#
# image_resolution is input shape parameter for model
#
# additional_layers is the number of dense layers to 
#   add on the end of vgg model (max=5) 
#
# additional_layer_units is vector of integers that represent the amount of 
#   units to add onto dense layers at end 
#
# compile_info is a list of compile information
#   first entry is loss function
#   next entry is type of optimizer
#   next entry is learning rate
#   next entry is metric 

library(keras)
library(dplyr)

BuildModel <- function( CNN_transfer_models,
                        image_resolution,
                        additional_layers,
                        additional_layer_units,
                        compile_info,
                        trainsteps,
                        epochs,
                        validsteps
                      )
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
    }else if( m == 'inception' )
    {
      mod <- application_inception_v3(weights = 'imagenet', include_top = FALSE,input_shape=imr)
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
  model = matchModel( CNN_transfer_models,image_resolution )
  summary(model)
  
  # stop all weights from being trained
  model = freeze_weights(model)
  
  # This for loop creates the 
  # ending dense layers for model
  prev = layer_flatten( get_output_at( get_layer( model, index=16 ), node_index = 1 ))
  for(i in 1:additional_layers){
    if( i != length(additional_layers) )
      {
      d1 = layer_dense(units=additional_layer_units[i],activation = 'relu')(prev)
    }
    else
    {
      d1 = layer_dense(units=additional_layer_units[i],activation = 'sigmoid')(prev)
    }
    
    prev = d1
  }
  
  k = keras_model( inputs = get_input_at( model, node_index = 1 ), outputs = prev )
  
  matchOptimizer <- function( c ){
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
  optimizer=matchOptimizer(compile_info)
  
  k %>% compile(
    loss = compile_info[[1]],
    optimize=optimizer,
    metrics=c(compile_info[[4]])
  )
  
  # return compiled model to be tested on test data
  return(k)
}
