# Walker Christensen
#
# Specific function to sort the data given in DM project
#   2 different classes will be made (can be four)
#     1. Normal  2. Diabetes  
#     or
#     2. First Semester 3. Mild 4. Moderate
#
# A file will be made for every class to feed into 
#   a generator function (custom or keras model)
#
#PRE: 
#     patient_information is patient data from the projectS
#
#     main_project_directory is the file folder for the project where R funcitons
#     and certain directories are found (train data, test data, pics, etc.)

# Post: Sorts excel data and image data
#
#       Outputs results from model 

library(readr)
library(xlsx)
library(keras)
library(dplyr)
library(magick)

org <- function( 
                 CNN_transfer_models,
                 image_resolution, 
                 patient_information, 
                 main_project_directory, 
                 photo_data_directory, 
                 image_type, 
                 classes
                 ) 
  {

  # patient_data column specifiers for id and class
  ID_COLUMN = 1
  CLASS_COLUMN = 4
  
  # Creates a vector of file names that can be created for 
  #   different classes
  makeClassDirectories <- function( imdata ){
    sen=c() 
    
    # File that contains the mass storage of photos 
    sen[1] = imdata
    
    # Files for the different classes
    for(i in 1:length( classes )){
      sen[i+1]=paste(main_project_directory,paste('/',classes[i]),sep='')
    }
    
    return(sen)
  }
  # dirs = makeClassDirectories( photo_data_directory )
  
  # Write vectors in memory that contain the strings for 
  #   each id that will be used to pull pictures from mass 
  #   photo data file to their respective class folders 
  normids=c()
  dids=c()
  classIds=list( normids, dids )
  
  # NOTE: This is where you would need to start integrating survey ?'s 
  
  # This loop seperates the ids into different classes
  for( i in 2:length( patient_information[[ ID_COLUMN ]]-1 )){
    
    # This for loop creates the string and adds to correct vector
    # The names for classes must be EXACT
    
    clause=FALSE
    p=''
    # This is how I determine which class the patient data is
    if( grepl( 'Diabetes', patient_information[[ CLASS_COLUMN ]][ i ] ) )
    {
      clause=TRUE
    }
    p = paste(photo_data_directory,'/',patient_information[[ID_COLUMN]][i],image_type,sep='')
    if( clause == FALSE ){
      # Put into normal vector
      classIds[[ 1 ]][ length( classIds[[ 1 ]]) + 1 ] = p
    }else{
      # Put into diabets vector
      classIds[[ 2 ]][ length( classIds[[ 2 ]] ) + 1 ] = p
    }
    
  }
  
  # Chooses which ids from each class will go 
  #   into the train, validation, and test sets and then 
  #   copies them over to the correct files (80/20 rule) 
  numFiles = length( normids )+length( dids )
  sep_photos_into_class <- function( ids, classes ){
    random_number = 0
    cou = 1
    
    for( i in 1:length( ids ) ){
      ncnt=1
      
      for( j in 1:length( ids[[ i ]]) ){
        train_versus_test_cutoff = length( ids[[ i ]] ) * .8
        from=''
        to = ''
        if( ncnt < train_versus_test_cutoff ){
          random_number = sample( 1:1000, 1 )
          
          if( random_number < 675 )
          {
            to = paste( main_project_directory,'/data/train/',classes[i],sep='' )
          }else{
            to = paste( main_project_directory,'/data/validation/',classes[i],sep='')
          }
          
        }else{
          to = paste( main_project_directory,'/data/test/testfolder',sep='' )
        }
        from = paste( ids[[i]][j], sep='' )
        file.copy( from, to )
        ncnt = ncnt+1
      }
    }
    
  }
  #sep_photos_into_class(classIds,classes)
  
  # Make generators to feed model
  # Every iteration we randomly change some of the data to 
  #   prevent overfitting
  train_data_gen = image_data_generator(rescale = 1/255,
                                        rotation_range = 15, 
                                        width_shift_range = 0.15, 
                                        height_shift_range = 0.15,
                                        shear_range = 0.2,
                                        zoom_range=0.2,
                                        horizontal_flip = TRUE)
  
  valid_data_gen = image_data_generator(rescale = 1/255)
  
  test_data_gen = image_data_generator( rescale = 1/255 )
  
  # Parameters that go into RunModel function in 
  #   source('D:/Dmjorp/gen_func.R') that will 
  #   be adjusted to test model
  # varies the type of model used 
  for( i in 1:length( CNN_transfer_models ) ){
    epchs = 50
    
    # Vary number of epochs
    while( epchs <= 100  ){
      print( paste( 'epchs',epchs ) )
      train_batch_size=8
      
      # Vary number of batches    
      while( train_batch_size <= 11 ){
        print( paste( 'train_batch_size',train_batch_size ) )
        additional_input_dimensions=0
        
        #Vary image input shape
        while( additional_input_dimensions <= 20 ){
          print( paste( 'additional_input_dimensions',additional_input_dimensions ) )
          
          image_resolution[[i]][1] = image_resolution[[i]][1] + additional_input_dimensions
          image_resolution[[i]][2] = image_resolution[[i]][2] + additional_input_dimensions
          
          # Makes generators by train,valid,test in that order
          makeGenerators <- function( class_directories,
                                      batch_sizes, 
                                      class_mode,
                                      image_target_size ){
            
            tgen=flow_images_from_directory(
              class_directories[1],
              train_data_gen,
              target_size = image_target_size,
              batch_size=batch_sizes[1],
              class_mode=class_mode
            )
            
            vgen=flow_images_from_directory(
              class_directories[2],
              valid_data_gen,
              target_size = image_target_size,
              batch_size=batch_sizes[2],
              class_mode=class_mode
            )
            
            testgen=flow_images_from_directory(
              class_directories[3],
              test_data_gen,
              target_size = image_target_size,
              batch_size=batch_sizes[3],
              class_mode=class_mode,
              shuffle=FALSE
            )
            return( c( tgen, vgen, testgen) )
          }
          
          set_dirs = c( paste(main_project_directory,'/data/train',sep = ''), 
                        paste(main_project_directory,'/data/validation',sep=''),
                        paste(main_project_directory,'/data/test',sep='') 
          )
          
          generator_batch_sizes = c(train_batch_size,1,1)
          ims = vector(length=1)
          ims[1] = image_resolution[[i]][1]
          ims[2] = image_resolution[[i]][1]
          
          # vector of generators
          gens = makeGenerators( set_dirs, generator_batch_sizes, 'binary' , ims )
          
          
          im = image_resolution[[ i ]]
          num_dense_layers = 1
          uns = c(1)
          # compile parameters that can be altered
          cominfo = list(
            'binary_crossentropy',
            'adam',
            1e-4,
            'acc'
          )
          
          # Create directories for train/test plots and results 
          #   dir.create(paste( main_project_directory,'/plots',sep=''))
          #   dir.create(paste(main_project_directory,'/plots/train',sep=''))
          #   dir.create(paste(main_project_directory,'/plots/test',sep=''))
          
          # Call function to make model 
          # If you want to know what the parameters represent check source code
          source('D:/Dmjorp/gen_func.R')
          k = BuildModel(
            
            CNN_transfer_models[i], 
            im,
            num_dense_layers,
            uns, 
            cominfo,
            trainsteps,
            epochs, 
            validsteps 
            
          )
          
          train_test_and_plot <- function( geners, tsteps, vsteps, teststeps, e, bsize, cColumnn, t, mdir, cnn, add){
            
            # Train and cross-validate data
            history1 = k %>% fit_generator(
              geners[[1]],
              steps_per_epoch=tsteps,
              epochs=e,
              validation_data=geners[[2]],
              validation_steps=vsteps
            )
            
            # Save plots of train data
            jpeg( file = paste( mdir,'/plots/train/',
                                cnn,'model_',e,'epochs_',bsize,'batchSize_',
                                add,'imgresolution.jpg',sep='' ) )
            y1 = history1$metrics$loss
            y2 = history1$metrics$val_loss
            x = 1:length(history1$metrics$loss)
            plot(x,y1,type='l')
            lines(x,y2,type='l',col=c('red'))
            dev.off()
            
            # Check model by making predictions on never seen data
            history2 = k %>% predict_generator(
              geners[[3]],
              steps=teststeps
              )
            
            # Save plots of test data
            jpeg( file = paste( 'D:/DMjorp/plots/test/',
                                cnn,'model_',epchs,'epochs_',bsize,'batchSize_',
                                add,'imgresolution.jpg',sep='' 
                              ) 
                )
            plot( history2, type='l')
            dev.off()
            
            cnt=1
            right=0
            total = length(tests)
            for(j in 1:length(history2)){
              
              if(history2[j] > .5){ #Normal
                
                if(patient_information[[cColumn]][t[j]] == 'Normal'){
                  right = right+1
                }
                
              }else{ #Diabetes
                
                if( grepl( 'Diabetes', patient_information[[cColumn]][tests[j]])){
                  right=right+1
                }
                
              }
              
            }
            
            # text file folder that contains test information
            filename = paste(mdir,'/plots/test/',
                             cnn,'model_',e,'epochs_',bsize,'batchSize_',
                             add,'imgresolution_results.txt',sep='')
            
            # Write strings out to text file contained in test plot area 
            write( c( paste('correct percentage',100*right/total),
                      paste('mean val loss',mean(history1$metrics$val_loss)),
                      paste('mean train loss',mean(history1$metrics$loss))),
                      file=filename )
          }
          
          trainsteps = 11
          validsteps = 11
          teststeps = 14
          tests = c( 39,40,63,64,65,66,67,68,69,70,71,73,74,75 )
          train_test_and_plot( gens,
                               trainsteps, 
                               validsteps, 
                               teststeps, 
                               epchs,
                               train_batch_size,
                               CLASS_COLUMN, 
                               main_project_directory,
                               CNN_transfer_models[i],
                               additional_input_dimensions ) 
          
          additional_input_dimensions = additional_input_dimensions+10
        }
        
      train_batch_size = train_batch_size+1
      }
    epchs = epchs+25
    }
  }
}
