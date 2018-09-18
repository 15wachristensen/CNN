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
#     pdata is patient data from the projectS
#
#     mainDir is the file folder for the project where R funcitons
#     and certain directories are found (train data, test data, pics, etc.)

# Post: returns filenames associated with the different classes 
#       that now contain the pictures and the specific classes excel file

library(readr)
library(xlsx)
library(keras)
library(dplyr)
library(magick)

org <- function( modeltypes, imres, pdata, mainDir, imdir, imagetype, felenames, classes)
{
  # If you want to pre organize
  # your data into respective classes before grabbing that you can
  # easily make directories for
  
  
  # Crop images to all be 608x608 to put into folder
  bigImagesDir = 'D:/DMjorp/tonguepics2'
  crop <- function(im){
    files = list.files(bigImagesDir)
    
    for(i in 1:length(files)){
      image = image_read(file.path(bigImagesDir,files[i]))
      image = image_crop(image,'608x608')
      image_write(image,path=file.path(imdir,files[i]))
    }
  }
  #crop(bigImagesDir)
  # creates a vector of file names 
  makeClassDirectories <- function(d){
    sen=c() 
    # File that contians the mass storage of photos 
    sen[1]=d
    # File that contains normal patients data/photo
    for(i in 1:length(classes)){
      sen[i+1]=paste(mainDir,paste('/',classes[i]),sep='')
    }
    
    return(sen)
  }
  # dirs=c()
  # dirs=makeClassDirectories(imdir)
  
  # write vectors in memory that contain the strings for 
  # each id that will be used to pull pictures from data file
  # to their respective class folders 

  # Hardcoded.... will need to change 
  # for different amount of classes
  normids=c()
  dids=c()
  classIds=list(normids,dids)
  
  
  # When giving matrix, this number must be adjusted to  
  # give the column where the different classes are wrote 
  #out to excel file
  id_column = 1
  class_column = 4
  
  # NOTE: This is where you would need to start adding rows
  #       to different data frames to record survey questions
  #       and integrate into the model 
  
  # TESTS:
  # print(length(pdata[[id_column]]))
  # print(length(classes))
  
  # This loop seperates the ids into different classes
  for(i in 2:length(pdata[[id_column]])){
    
    # This for loop creates the string and adds to id vectors
    # The names for classes must be EXACT
    
      # TESTS:
      print(paste('data',pdata[[class_column]][i]))
      print(paste('data ids',pdata[[class_column]][i]))
      print(paste('class',classes[j]))
      clause=FALSE
      # Temporary fix for turning different classes of diabetes into one class
      p=''
      if(pdata[[class_column]][i]=='Mild Diabetes' || 
         pdata[[class_column]][i]=='First Semester Diabetes'||
         pdata[[class_column]][i]=='Moderate Diabetes'||
         pdata[[class_column]][i]=='Severe Diabetes')
      {
        clause=TRUE  
      }
      p = paste(imdir,'/',pdata[[id_column]][i],imagetype,sep='')
      #print(paste('p',p))
      
      if(clause==TRUE){
        classIds[[2]][length(classIds[[2]])+1]=p
      }else{
        classIds[[1]][length(classIds[[1]])+1]=p
      }
    
  }
  
  # chooses which ids from each class will go 
  # into the train, validation, and test sets and then 
  # even copies them over to the correct files 
  numFiles = length(normids)+length(dids)
  sep_photos_into_class <- function(c,classes){
    r = 0
    cou = 1
    for(i in 1:length(c)){
      ncnt=1
      for(j in 1:length(c[[i]])){
        n = length(c[[i]])*.8
        from=''
        to = ''
        if(ncnt < n){
          r = sample(1:1000,1)
          if(r < 800){
            to = paste(mainDir,'/data/train/',classes[i],sep='')
          }else{
            to = paste(mainDir,'/data/validation/',classes[i],sep='')
          }
        }else{
          to = paste(mainDir,'/data/test/testfolder',sep='')
        }
        from=paste(c[[i]][j],sep='')
        if(!grepl('NA',from ) & !grepl('70800',from)){
          file.copy(from,to)
        }
        
        # horizontally flip image and place into the same category?
        if(!grepl('NA',from) & !grepl('70800',from)){
          img = image_read(from)
          str(img)
          img = image_flop(img)
          image_write(img,path=paste(to,'/',cou,'.jpg',sep=''))
          cou = cou+1
        }
        
        ncnt=ncnt+1
      }
    }
    
  }
  #sep_photos_into_class(classIds,classes)
  print(paste('norm',length(classIds[[1]])))
  print(paste('diab',length(classIds[[2]])))
  
  # Make generators to feed model, could be made in to a function
  train_data_gen = image_data_generator(rescale = 1/255)
  valid_data_gen = image_data_generator(rescale = 1/255)
  test_data_gen = image_data_generator(rescale = 1/255)
  
  # Parameters that go into RunModel function in 
  # source('D:/Dmjorp/gen_func.R') that will 
  # be adjusted to test model
  
  for(i in 1:length(modeltypes)){
    
    # Make data generators
    # Parameters:
    #
    #  dirs is the name of file directory to find train, valid, test sets
    #
    #  datagens are the data generators for train, valid, test sets
    #
    #  bsizes are the batch sizes for train, valid, test sets
    #  NOTE: 
    #       valid and test set batch sizes should be a divisor 
    #       of the total amount of data points in dir
    # 
    #  cmode is string that represents that class mode wanted (binary, categorical, any others???)
    #
    #  imrs is the target_size parameter, this is different than 
    #  what you feed into model because it only have hxw dimension
    makeGenerators <- function( dirs, datagens, bsizes, cmode,imrs ){
    print(imrs)
    tgen=flow_images_from_directory(
        dirs[1],
        train_data_gen,
        target_size = imrs,
        batch_size=bsizes[1],
        class_mode=cmode
      )
      
      vgen=flow_images_from_directory(
        dirs[2],
        valid_data_gen,
        target_size = imrs,
        batch_size=bsizes[2],
        class_mode=cmode
      )

      testgen=flow_images_from_directory(
        dirs[3],
        test_data_gen,
        target_size = imrs,
        batch_size=bsizes[3],
        class_mode=cmode,
        shuffle=FALSE
      )
      return( c( tgen, vgen, testgen) )
    }
    set_dirs = c( paste(mainDir,'/data/train',sep = ''), 
                  paste(mainDir,'/data/validation',sep=''),
                  paste(mainDir,'/data/test',sep='') 
                )
    batches = c(8,1,1)
    ims = vector(length=1)
    ims[1] = imres[[i]][1]
    ims[2] = imres[[i]][1]
   
    # vector of generators
    gens = makeGenerators( set_dirs, generators, batches, 'binary',ims )
    
    
    im = imres[[i]]
    num_dense_layers = 1
    uns = c(1)
    cominfo = list('binary_crossentropy','adam',1e-4,'acc')
    trainsteps = 2
    epochs = 2
    validsteps = 3
    # Call function to make model 
    # If you want to know what the parameters represent check source code
    source('D:/Dmjorp/gen_func.R')
    k = BuildModel( modeltypes[i], im, num_dense_layers, uns, cominfo,
                    trainsteps, epochs, validsteps )
    
    # Train and test data 
    history1 = k %>% fit_generator(
      gens[[1]],
      steps_per_epoch=trainsteps,
      epochs=epochs,
      validation_data=gens[[2]],
      validation_steps=validsteps
    )
    
    # Predict steps values 
    steps=11
    history2 = k %>% predict_generator(
      gens[[3]],
      steps=steps,
      verbose=2
    )
    
    #Create directories for plots on mainDir
    # to save train/test result result plots from 
    # val_loss and train loss
    dir.create(paste( mainDir,'/plots',sep=''))
    dir.create(paste(mainDir,'/plots/train',sep=''))
    dir.create(paste(mainDir,'/plots/test',sep=''))
    
    jpeg( file= paste( mainDir,'/plots/train/',modeltypes[i],'.jpg',sep='' ) )
    y1 = history1$metrics$loss
    y2 = history1$metrics$val_loss
    x = 1:length(history1$metrics$loss)
    plot(x,y1)
    dev.off()
    jpeg( file= paste( 'D:/DMjorp/plots/test/',modeltypes[i],'.jpg',sep='' ) )
    plot(history2)
    dev.off()
    
    # Print the predicted category from test data
    predictions=vector(length=length(history2))
    for(i in 1:length(history2)){
      if(history2[i] > .5){
        predictions[i]='Normal'
      }else{
        predictions[i]='Diabetes'
      }
    }
    str=substr(list.files('D:/DMjorp/data/test/testfolder'),1,6)
    
    # cnt=1
    # right=0
    # wrong=0
    # tests = c(39,40,63,64, 65,66,67,68,69,70)
    # print(predictions)
    # for(i in 1:length(tests)){
    #   if(predictions[i]=='Diabetes'){
    #     if(grepl(pdata[[class_column]][tests[i]],'Diabetes')){
    #       right=right+1
    #     }else{
    #       wrong=wrong+1
    #     }
    #   }else{
    #     if(predictions[i]==pdata[[class_column]][tests[i]]){
    #       right=right+1
    #     }else{
    #       wrong=wrong+1
    #     }
    #   }
    #   cnt=cnt+1
    # }
    # 
    # 
    # 
    # print(paste('right',right,'wrong',wrong))
    # 
    # 
  }
}