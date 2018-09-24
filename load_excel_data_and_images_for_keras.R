# By: Walker Christensen

# Pre: 
#      main_project_directory is folder containing necessary functions and file strings
#      for project 
#
#      class_names is a vector containing the types of classes
#      needed to make for file directories and make predicitions on
#
#      class_namesphoto_data_directory is the file name to grab image data from to match with excel ids
#
#      CNN_transfer_models is a vector of strings representing the models you want to test for data
#       choice of 1. vgg16  2.vgg19   3.xception  4. resnet50   5. resnetv2
#
#      image_resolution is a vector of 3-d vectors that contains the 
#      parallel image resolution for the CNN_transfer_models parameter 
#      Note: 
#       1. vgg16 can be any  
#       2. ???
#       3. xception has minimum of 71x71  
#       4. resnet50 has a minimum of 197x197
#       5. restnetv2 has a minimum of 139x139
#
#      image_type is the type of image file ( .jpg,.png ) in string format
#       
#      Note: Files are put into pre made folder, new projects must be emptied 
#            to keep organized if using more than initial run.
#
# Post: This function will create folders to split excel 
#       data and image data to use for keras models.
#
# This package uses flow_images_from_directory to feed model 

library(readr)
library(xlsx)

RunMain <- function( main_project_directory,
                     class_names,
                     photo_data_directory,
                     CNN_transfer_models,
                     image_resolution,
                     image_type )
{
    numOfClasses=length(class_names)

    # File that contains the funciton able to 
    #   create the file structure necessary for flow_images_from_directory
    #   ( train, validation, test directories each containing folders
    #   for the class_names given, for first time use ONLY ). 
    #   This function returns a matrix of strings for the folders
    #   described above.
    # source(paste(main_project_directory,'/classfiles.R',sep=''))
    # names=fnames(class_names)
    
    # File to find the info of the patients 
    # ( excel file w/ i.d. column and class_column )
    pfile = paste( main_project_directory, '/englishdata.xlsx', sep='' )
    
    # Reading an xlsx file results into a data frame object
    patient_information = read.xlsx( file=pfile,1 )
    
    # Method to organize the images given the class_names 
    # source(paste(main_project_directory,'/organizedmdata.R',sep=''))
    foo=org(
             CNN_transfer_models, 
             image_resolution, 
             patient_information, 
             main_project_directory, 
             photo_data_directory, 
             image_type,  
             class_names 
            )
    
}