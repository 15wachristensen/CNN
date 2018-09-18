# By: Walker Christensen

# General function that can use the different directories
#  (train,valid,test) to seperate the data into their 
#   respective classes while at the same time splitting
#   excel data and image data to use for keras models

# Pre: 
#      maindir is folder containing necessary functions and file strings
#      for project 
#
#      classes is a vector containing the types of classes
#      needed to make for file directories and make predicitions on
#
#      photodata is the file name to grab image data from to match with excel ids
#
#      modeltypes is a vector of strings representing the models you want to test for data
#       choice of 1. vgg16  2.vgg19   3.xception  4. resnet50   5. resnetv2
#
#      imres is a vector of 3-d vectors that contains the image resolution for 
#      the modeltypes parameter 
#      Note that 
#       1. vgg16 can be any  
#       2. ???
#       3. xception has minimum of 71x71  
#       4. resnet50 has a minimum of 197x197
#       5. restnetv2 has a minimum of 139x139
#
#      imagetype is the type of image file 
#       
#      Note: Files are put into pre made folder, new projects must be emptied 
#            to keep organized
#
# Post: This function will create folders to split excel 
#       data and image data to use for keras models.
#       The type of file systems is correct for using generator functions


library(readr)
library(xlsx)

RunMain <- function( maindir, classes, photodata, modeltypes, imres, imagetype )
{
    numOfClasses=length(classes)

    # file that contains the funciton able to 
    # create the file structure necessary for generator
    # (train,valid,test sets each containin folders
    #  for the classes given in , used for first time use ) 
    # source(paste(maindir,'/classfiles.R',sep=''))
    
    # This function returns a matrix with coloumn 1 used for 
    # train folder that holds the different classes, a valid folder
    # that holds the different classes, and a test folder 
    # that holds the different classes
    # names=fnames(classes)
    
    # File to find the info of the patients
    # Reading an xlsx file results in a data frame object
    
    pfile = paste( maindir,'/englishdata.xlsx',sep='' )
    pats = read.xlsx( file=pfile,1 )
    
    #print(pats[[1]])
    
    # Specific method to organize the images given the classes 
    # source(paste(mainDir,'/organizedmdata.R',sep=''))
    
    # Test to get filenames contained in name matrix that should
    # be sized 3 x numOfClasses.
    # print('names')
    # print(names)
    
    foo=org( modeltypes, imres, pats, maindir, photodata, imagetype, names, classes )
    
}