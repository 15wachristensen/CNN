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

RunMain <- function(maindir,classes,photodata,imagetype)
{
    numOfClasses=length(classes)

    # file that contains the funciton able to 
    # create the file structure necessary for generator
    # (train,valid,test sets each containin folders
    #  for the classes given in )
    # source(paste(maindir,'/classfiles.R',sep=''))
    
    # This function returns a matrix with coloumn 1 used for 
    # train folder that holds the different classes, a valid folder
    # that holds the different classes, and a test folder 
    # that holds the different classes
    # names=fnames(classes)
    
    # File to find the info of the patients
    # Reading an xlsx file results in a data frame object
    pfile=paste(maindir,'/englishdata.xlsx',sep='')
    pats=read.xlsx(file=pfile,1)
    #print(pats[[1]])
    
    # Specific method to organize the images given the classes 
    # source(paste(mainDir,'/organizedmdata.R',sep=''))
    
    # Test to get filenames contained in name matrix that should
    # be sized 3 x numOfClasses.
    # print('names')
    # print(names)
    
    foo=org(pats,maindir,photodata,imagetype,names,classes)
    
}