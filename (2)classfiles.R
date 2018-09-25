
# This function splits the classes given into folders to hold data 
# Can change the f1,f2,f3 to put train, valid, test files in any other 
# created directory *
fnames <- function(classes){
  # filenames to hold data in matrix format
  numClasses=length(classes)
  filenames = matrix(ncol=numClasses)
  f = c()
  for( i in 1:length(classes) )
  {
    # * For example instead i could write
    # f1 = file.path(paste('D:/DMjorp/MODEL1DATA/train/',classes[i],sep=''))
    # as long as there is a directory D:/DMjorp/MODEL1DATA created
    
    f1 = file.path(paste('D:/DMjorp/train/',classes[i],sep=''))
    f1 = tolower(gsub(' ','',f1))
    f[length(f)+1]=f1
    dir.create(f1)
  
    f2 = file.path(paste('D:/DMjorp/valid/',classes[i],sep=''))
    f2 = tolower(gsub(' ','',f2))
    f[length(f)+1]=f2
    dir.create(f2)
    
    f3 = file.path(paste('D:/DMjorp/test/',classes[i],sep=''))
    f3 = tolower(gsub(' ','',f3))
    f[length(f)+1]=f3
    dir.create(f3)
    
    filenames <- rbind(filenames,f)
    f = c()
    
  }
  
  return(filenames)
}
