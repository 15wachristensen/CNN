# CNN Model With Clean Data
Provides the methods to clean data, train CNN, and make predictions. Each row of excel file(table) should represent a unique data entry with certain columns. These methods rely heavily on r-keras functions found with a quick "keras cran documentation" Google search. Note the structure of train, valid test folders. Some numbers are not included as parameter and may need to be changed near the place they're needed. Also saves plots of different types of transfer models to directories it manually creates for you. Below is an example of a method call to run the class

RunMain(
        main_directory_filename, 
        vector_strings_for_classes, 
        image_data_directory_filename,
        c('vgg16','xception','resnet50','resnetv2'), -----> Transfer models supported 
        list(c(64,64,3),c(71,71,3),c(197,197,3),c(139,139,3)), Input dimensions for above ------>
        image_file_extension
        )
        
