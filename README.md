# CNN Model With Clean Data

Provides the methods to clean data, train CNN, and make predictions. The main goal was to create an easily re-usable CNN model structure that supports how data is usually recieved for a project. 

Each row of excel file(table) should represent a unique data entry with certain columns. These methods rely heavily on r-keras functions found with a quick "keras cran documentation" Google search. Note the structure of train, valid test folders. Some numbers are not included as parameter and may need to be changed near the place they're needed. Also saves plots of different types of transfer models and hyper-parameters to directories it manually creates for you. 
