####### 1. import the neccessary package, data and function #####
plot_items_pattern = function( data.df, colNames=NULL) {
  ##*******************************************************************************
  ## fucntion to plot a vector of continuous values using boxplot or              #
  ##      a vector of discrete values using bar plot                              #
  ## Author: Xiaohua Douglas Zhang, January 2020                                  #
  ## Arguments:                                                                   #
  ##   data.df: a data.frame to be plotted using boxplot or                       #
  ##            a discrete vector to be plotted using bar plot                    #
  ##   colNames: names for the columns in data.df                                 #
  ## Output:                                                                      #
  ##   None                                                                       #
  ##******************************************************************************* 
  
  if( is.null(colNames) ) colNames = colnames(data.df)
  nColumn = length(colNames)
  for(i in 1:nColumn ) {
    x = unlist(data.df[,i])
    if( is.character(x) | is.factor(x) | is.logical(x) ) {
      barplot( table(x), main=colNames[i] )
    } else if( is.numeric(x) ) {
      hist( x, main=colNames[i] )
    } else {
      stop(paste("the", i, "th column of data.df must be character, numeric, factor or logical."))
    }
  }
}