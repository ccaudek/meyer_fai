# this is a section of R script that prepares the analysis; it imports and uploads some libraries which will be used later on, and defines some useful settings and functions.
# here you may want to add some more advanced functions later on
# there is no need to modify anything for a basic analysis

# install and upload the relevant libraries ####

# added 'stargazer' and 'MASS' for sensitivity analyses

for (n in c('foreign', 'car', 'utils', 'relimp', 'ggplot2', 'ggdendro', 
            'tidyr', 'reshape2', 'tibble', 'stargazer',
            'psych', 'memisc', 'Hmisc', 'ltm', 'MASS',
            'lavaan','semTools','semPlot', 'qgraph','sem',
            'mirt', 'eRm', 'mokken', 'rgl','scales',
            'CTT','MBESS', 'knitr', 'bookdown',
            'tidyverse', 'readxl', 'mice', 'VIM',
            'paran', 'polycor', 'multilevel'))
{
  if(!require(n, character.only=TRUE))
  {
    stop(paste0("Package '",n,"' is not installed: please install it and try again!\n"));
    # install.packages(n)
  }
  library(n, character.only=TRUE)
}

options(max.print = 1000000)


# add captioning for figures and tables (as here: https://www.r-bloggers.com/r-markdown-how-to-number-and-reference-tables/)

# table_nums <- captioner::captioner(prefix = "Table")
# figure_nums <- captioner::captioner(prefix = "Figure")
# t.ref <- function(x) {
#   stringr::str_extract(table_nums(x), "[^:]*")
# }
# f.ref <- function(x) {
#   stringr::str_extract(figure_nums(x), "[^:]*")
# }

# add functions needed in analyses

# defines a function to partition an item set into mokken scales - lowerbound from .05 to .80 
moscales.for.lowerbounds <- function( x, lowerbounds=seq(from=0.05,to=0.80,by=0.05) )
{
  ret.value <- NULL;
  for( lowerbound in lowerbounds )
  {
    tmp <- aisp( x,  lowerbound=lowerbound );
    if( is.null(ret.value) )
    {
      ret.value <- data.frame( "Item"=rownames(tmp), "Scales."=tmp[,1] );
    }
    else
    {
      ret.value <- cbind( ret.value, "Scales."=tmp[,1] );
    }
    names(ret.value)[ncol(ret.value)] <- sprintf("%.2f",lowerbound);
  }
  rownames(ret.value) <- NULL;
  ret.value;
}

# correlation tables with stars adapted from here: http://myowelt.blogspot.nl/2008/04/beautiful-correlation-tables-in-r.html;
# for pearson corrs
corstars1 <- function(x){ 
  require(Hmisc) 
  x <- as.matrix(x) 
  R <- rcorr(x, type="pearson")$r 
  p <- rcorr(x, type="pearson")$P 
  ## define notions for significance levels; spacing is important.
  mystars <- ifelse(p < .001, "***", ifelse(p < .01, "** ", ifelse(p < .05, "* ", ifelse(p < .10, "# ", " "))))
  ## truncate the matrix that holds the correlations to two decimals
  R <- format(round(cbind(rep(-1.11, ncol(x)), R), 2))[,-1] 
  ## build a new matrix that includes the correlations with their apropriate stars 
  Rnew <- matrix(paste(R, mystars, sep=""), ncol=ncol(x)) 
  diag(Rnew) <- paste(diag(R), " ", sep="") 
  rownames(Rnew) <- colnames(x) 
  colnames(Rnew) <- paste(colnames(x), "", sep="") 
  ## remove upper triangle
  Rnew <- as.matrix(Rnew)
  Rnew[upper.tri(Rnew, diag = TRUE)] <- ""
  Rnew <- as.data.frame(Rnew) 
  ## remove last column and return the matrix (which is now a data frame)
  Rnew <- cbind(Rnew[1:length(Rnew)-1])
  return(Rnew) 
}

# for spearman corrs
corstars2 <- function(x){ 
  require(Hmisc) 
  x <- as.matrix(x) 
  R <- rcorr(x, type="spearman")$r 
  p <- rcorr(x, type="spearman")$P 
  ## define notions for significance levels; spacing is important.
  mystars <- ifelse(p < .001, "***", ifelse(p < .01, "** ", ifelse(p < .05, "* ", ifelse(p < .10, "# ", " "))))
  ## trunctuate the matrix that holds the correlations to two decimal
  R <- format(round(cbind(rep(-1.11, ncol(x)), R), 2))[,-1] 
  ## build a new matrix that includes the correlations with their apropriate stars 
  Rnew <- matrix(paste(R, mystars, sep=""), ncol=ncol(x)) 
  diag(Rnew) <- paste(diag(R), " ", sep="") 
  rownames(Rnew) <- colnames(x) 
  colnames(Rnew) <- paste(colnames(x), "", sep="") 
  ## remove upper triangle
  Rnew <- as.matrix(Rnew)
  Rnew[upper.tri(Rnew, diag = TRUE)] <- ""
  Rnew <- as.data.frame(Rnew) 
  ## remove last column and return the matrix (which is now a data frame)
  Rnew <- cbind(Rnew[1:length(Rnew)-1])
  return(Rnew) 
}

