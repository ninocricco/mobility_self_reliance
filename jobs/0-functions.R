#------------------------------------------------------------------------------
# PROJECT: MOBILITY SELF-RELIANCE
# FILE: CUSTOM USEFUL DATA CLEANING FUNCTIONS
# AUTHOR: NINO CRICCO
# CONTACT: ncricco@g.harvard.edu
#------------------------------------------------------------------------------

# This function does the opposite of %in% - works as an inverse selector
'%!in%' <- function(x,y)!('%in%'(x,y))

# This function converts all values in a vector to NA
na_codes <- function(x, ...) {
  x[x %in% c(...)] <- NA
  x
}

# This function extracts all numeric characters from a string
numextract <- function(string){ 
  str_extract(string, "\\-*\\d+\\.*\\d*")
} 

# This function calculates the p-value appropriate for combined
# regression coefficients across imputations
MIcombineP <- function(MIcombineRes,digits=3) {
  tStat <- MIcombineRes$coefficients/sqrt(diag(MIcombineRes$variance))
  round(2*pt(-abs(tStat),df=MIcombineRes$df),digits)
}