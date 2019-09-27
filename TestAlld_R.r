# This method automates chi-squared test, t-student test and kruskal test for statistics with them correlation force test
# in a dataframe with factor and/or numeric variables
# insert dataframe, wait the magic
#
# d: dataframe
#
# testtype: 'chisq.test', 't.test', 'kruskal.test'
#
# format:
#   'matrix' square matrix
#   'association' string in this format: "A - B : p-value/cramer/pearson_only_if_p-value_under_or_equal_0.05"
#
# correlation:
#   'p-value' for square matrix: all result; for association: only p-value <= 0.05
#   'cramer' for square matrix: all result; for association: only cramer_correlation with p-value <= 0.05
#            string like "A - B : cramer_correlation"
#   'pearson' for square matrix: all result; for association: only pearson_correlation with p-value <= 0.05
#             string like "A - B : pearson_correlation"
#
# extension: 'txt', 'csv', etc...
#
# PATHset: 'path/in/which/save/your/file'
#
# yates: yate's correction for chi-squared test, default: FALSE

TestAll <- function(d, testtype, correlation = NULL, format = NULL, extension = NULL, PATHset = NULL, yates = FALSE) {
  
  if(!is.data.frame(d)){
    warning(paste(d,"is not a dataframe"))
    exit()
  }
  if(dim(d)[1] == 0 || dim(d)[2] == 0){
    warning(paste(d,"is empty"))
    exit()
  }
  if(!class(yates) == 'logical'){
    warning(paste(yates,"is not boolean;
                  accepted values: TRUE or FALSE"))
    exit()
  }
  
  library(xlsx); library(lsr)
  
  
  if(!is.null(testtype) && (testtype == 'chisq.test' || testtype == 't.test')){
    
    if(testtype == 'chisq.test') is.filter <- sapply(d, is.factor)
    else is.filter <- sapply(d, is.numeric)
    
    d1 <- d[, is.filter]
    
    m <- matrix(data = NA, nrow = ncol(d1), ncol = ncol(d1))
    rownames(m) <- colnames(d1)
    colnames(m) <- colnames(d1)
    
    for(c in seq(1, ncol(d1), 1)){
      for(r in seq(1, ncol(d1), 1)){
        if(testtype == 'chisq.test' && correlation == 'p-value')
          m[r,c] <- substr(chisq.test(d1[, c], d1[, r],correct = yates)$p.value, 0, 9)
        else if(correlation == 'cramer')
          m[r,c] <- substr(cramersV(d1[, c], d1[, r]), 0, 9)
        else if(testtype == 't.test' && correlation == 'p-value')
          m[r,c] <- substr(t.test(d1[, c], d1[, r])$p.value, 0, 9)
        else if(correlation == 'pearson')
          m[r,c] <- substr(cor.test(d1[, c], d1[, r], method = "pearson")$estimate, 0, 9)
      }
    }
  }
  
  else if(!is.null(testtype) && testtype == 'kruskal.test'){
    
    is.num <- sapply(d, is.numeric)
    d1 <- d[, is.num]
    
    is.fact <- sapply(d, is.factor)
    d2 <- d[, is.fact]
    
    m <- matrix(data = NA, nrow = ncol(d2), ncol = ncol(d1))
    rownames(m) <- colnames(d2)
    colnames(m) <- colnames(d1)
    
    for(c in seq(1, ncol(d1), 1)){
      for(r in seq(1, ncol(d2), 1)){
        m[r,c] <- substr(kruskal.test(d1[, c], d2[, r])$p.value, 0, 9)
      }
    }
  }
  
  path <- paste(PATHset,format,'_',testtype,'_',correlation,'.',extension, sep="")
  
  if(!is.null(format) && (format == 'association')){
    association <- vector()
    association <- pSearch(d1, m, testtype, correlation, format)
    print(association)
    if(!is.null(extension) && !is.null(PATHset)) write.table(association, path, sep="\t")
  }
  else if(!is.null(format) && format == 'matrix'){
    print(m)
    if(!is.null(extension) && !is.null(PATHset)) write.table(m, path, sep="\t", row.names=TRUE)
  }
}


pSearch <- function(d, m, testtype, correlation ,format) {
  
  association <- vector()
  
  for(c in seq(1, ncol(m), 1)){
    for(r in seq(1, nrow(m), 1)){
      if(as.numeric(m[r,c] <= 0.05) && r != c){
        if(format == 'association' && correlation == 'p-value') association <- append(association,paste(colnames(m)[c],"-",rownames(m)[r],":",m[r,c]))
        else if(correlation == 'cramer')
          association <- append(association,paste(colnames(m)[c],"-",rownames(m)[r],":",substr(cramersV(d[, c], d[, r]), 0, 9)))
        else if(correlation == 'pearson')
          association <- append(association,paste(colnames(m)[c],"-",rownames(m)[r],":",substr(cor.test(d[, c], d[, r], method = "pearson")$estimate, 0, 9)))
      }
    }
  }
  if(!is.null(testtype) && testtype != 'kruskal.test') association <- cleanVector(association)
  return(sort(association))
}


cleanVector <- function(v) {
  tmp <- vector()
  tmp1 <- vector()
  newV <- vector()
  str <- ''
  for (i in 1:length(v)) {
    tmp <- append(tmp,strsplit(v[i], " : "))
    tmp1 <- append(tmp1,strsplit(tmp[[i]][1], " - "))
    str <- paste(tmp1[[i]][2],"-",tmp1[[i]][1])
    for(k in 1:length(v)){
      if(grepl(str,v[k])){
        newV <- append(newV,v[k])
        v[k] <- NA
      }
    }
  }
  return(newV)
}

exit <- function() {
  .Internal(.invokeRestart(list(NULL, NULL), NULL))
}
