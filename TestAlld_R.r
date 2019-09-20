# automates chi-squared test, t-student test and kruskal test for statistics
# in a dataframe with factor and/or numeric variables
# insert dataframe, wait the magic

# d: dataframe
# testtype: 'chisq.test', 't.test', 'kruskal.test'
# toPrint: 'matrix' for a matrix, 'association' for string like "A - B : p-value_valor_under_or_equal_to_0.05"
# extension: 'txt', 'csv', etc...
# PATHset: 'path/in/which/save/your/file'
# yates: yate's correction for chi-squared test, default: FALSE

TestAll <- function(d, testtype = NULL, toPrint = NULL, extension = NULL, PATHset = NULL, yates = FALSE) {
  
  if(!is.data.frame(d)){
    warning(paste(d,"is not a dataframe"))
    exit()
  }
  if(dim(d)[1] == 0 || dim(d)[2] == 0){
    warning(paste(d,"is empty"))
    exit()
  }
  
  library(xlsx)
  
  if(!is.null(testtype) && (testtype == 'chisq.test' || testtype == 't.test')){
    
    if(testtype == 'chisq.test') is.filter <- sapply(d, is.factor)
    else is.filter <- sapply(d, is.numeric)
    
    d1 <- d[, is.filter]
    
    m <- matrix(data = NA, nrow = ncol(d1), ncol = ncol(d1))
    rownames(m) <- colnames(d1)
    colnames(m) <- colnames(d1)
    
    for(c in seq(1, ncol(d1), 1)){
      for(r in seq(1, ncol(d1), 1)){
        if(testtype == 'chisq.test')
          m[r,c] <- substr(chisq.test(d1[, c], d1[, r],correct = yates)$p.value, 0, 9)
        else if(testtype == 't.test'){
          m[r,c] <- substr(t.test(d1[, c], d1[, r])$p.value, 0, 9)
        }
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
  
  path <- paste(PATHset,toPrint,'_',testtype,'.',extension, sep="")
  
  if(!is.null(toPrint) && toPrint == 'association'){
    association <- vector()
    association <- pSearch(m, testtype)
    print(association)
    if(!is.null(extension) || !is.null(PATHset)) write.table(association, path, sep="\t")
  }
  else if(!is.null(toPrint) && toPrint == 'matrix'){
    print(m)
    if(!is.null(extension) || !is.null(PATHset)) write.table(association, path, sep="\t")
  }
}


pSearch <- function(m, testtype) {
  association <- vector()
  for(c in seq(1, ncol(m), 1)){
    for(r in seq(1, nrow(m), 1)){
      if(as.numeric(m[r,c] <= 0.05) && r != c){
        association <- append(association,paste(colnames(m)[c],"-",rownames(m)[r],":",m[r,c]))
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
