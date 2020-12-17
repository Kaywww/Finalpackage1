#' Dealing with NAs in the dataframe
#'
#' @param data original dataframe
#' @param method "Default","delete": delete all observation with NAs;
#' "mean": impute the missing value by the mean of the column;
#' "knn": impute the missing value by knn method
#'
#' @return
#' @export
#'
#' @examples
#' dealing_NAs(dataframe, method="knn")
dealing_NAs=function(data,method="Default"){
  nna=sum(is.na(data))
  if(nna==0){
    return(c("There is no NA in the dataframe"))
  }
  else if(method=="delete" | method=="Default" && nna!=0){
      data1= data [complete.cases(data),]
      return(data1)
    }
  else if(method=="mean"&& nna!=0){
    for (i in(1:(ncol(data))))
      {data1[,i] <- rep(mean(data1[,i], na.rm=T), length(data1[,i]))}
      return(data1)
    }
  else if(method=="knn"&& nna!=0){
      data1 <- DMwR::knnImputation(data[, !names(data) %in% "colname"])
      return(data1)
  }
  else{return(c("wrong input in method"))}
}
