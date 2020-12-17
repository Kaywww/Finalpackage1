#' Provide the basic statistic graph for every variable in the dataframe
#'
#' @param data target dataframe
#'
#' @return
#' @export
#'
#' @examples
#' basic_plot(dataframe1)
basic_plot=function(data){
  n=ncol(data)
  par(mfrow=c((round(n/3)+1),3))
  for(i in (1:n)){
    if(is.numeric(data[,i])==TRUE){
      hist(data[,i], freq = FALSE, xlab = colnames(data)[i], ylab="Density", col = "blue", main = NULL)
      lines(density(data[,i]), lwd=2, col = "red")
    }
    else{
      barplot(prop.table(table(data[,i])), xlab = colnames(data)[i], ylab="Density", col = "blue")
    }
  }
}
