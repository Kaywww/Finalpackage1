#' Showing AICs and BICs for three models to select the best
#'
#' @param model1 regression model 1
#' @param model2 regression model 2
#' @param model3 regression model 3
#'
#' @return
#' @export
#'
#' @examples
#' model_select(lm(y1~x1+x2),lm(y1~x1:x2),lm(y1~I(x1^2)+x2))
model_select=function(model1,model2,model3){
  whole=dplyr::full_join(model1,model2,model3)
  aic=numeric(length=3)
  for(i in 4){
    aic[i]=AIC(whole[i])
  }
  bic=numeric(length=3)
  for(i in 4){
    bic[i]=BIC(whole[i])
  }
  results=dplyr::full_join(aic,bic)
  colnames(results)=c("AIC","BIC")
  return(results)
}
