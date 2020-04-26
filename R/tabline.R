#' Convert output from function checktest assessing various aspects in a two-stage design into a line of the table in  the manuscript
#'
#' @param cto output from checktest
#' @param latex flag governing whether the line is formatted in a way that lets it be placed directly into the paper.
#'
#' @return The table line
#' @export
tabline<-function(cto,latex=TRUE){
   refl<-cumsum(c(cto$m1,cto$m2))*cumsum(c(cto$m1,cto$m2))
   line<-c(round((1-cto$quantiles)*100,2),refl-cto$cunc,refl-cto$ccor,
      round(100*c(cto$power,cto$uncedspower,cto$coredspower),2))
   if(latex) line<-paste(paste(line,colapse="&"),"\\\\",sep="")
   return(line)
}
