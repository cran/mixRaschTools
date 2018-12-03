
#' mixRaschTools Functions -----------------------------------------------------------
#' @name Item parameter plot for mixed Rasch model
#' @title Item parameter plot for mixed Rasch model
#' @description This function produces a plot of item parameters by latent class for Rasch calibrated mixture models.
#' @usage
#' mixRasch.plot(x, xlab, ylab)
#' @param  x A fitted multiple class mixture Rasch model
#' @param xlab user defined label for x axis
#' @param ylab user defined label for y axis
#' @details This function provides an item parameter plot that can be used to compare the different item
#' parameters in mixture Rasch models that contain two or more latent classes.
#' @author Pamela S Trantham
#' @examples
#' ##Example multiple class mixture Rasch models included with mixRaschTools
#' data(threeclass_ex)
#' ##Item Parameter Plot
#' mixRasch.plot(threeclass_ex)
#' @export
mixRasch.plot <- function(x, xlab, ylab){
  nc<- length(x$LatentClass)
    plottemp <- sapply(x$LatentClass, function(x) x$item.par$delta.i)
  matplot(plottemp, type = "o", main = "Item Parameters by Class", xlab = "Items", ylab = "Parameters", pch = 20, bg = 1:nc)
  abline(h = 0, lty = 2)
  par(mfrow = c(1, 2))
  legend("topright", legend = paste("class", 1:nc, sep = " "), fill = 1:nc, col=1:nc)
}

#' mixRaschTools Average Theta -----------------------------------------------------------
#' @name Average Ability Levels for Multiple Class Mixed Rasch Model
#' @title Average Ability Levels for Multiple Class Mixed Rasch Model
#' @description This function produces mean ability levels for each class in a mixture Rasch model.
#' @usage
#' avg.theta(x)
#' @param x A fitted multiple class mixture Rasch model
#' @details This function produces a matrix containing the average theta values for each latent class
#' included in mixture Rasch model.
#' @author Pamela S Trantham
#' @references Willse, J. T. (2011). Mixture Rasch models with joint maximum likelihood estimation. Educational and Psychological Measurement, 71, 5-19, https://doi.org/10.1177/0013164410387335
#' @examples
#' ##Example multiple class mixture Rasch models included with mixRaschToolkit
#' data(threeclass_ex)
#' ##Average Theta Values
#' avg.theta(threeclass_ex)
#' @importFrom 'graphics' 'matplot' 'abline' 'par' 'legend'
#' @export
avg.theta<- function(x){
    outtheta <- sapply(x$LatentClass,
      function(x) mean(x$person.par$theta))
    return(outtheta)
}

#' mixRaschTools Sample Four Class Model -----------------------------------------------------------
#' @name fourclass_ex
#' @docType data
#' @description Object containing model information for a four class model.
#' @format This four class mixed Rasch model was created with  the use of the mixRasch program (Willse,
#' 2014) using a subset of the 2012 PISA US  math achievement data (OECD, 2012).  This model was created using  the "Math Self Efficacy" scale included in the
#' in the contextual assessment included in that year.  The dataset used for the models was created using students from the United States who had completed all
#' math questions contained in the PM7A booklet.  The final dataset contained 12 dichotomously scored
#' math achievement items creating a sample of 1229 students.  These models contain the following parameters:
#' "fourclass_ex" contains the ability estimates, item parameters, and standard errors for a four class Rasch model fitted using the mixRasch program.
#' "threeclass_ex" contains the ability estimates, item parameters, and standard errors for a three class Rasch model fitted using the mixRasch program.
#' @source {http://www.oecd.org/pisa/pisaproducts/pisa2012database-downloadabledata.htm}
#' @references
#' Organisation for Economic Co-operation and Development., & Programme for International
#'       Student Assessment. (2012a).  Paris: OECD.
#' Willse, J. T. (2014).  Package mixRasch.  R package version 1.1.
#' @keywords {datasets}
"fourclass_ex"

#' mixRaschTools Sample Three Class Model -----------------------------------------------------------
#' @name threeclass_ex
#' @docType data
#' @description Object containing model information for a three class model.
#' @format This three class mixed Rasch model was created with  the use of the mixRasch program (Willse,
#' 2014) using a subset of the 2012 PISA US math achievement data (OECD, 2012).  This model was created using  the "Math Self Efficacy" scale included in the
#' in the contextual assessment included in that year.  The dataset used for the models was created using students from the United States who had completed all
#' math questions contained in the PM7A booklet.  The final dataset contained 12 dichotomously scored
#' math achievement items creating a sample of 1229 students. These models contain the following parameters:
#' "fourclass_ex" contains the ability estimates, item parameters, and standard errors for a four class Rasch model fitted using the mixRasch program.
#' "threeclass_ex" contains the ability estimates, item parameters, and standard errors for a three class Rasch model fitted using the mixRasch program.
#' @source {http://www.oecd.org/pisa/pisaproducts/pisa2012database-downloadabledata.htm}
#' @references
#' Organisation for Economic Co-operation and Development., & Programme for International
#'       Student Assessment. (2012a).  Paris: OECD.
#' Willse, J. T. (2014).  Package mixRasch.  R package version 1.1.
#' Willse, J. T. (2011). Mixture Rasch models with joint maximum likelihood estimation.
#'      Educational and Psychological Measurement, 71, 5-19.https://doi.org/10.1177/0013164410387335
#' @keywords {datasets}
"threeclass_ex"

