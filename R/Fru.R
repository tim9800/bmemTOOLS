#' Estimate pooled F of a model from multiply imputed data
#'
#' Returns the F statistic of a regression model, given the response and
#' predictor variables. Not all variables need to be tested for significance
#' simultaneously, which allows F-testing of hierarchical regressions.
#'
#' @param dataset mids object, a multiply-imputed dataset generated from mice
#' @param response character vector, specifying the name of the response variable in the dataset
#' @param predictors vector, specifying the name(s) of the predictors in the dataset
#' @param testpredictors vector, specifying the name(s) of predictors to be tested for significance
#'
#' @return
#' @export
#' @importFrom stats coef vcov
#' @rawNamespace import(mice, except = getNamespaceExports("mice"))
#' @importFrom mice pool
#' @importFrom miceadds create.designMatrices.waldtest MIwaldtest
#' @importFrom mitools MIextract
#'
#'
#' @examples
#' mids <- mice::mice(mice::nhanes, m=5, maxit=1)
#' outcome <- "hyp"
#' predictors <- c("age", "bmi")
#' testpredictors <- c("age","bmi")
#' F1 <- Fru(mids, outcome, predictors, testpredictors)
Fru <- function(dataset, response, predictors, testpredictors){
  eq <- paste(response, " ~ ",
              paste(predictors, collapse =" + "))
  model <- with(dataset, lm(as.formula(eq)))
  pooled_model <- pool(model)

  # Matrix of regression coefficients
  qhat <- MIextract(model$analyses, fun=coef)
  # Variance-covariance matrix
  u <- MIextract(model$analyses, fun=vcov)

  # Creating a vector containing the parameter names of the model.
  pars <- names(qhat[[1]])

  # Creating a design matrix indicating which of the parameters in qhat are tested simultaneously. The
  # create.designMatrices.waldtest function facilitates the creation of the design matrix. Since the
  # miceadds manual (Robitzsch, Grund, & Henke, 2017), #pp. 103-107) gives some clear examples of this
  # function, the next lines are not further explained.
  design <- create.designMatrices.waldtest(pars = pars,
                                           k = length(testpredictors))
  Cdes <- design$Cdes
  rdes <- design$rdes
  ii <- 0
  for (predictor in testpredictors) {
    ii <- ii +1
    Cdes[ii, predictor] <- 1
  }

  # The MIwaldtest function in the miceadds package calculates a pooled F value testing the
  # parameters in vector "testparameters" for significance
  Wald <- MIwaldtest(qhat, u, Cdes, rdes)
  Wald
}
