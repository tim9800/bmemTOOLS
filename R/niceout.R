#' Elegant tables for regressions on multiply imputed data
#'
#' Outputs nicely-formatted regression tables in html format, containing standardised and
#' unstandardised regression coefficients with SEs, p values, t statistics,
#' F statistics, R-squared and R-squared change for regression models.
#'
#' @param mids mids object, a multiply-imputed dataset generated from mice
#' @param filename character vector, specify the name of the file (without extension)
#' @param outcome character vector, specifying the name of the response variable in the dataset
#' @param predictors1 character vector, predictors in Step 1 of regression
#' @param predictors2 character vector, predictors in Step 2 of regression
#' @param predictors3 (OPTIONAL) character vector, predictors in Step 3 of regression
#' @param filepath string, specify full path to output directory in the format
#' "~/directory/subdirectory/"
#'
#' @return
#' @export
#'
#' @importFrom texreg createTexreg htmlreg
#'
#' @examples
#' mids <- mice::mice(mice::nhanes, m=5, maxit=1)
#'
#' niceout(mids,"Hierarchical_regression_1",
#' outcome = "hyp",
#' predictors1 = c("age", "bmi"),
#' predictors2 = c("age", "bmi","chl"),
#' filepath = "D:/Desktop/")
niceout <- function (mids, filename, outcome, predictors1, predictors2, predictors3=NULL, filepath=NULL) {
  eq1 <- paste(outcome, " ~ ", paste(predictors1, collapse = " + "))
  regr1 <- with(mids, expr={
    lm(as.formula(eq1))
  })
  eq1s <- paste(paste0("scale(", outcome, ")"), " ~ ",
                paste0("scale(", predictors1, ")", collapse = " + "))
  regr1s <- with(mids, expr={
    lm(as.formula(eq1s))
  })

  pooled1 <- pool(regr1)
  pooled1s <- pool(regr1s)
  R1 <- mean(pooled1$glanced$r.squared)
  F1 <- Fru(mids, outcome, predictors1, predictors1)
  sum1 <- summary(pooled1)

  tex1 <- createTexreg(
    coef.names = as.character(pooled1$pooled$term),
    coef = sum1$estimate,
    se = sum1$std.error,
    pvalues = sum1$p.value,
    gof.names = c(paste0("R","\U00B2"," change"),
                  paste0("\U0394"," F"),
                  paste0("sig. F"),
                  paste0("df1 F"),
                  paste0("df2 F")),
    gof = c(R1, F1$stat$F, F1$stat$pval, round(F1$stat$df1), round(F1$stat$df2)),
    gof.decimal = c(T,T,T,F,F)
  )
  tex1s <- createTexreg(
    coef.names = as.character(pooled1$pooled$term),
    coef = pooled1s$pooled$estimate
  )
  tex1t <- createTexreg(
    coef.names = as.character(pooled1$pooled$term),
    coef = sum1$statistic,
    se = round(pooled1$pooled$df)
  )
  tex1p <- createTexreg(
    coef.names = as.character(pooled1$pooled$term),
    coef = sum1$p.value
  )


  eq2 <- paste(outcome, " ~ ", paste(predictors2, collapse = " + "))
  regr2 <- with(mids, expr={
    lm(as.formula(eq2))
  })
  eq2s <- paste(paste0("scale(", outcome, ")"), " ~ ",
                paste0("scale(", predictors2, ")", collapse = " + "))
  regr2s <- with(mids, expr={
    lm(as.formula(eq2s))
  })

  pooled2 <- pool(regr2)
  pooled2s <- pool(regr2s)
  R2 <- mean(pooled2$glanced$r.squared) - R1
  F2 <- Fru(mids, outcome, predictors2, setdiff(predictors2, predictors1))
  sum2 <- summary(pooled2)

  tex2 <- createTexreg(
    coef.names = as.character(pooled2$pooled$term),
    coef = sum2$estimate,
    se = sum2$std.error,
    pvalues = sum2$p.value,
    gof.names = c(paste0("R","\U00B2"," change"),
                  paste0("\U0394"," F"),
                  paste0("sig. F"),
                  paste0("df1 F"),
                  paste0("df2 F")),
    gof = c(R2, F2$stat$F, F2$stat$pval, round(F2$stat$df1), round(F2$stat$df2)),
    gof.decimal = c(T,T,T,F,F)
  )
  tex2s <- createTexreg(
    coef.names = as.character(pooled2$pooled$term),
    coef = pooled2s$pooled$estimate
  )
  tex2t <- createTexreg(
    coef.names = as.character(pooled2$pooled$term),
    coef = sum2$statistic,
    se = round(pooled2$pooled$df)
  )
  tex2p <- createTexreg(
    coef.names = as.character(pooled2$pooled$term),
    coef = sum2$p.value
  )

  if (is.null(predictors3)){
    if (is.null(filepath)){
      file_out <- paste0(filename, ".html")
    } else {
      file_out <- paste0(filepath, filename, ".html")
    }

    disp <- list(tex1,tex1s,tex1t,tex1p,tex2,tex2s,tex2t,tex2p)
    screen <- htmlreg(disp, file = file_out, digits = 3, single.row = F, leading.zero = F,
                      custom.header = list("Step 1: Demographics" = 1:4,
                                           "Step 2: + Performance EF" = 5:8),
                      custom.model.names = c("b", "Beta", "t(df)", "p",
                                             "b", "Beta", "t(df)", "p"),
                      omit.coef = "Intercept")
    return(screen)

  } else {
    eq3 <- paste(outcome, " ~ ", paste(predictors3, collapse = " + "))
    regr3 <- with(mids, expr={
      lm(as.formula(eq3))
    })
    eq3s <- paste(paste0("scale(", outcome, ")"), " ~ ",
                  paste0("scale(", predictors3, ")", collapse = " + "))
    regr3s <- with(mids, expr={
      lm(as.formula(eq3s))
    })

    pooled3 <- pool(regr3)
    pooled3s <- pool(regr3s)
    R3 <- mean(pooled3$glanced$r.squared) - R2 - R1
    F3 <- Fru(mids, outcome, predictors3, setdiff(predictors3, predictors2))
    sum3 <- summary(pooled3)

    tex3 <- createTexreg(
      coef.names = as.character(pooled3$pooled$term),
      coef = sum3$estimate,
      se = sum3$std.error,
      pvalues = sum3$p.value,
      gof.names = c(paste0("R","\U00B2"," change"),
                    paste0("\U0394"," F"),
                    paste0("sig. F"),
                    paste0("df1 F"),
                    paste0("df2 F")),
      gof = c(R3, F3$stat$F, F3$stat$pval, round(F3$stat$df1), round(F3$stat$df2)),
      gof.decimal = c(T,T,T,F,F)
    )
    tex3s <- createTexreg(
      coef.names = as.character(pooled3$pooled$term),
      coef = pooled3s$pooled$estimate
    )
    tex3t <- createTexreg(
      coef.names = as.character(pooled3$pooled$term),
      coef = sum3$statistic,
      se = round(pooled3$pooled$df)
    )
    tex3p <- createTexreg(
      coef.names = as.character(pooled3$pooled$term),
      coef = sum3$p.value
    )

    if (is.null(filepath)){
      file_out <- paste0(filename, ".html")
    } else {
      file_out <- paste0(filepath, filename, ".html")
    }
    disp <- list(tex1,tex1s,tex1t,tex1p,tex2,tex2s,tex2t,tex2p,tex3,tex3s,tex3t,tex3p)
    screen <- htmlreg(disp, file = file_out, digits = 3, single.row = F, leading.zero = F,
                      custom.header = list("Step 1: Demographics" = 1:4,
                                           "Step 2: + Performance EF" = 5:8,
                                           "Step 3: + Self-report EF" = 9:12),
                      custom.model.names = c("b", "Beta", "t(df)", "p",
                                             "b", "Beta", "t(df)", "p",
                                             "b", "Beta", "t(df)", "p"),
                      omit.coef = "Intercept")
    return(screen)
  }


}
