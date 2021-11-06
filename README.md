# bmemTOOLS
Complements the edited version of bmem, bmemEDIT. Works specifically with data from my honours thesis

# Fru()
Incorporates a code snippet from van Ginkel (2019) to calculate a pooled F statistic for multiply-imputed data. Output is included in niceout()

# SRS_convert_T()
Converts SRS raw scores into T scores.

# BRIEF_convert_T()
Converts BRIEF raw scores into T scores.

# niceout()
Relies mainly on miceadds (analysis) and texreg (output formatting) to perform regressions on multiply-imputed data. Formatted regression tables are output in .html format and sent to a location of choice. Current statistics in the output are:
  - Standardised (beta) and unstandardised (b) regression coefficients
  - Standard error of the unstandardised regression coefficients
  - t-statistic, with degrees of freedom
  - p-values for regression coefficients
  - R-square change
  - F-test for R-square change, with degrees of freedom. See Fru()
  - p-values for F-test
