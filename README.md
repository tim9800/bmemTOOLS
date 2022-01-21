# bmemTOOLS
Complements the edited version of bmem, bmemEDIT. Works specifically with data from my honours thesis

# Fru()
Calculates a pooled F statistic for regressions conducted on multiply-imputed data. Incorporates a code snippet from van Ginkel (2019), see https://www.tandfonline.com/doi/full/10.1080/00273171.2018.1540967. The pooled F-statistic is included in calls to niceout().

# SRS_convert_T()
Converts raw SRS (Social Responsiveness Scale 2) scores into age-normed T scores.

# BRIEF_convert_T()
Converts raw BRIEF (Behavior Rating Inventory of Executive Function - Adult) scores into age-normed T scores.

# niceout()
Performs hierarchical regressions on multiply-imputed data. Depends on miceadds for analysis and texreg for regression table output. Currently supports up to three separate regression steps. Formatted regression tables are output in .html format and saved in a location of choice. Supported statistics include:
  - Standardised (beta) and unstandardised (b) regression coefficients
  - Standard error of the unstandardised regression coefficients
  - t-statistic and associated degrees of freedom
  - p-values for regression coefficients
  - R-square change
  - F-test for R-square change, with degrees of freedom. See Fru()
  - p-values for F-test
