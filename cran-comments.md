Dear CRAN volunteers,

Thank you for reviewing this submission. It contains an update to the priorCON R
package. Specifically, the update contains assorted minor improvements, bug
fixes, and updates to the package documentation. It also addresses the NOTEs
currently produced during CRAN package checks related to missing documentation
links.

Best,

Christos Adam

## CRAN check notes

* Found the following (possibly) invalid URLs:
  URL: https://highs.dev
    From: man/basic_scenario.Rd
          man/connectivity_scenario.Rd
    Status: Error
    Message: Recv failure: Connection was reset

**This URL is checked manually and it works fine. So it is a valid URL.**

* checking DESCRIPTION meta-information ... NOTE
  Missing dependency on R >= 4.1.0 because package code uses the pipe
  |> or function shorthand \\(...) syntax added in R 4.1.0.
  File(s) using such syntax:
    'functions_connect.R'

**Added the following field in the DESCRIPTION:**

Depends:
    R (>= 4.1.0)

## R CMD check results

0 errors | 0 warnings | 1 notes
