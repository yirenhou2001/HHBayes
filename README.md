<<<<<<< HEAD
# Household.Transmission.Chain.Data.Analysis
=======

### {Household.Transmission.Chain.Data.Analysis}: Household Transmission Chain Simulation and Estimation

{Household.Transmission.Chain.Data.Analysis} helps you do two things in
a streamlined process:

1.  **Generate synthetic household data** and run the **full estimation
    pipeline** end-to-end with GenSyn().
2.  **Estimate transmission parameters from your own data** with
    TransmissionChainAnalysis().

Within this pipeline, the package:

- summarizes individuals and imputes infection timelines (latent
  periods, reporting delays, infectious periods);
- builds a person-day table suitable for likelihood-based estimation;
- fits penalized models for community and household risks with optional
  covariates (shared or role-specific);
- provides post-processing that reports mean estimates, uncertainty
  (standard error), bias, and relative bias;
- provides the option to summarize data by infection episodes per
  individual.

## Package purpose

This package lets user simulate household transmission data, estimate
parameters of interest, and compare results either against known “true”
values (specified for synthetic data simulation and analysis) or compare
parameter estimates of your own data (which may have missing values or
limited covariates) with parameter estimates of simulated synthetic data
for population of interest.

- **One call, full workflow**: simulate data/user data -\> summarize
  individuals -\> impute infection time -\> estimate parameters -\>
  post-process of estimates.
- **Emulates** user study’s structure (dates, testing frequency,
  covariates, missingness) to see how coefficient estimates shift
  relative to the “ideal” simulated situation.
- **Compares** to ground truth for simulations, post-processing reports
  bias and relative bias versus the known parameters.

## Quick start

``` r
# Install from source
devtools::install()
```

    ## 
    ## ── R CMD build ─────────────────────────────────────────────────────────────────
    ##      checking for file ‘/Users/yh785/Downloads/DSDE/Research/Dan and Ke - A Framework for Household Transmission Data/Household.Transmission.Chain.Data.Analysis/DESCRIPTION’ ...  ✔  checking for file ‘/Users/yh785/Downloads/DSDE/Research/Dan and Ke - A Framework for Household Transmission Data/Household.Transmission.Chain.Data.Analysis/DESCRIPTION’
    ##   ─  preparing ‘Household.Transmission.Chain.Data.Analysis’:
    ##    checking DESCRIPTION meta-information ...  ✔  checking DESCRIPTION meta-information
    ##   ─  checking for LF line-endings in source and make files and shell scripts
    ##   ─  checking for empty or unneeded directories
    ##   ─  building ‘Household.Transmission.Chain.Data.Analysis_0.0.0.9000.tar.gz’
    ##      Warning: invalid uid value replaced by that for user 'nobody'
    ##    Warning: invalid gid value replaced by that for user 'nobody'
    ##      
    ## Running /Library/Frameworks/R.framework/Resources/bin/R CMD INSTALL \
    ##   /var/folders/vc/ndslnt5n52361c8cdskwj0_ddknq6f/T//RtmpKRxdlP/Household.Transmission.Chain.Data.Analysis_0.0.0.9000.tar.gz \
    ##   --install-tests 
    ## * installing to library ‘/Users/yh785/Library/R/arm64/4.5/library’
    ## * installing *source* package ‘Household.Transmission.Chain.Data.Analysis’ ...
    ## ** this is package ‘Household.Transmission.Chain.Data.Analysis’ version ‘0.0.0.9000’
    ## ** using staged installation
    ## ** R
    ## ** byte-compile and prepare package for lazy loading
    ## ** help
    ## *** installing help indices
    ## ** building package indices
    ## ** installing vignettes
    ## ** testing if installed package can be loaded from temporary location
    ## ** testing if installed package can be loaded from final location
    ## ** testing if installed package keeps a record of temporary installation path
    ## * DONE (Household.Transmission.Chain.Data.Analysis)

``` r
library(Household.Transmission.Chain.Data.Analysis)
# 1) Simulate and estimate
out1 <- GenSyn(
  n_households = 10,
  n_runs       = 10,
  data_summary = TRUE
)
```

    ## Initialized start_par of length 8 (based on available covariates).

    ## 
    ## --- Data summary ---
    ## Index: <infected>
    ##     ID_hh indiv.index n.true.infection n.detected.infection
    ##     <int>       <int>            <int>                <int>
    ##  1:     1           1                1                    1
    ##  2:     1           2                1                    1
    ##  3:     1           3                1                    1
    ##  4:     1           4                1                    1
    ##  5:     1           5                1                    1
    ##  6:     1           6                1                    1
    ##  7:     1           7                1                    1
    ##  8:     2           1                1                    1
    ##  9:     2           2                1                    1
    ## 10:     2           3                1                    1
    ## 11:     2           4                1                    1
    ## 12:     2           5                1                    1
    ## 13:     2           6                1                    1
    ## 14:     2           7                1                    1
    ## 15:     3           1                1                    1
    ## 16:     3           2                1                    1
    ## 17:     3           3                1                    1
    ## 18:     3           4                1                    1
    ## 19:     3           5                1                    1
    ## 20:     3           6                1                    1
    ## 21:     3           7                1                    1
    ## 22:     4           1                1                    1
    ## 23:     4           2                1                    1
    ## 24:     4           3                1                    1
    ## 25:     4           4                1                    1
    ## 26:     4           5                1                    1
    ## 27:     4           6                1                    1
    ## 28:     4           7                1                    1
    ## 29:     5           1                1                    1
    ## 30:     5           2                1                    1
    ## 31:     5           3                1                    1
    ## 32:     5           4                1                    1
    ## 33:     5           5                1                    1
    ## 34:     5           6                1                    1
    ## 35:     5           7                1                    1
    ## 36:     6           1                1                    1
    ## 37:     6           2                1                    1
    ## 38:     6           3                1                    1
    ## 39:     6           4                1                    1
    ## 40:     6           5                1                    1
    ## 41:     6           6                1                    1
    ## 42:     6           7                1                    1
    ## 43:     7           1                1                    1
    ## 44:     7           2                1                    1
    ## 45:     7           3                1                    1
    ## 46:     7           4                1                    1
    ## 47:     7           5                1                    1
    ## 48:     7           6                1                    1
    ## 49:     7           7                1                    1
    ## 50:     8           1                1                    1
    ## 51:     8           2                1                    1
    ## 52:     8           3                1                    1
    ## 53:     8           4                1                    1
    ## 54:     8           5                1                    1
    ## 55:     8           6                1                    1
    ## 56:     8           7                1                    1
    ## 57:     9           1                1                    1
    ## 58:     9           2                1                    1
    ## 59:     9           3                1                    1
    ## 60:     9           4                1                    1
    ## 61:     9           5                1                    1
    ## 62:     9           6                1                    1
    ## 63:     9           7                1                    1
    ## 64:    10           1                1                    1
    ## 65:    10           2                1                    1
    ## 66:    10           3                1                    1
    ## 67:    10           4                1                    1
    ## 68:    10           5                1                    1
    ## 69:    10           6                1                    1
    ## 70:    10           7                1                    1
    ##     ID_hh indiv.index n.true.infection n.detected.infection
    ##     infection.detected.start infection.detected.end infection.true.duration
    ##                        <int>                  <int>                   <num>
    ##  1:                       67                     84                      18
    ##  2:                       64                     67                       4
    ##  3:                       66                     73                       8
    ##  4:                       68                     75                       8
    ##  5:                       83                     90                       8
    ##  6:                       66                     73                       8
    ##  7:                       65                     72                       8
    ##  8:                      136                    173                      38
    ##  9:                      131                    138                       8
    ## 10:                      138                    145                       8
    ## 11:                      132                    139                       8
    ## 12:                      139                    146                       8
    ## 13:                      127                    133                       7
    ## 14:                      131                    138                       8
    ## 15:                       12                     34                      23
    ## 16:                       17                     24                       8
    ## 17:                       13                     20                       8
    ## 18:                       19                     26                       8
    ## 19:                       18                     25                       8
    ## 20:                       12                     19                       8
    ## 21:                        8                     14                       7
    ## 22:                       22                     37                      16
    ## 23:                       32                     39                       8
    ## 24:                       16                     23                       8
    ## 25:                       15                     18                       4
    ## 26:                       29                     36                       8
    ## 27:                       21                     28                       8
    ## 28:                       23                     30                       8
    ## 29:                       31                     88                      58
    ## 30:                       22                     26                       5
    ## 31:                       36                     43                       8
    ## 32:                       29                     36                       8
    ## 33:                       24                     31                       8
    ## 34:                       30                     37                       8
    ## 35:                       24                     31                       8
    ## 36:                       36                     53                      18
    ## 37:                       41                     48                       8
    ## 38:                       38                     45                       8
    ## 39:                       37                     44                       8
    ## 40:                       43                     50                       8
    ## 41:                       39                     46                       8
    ## 42:                       45                     52                       8
    ## 43:                       14                     59                      46
    ## 44:                        8                     12                       5
    ## 45:                       11                     18                       8
    ## 46:                       18                     25                       8
    ## 47:                       20                     27                       8
    ## 48:                       11                     18                       8
    ## 49:                       16                     23                       8
    ## 50:                       91                    127                      37
    ## 51:                       96                    103                       8
    ## 52:                       96                    103                       8
    ## 53:                       85                     91                       7
    ## 54:                       96                    103                       8
    ## 55:                       98                    105                       8
    ## 56:                       92                     99                       8
    ## 57:                       43                     74                      32
    ## 58:                       49                     56                       8
    ## 59:                       53                     60                       8
    ## 60:                       53                     60                       8
    ## 61:                       48                     55                       8
    ## 62:                       52                     59                       8
    ## 63:                       53                     60                       8
    ## 64:                       91                    128                      38
    ## 65:                       89                     96                       8
    ## 66:                       89                     96                       8
    ## 67:                       96                    103                       8
    ## 68:                       83                     90                       8
    ## 69:                       97                    104                       8
    ## 70:                       78                     83                       6
    ##     infection.detected.start infection.detected.end infection.true.duration
    ##     last_negative
    ##             <int>
    ##  1:            66
    ##  2:            57
    ##  3:            65
    ##  4:            67
    ##  5:            82
    ##  6:            65
    ##  7:            64
    ##  8:           135
    ##  9:           130
    ## 10:           137
    ## 11:           131
    ## 12:           138
    ## 13:           120
    ## 14:           130
    ## 15:            11
    ## 16:            16
    ## 17:            12
    ## 18:            18
    ## 19:            17
    ## 20:            11
    ## 21:             1
    ## 22:            21
    ## 23:            31
    ## 24:            15
    ## 25:             8
    ## 26:            28
    ## 27:            20
    ## 28:            22
    ## 29:            30
    ## 30:            15
    ## 31:            35
    ## 32:            28
    ## 33:            23
    ## 34:            29
    ## 35:            23
    ## 36:            29
    ## 37:            40
    ## 38:            37
    ## 39:            36
    ## 40:            42
    ## 41:            38
    ## 42:            44
    ## 43:            13
    ## 44:             1
    ## 45:            10
    ## 46:            17
    ## 47:            19
    ## 48:            10
    ## 49:            15
    ## 50:            90
    ## 51:            95
    ## 52:            95
    ## 53:            78
    ## 54:            95
    ## 55:            97
    ## 56:            91
    ## 57:            36
    ## 58:            48
    ## 59:            52
    ## 60:            52
    ## 61:            47
    ## 62:            51
    ## 63:            52
    ## 64:            90
    ## 65:            88
    ## 66:            88
    ## 67:            95
    ## 68:            82
    ## 69:            96
    ## 70:            71
    ##     last_negative
    ##                                                                                                                                                          infection.infectious.day
    ##                                                                                                                                                                            <char>
    ##  1:                                                                                                                         67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84
    ##  2:                                                                                                                                                                   64,65,66,67
    ##  3:                                                                                                                                                       66,67,68,69,70,71,72,73
    ##  4:                                                                                                                                                       68,69,70,71,72,73,74,75
    ##  5:                                                                                                                                                       83,84,85,86,87,88,89,90
    ##  6:                                                                                                                                                       66,67,68,69,70,71,72,73
    ##  7:                                                                                                                                                       65,66,67,68,69,70,71,72
    ##  8:                       136,137,138,139,140,141,142,143,144,145,146,147,148,149,150,151,152,153,154,155,156,157,158,159,160,161,162,163,164,165,166,167,168,169,170,171,172,173
    ##  9:                                                                                                                                               131,132,133,134,135,136,137,138
    ## 10:                                                                                                                                               138,139,140,141,142,143,144,145
    ## 11:                                                                                                                                               132,133,134,135,136,137,138,139
    ## 12:                                                                                                                                               139,140,141,142,143,144,145,146
    ## 13:                                                                                                                                                   127,128,129,130,131,132,133
    ## 14:                                                                                                                                               131,132,133,134,135,136,137,138
    ## 15:                                                                                                          12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34
    ## 16:                                                                                                                                                       17,18,19,20,21,22,23,24
    ## 17:                                                                                                                                                       13,14,15,16,17,18,19,20
    ## 18:                                                                                                                                                       19,20,21,22,23,24,25,26
    ## 19:                                                                                                                                                       18,19,20,21,22,23,24,25
    ## 20:                                                                                                                                                       12,13,14,15,16,17,18,19
    ## 21:                                                                                                                                                            8,9,10,11,12,13,14
    ## 22:                                                                                                                               22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37
    ## 23:                                                                                                                                                       32,33,34,35,36,37,38,39
    ## 24:                                                                                                                                                       16,17,18,19,20,21,22,23
    ## 25:                                                                                                                                                                   15,16,17,18
    ## 26:                                                                                                                                                       29,30,31,32,33,34,35,36
    ## 27:                                                                                                                                                       21,22,23,24,25,26,27,28
    ## 28:                                                                                                                                                       23,24,25,26,27,28,29,30
    ## 29: 31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88
    ## 30:                                                                                                                                                                22,23,24,25,26
    ## 31:                                                                                                                                                       36,37,38,39,40,41,42,43
    ## 32:                                                                                                                                                       29,30,31,32,33,34,35,36
    ## 33:                                                                                                                                                       24,25,26,27,28,29,30,31
    ## 34:                                                                                                                                                       30,31,32,33,34,35,36,37
    ## 35:                                                                                                                                                       24,25,26,27,28,29,30,31
    ## 36:                                                                                                                         36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53
    ## 37:                                                                                                                                                       41,42,43,44,45,46,47,48
    ## 38:                                                                                                                                                       38,39,40,41,42,43,44,45
    ## 39:                                                                                                                                                       37,38,39,40,41,42,43,44
    ## 40:                                                                                                                                                       43,44,45,46,47,48,49,50
    ## 41:                                                                                                                                                       39,40,41,42,43,44,45,46
    ## 42:                                                                                                                                                       45,46,47,48,49,50,51,52
    ## 43:                                     14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59
    ## 44:                                                                                                                                                                  8,9,10,11,12
    ## 45:                                                                                                                                                       11,12,13,14,15,16,17,18
    ## 46:                                                                                                                                                       18,19,20,21,22,23,24,25
    ## 47:                                                                                                                                                       20,21,22,23,24,25,26,27
    ## 48:                                                                                                                                                       11,12,13,14,15,16,17,18
    ## 49:                                                                                                                                                       16,17,18,19,20,21,22,23
    ## 50:                                    91,92,93,94,95,96,97,98,99,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,118,119,120,121,122,123,124,125,126,127
    ## 51:                                                                                                                                                   96,97,98,99,100,101,102,103
    ## 52:                                                                                                                                                   96,97,98,99,100,101,102,103
    ## 53:                                                                                                                                                          85,86,87,88,89,90,91
    ## 54:                                                                                                                                                   96,97,98,99,100,101,102,103
    ## 55:                                                                                                                                                 98,99,100,101,102,103,104,105
    ## 56:                                                                                                                                                       92,93,94,95,96,97,98,99
    ## 57:                                                                               43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74
    ## 58:                                                                                                                                                       49,50,51,52,53,54,55,56
    ## 59:                                                                                                                                                       53,54,55,56,57,58,59,60
    ## 60:                                                                                                                                                       53,54,55,56,57,58,59,60
    ## 61:                                                                                                                                                       48,49,50,51,52,53,54,55
    ## 62:                                                                                                                                                       52,53,54,55,56,57,58,59
    ## 63:                                                                                                                                                       53,54,55,56,57,58,59,60
    ## 64:                                91,92,93,94,95,96,97,98,99,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,118,119,120,121,122,123,124,125,126,127,128
    ## 65:                                                                                                                                                       89,90,91,92,93,94,95,96
    ## 66:                                                                                                                                                       89,90,91,92,93,94,95,96
    ## 67:                                                                                                                                                   96,97,98,99,100,101,102,103
    ## 68:                                                                                                                                                       83,84,85,86,87,88,89,90
    ## 69:                                                                                                                                                  97,98,99,100,101,102,103,104
    ## 70:                                                                                                                                                             78,79,80,81,82,83
    ##                                                                                                                                                          infection.infectious.day
    ##     community.risk    role  T_FP_date  T_LP_date last_neg_date inf_date
    ##              <num>  <char>     <Date>     <Date>        <Date>   <Date>
    ##  1:          0.002  infant 2024-11-27 2024-12-14    2024-11-26     <NA>
    ##  2:          0.002   adult 2024-11-24 2024-11-27    2024-11-17     <NA>
    ##  3:          0.002   adult 2024-11-26 2024-12-03    2024-11-25     <NA>
    ##  4:          0.002 sibling 2024-11-28 2024-12-05    2024-11-27     <NA>
    ##  5:          0.002 sibling 2024-12-13 2024-12-20    2024-12-12     <NA>
    ##  6:          0.002 sibling 2024-11-26 2024-12-03    2024-11-25     <NA>
    ##  7:          0.002   elder 2024-11-25 2024-12-02    2024-11-24     <NA>
    ##  8:          0.002  infant 2025-02-04 2025-03-13    2025-02-03     <NA>
    ##  9:          0.002   adult 2025-01-30 2025-02-06    2025-01-29     <NA>
    ## 10:          0.002   adult 2025-02-06 2025-02-13    2025-02-05     <NA>
    ## 11:          0.002 sibling 2025-01-31 2025-02-07    2025-01-30     <NA>
    ## 12:          0.002 sibling 2025-02-07 2025-02-14    2025-02-06     <NA>
    ## 13:          0.002 sibling 2025-01-26 2025-02-01    2025-01-19     <NA>
    ## 14:          0.002 sibling 2025-01-30 2025-02-06    2025-01-29     <NA>
    ## 15:          0.002  infant 2024-10-03 2024-10-25    2024-10-02     <NA>
    ## 16:          0.002   adult 2024-10-08 2024-10-15    2024-10-07     <NA>
    ## 17:          0.002   adult 2024-10-04 2024-10-11    2024-10-03     <NA>
    ## 18:          0.002 sibling 2024-10-10 2024-10-17    2024-10-09     <NA>
    ## 19:          0.002 sibling 2024-10-09 2024-10-16    2024-10-08     <NA>
    ## 20:          0.002   elder 2024-10-03 2024-10-10    2024-10-02     <NA>
    ## 21:          0.002   elder 2024-09-29 2024-10-05    2024-09-22     <NA>
    ## 22:          0.002  infant 2024-10-13 2024-10-28    2024-10-12     <NA>
    ## 23:          0.002   adult 2024-10-23 2024-10-30    2024-10-22     <NA>
    ## 24:          0.002   adult 2024-10-07 2024-10-14    2024-10-06     <NA>
    ## 25:          0.002 sibling 2024-10-06 2024-10-09    2024-09-29     <NA>
    ## 26:          0.002 sibling 2024-10-20 2024-10-27    2024-10-19     <NA>
    ## 27:          0.002 sibling 2024-10-12 2024-10-19    2024-10-11     <NA>
    ## 28:          0.002 sibling 2024-10-14 2024-10-21    2024-10-13     <NA>
    ## 29:          0.002  infant 2024-10-22 2024-12-18    2024-10-21     <NA>
    ## 30:          0.002   adult 2024-10-13 2024-10-17    2024-10-06     <NA>
    ## 31:          0.002   adult 2024-10-27 2024-11-03    2024-10-26     <NA>
    ## 32:          0.002 sibling 2024-10-20 2024-10-27    2024-10-19     <NA>
    ## 33:          0.002 sibling 2024-10-15 2024-10-22    2024-10-14     <NA>
    ## 34:          0.002 sibling 2024-10-21 2024-10-28    2024-10-20     <NA>
    ## 35:          0.002   elder 2024-10-15 2024-10-22    2024-10-14     <NA>
    ## 36:          0.002  infant 2024-10-27 2024-11-13    2024-10-20     <NA>
    ## 37:          0.002   adult 2024-11-01 2024-11-08    2024-10-31     <NA>
    ## 38:          0.002   adult 2024-10-29 2024-11-05    2024-10-28     <NA>
    ## 39:          0.002 sibling 2024-10-28 2024-11-04    2024-10-27     <NA>
    ## 40:          0.002 sibling 2024-11-03 2024-11-10    2024-11-02     <NA>
    ## 41:          0.002 sibling 2024-10-30 2024-11-06    2024-10-29     <NA>
    ## 42:          0.002 sibling 2024-11-05 2024-11-12    2024-11-04     <NA>
    ## 43:          0.002  infant 2024-10-05 2024-11-19    2024-10-04     <NA>
    ## 44:          0.002   adult 2024-09-29 2024-10-03    2024-09-22     <NA>
    ## 45:          0.002   adult 2024-10-02 2024-10-09    2024-10-01     <NA>
    ## 46:          0.002 sibling 2024-10-09 2024-10-16    2024-10-08     <NA>
    ## 47:          0.002 sibling 2024-10-11 2024-10-18    2024-10-10     <NA>
    ## 48:          0.002   elder 2024-10-02 2024-10-09    2024-10-01     <NA>
    ## 49:          0.002   elder 2024-10-07 2024-10-14    2024-10-06     <NA>
    ## 50:          0.002  infant 2024-12-21 2025-01-26    2024-12-20     <NA>
    ## 51:          0.002   adult 2024-12-26 2025-01-02    2024-12-25     <NA>
    ## 52:          0.002   adult 2024-12-26 2025-01-02    2024-12-25     <NA>
    ## 53:          0.002 sibling 2024-12-15 2024-12-21    2024-12-08     <NA>
    ## 54:          0.002 sibling 2024-12-26 2025-01-02    2024-12-25     <NA>
    ## 55:          0.002   elder 2024-12-28 2025-01-04    2024-12-27     <NA>
    ## 56:          0.002   elder 2024-12-22 2024-12-29    2024-12-21     <NA>
    ## 57:          0.002  infant 2024-11-03 2024-12-04    2024-10-27     <NA>
    ## 58:          0.002   adult 2024-11-09 2024-11-16    2024-11-08     <NA>
    ## 59:          0.002   adult 2024-11-13 2024-11-20    2024-11-12     <NA>
    ## 60:          0.002 sibling 2024-11-13 2024-11-20    2024-11-12     <NA>
    ## 61:          0.002 sibling 2024-11-08 2024-11-15    2024-11-07     <NA>
    ## 62:          0.002 sibling 2024-11-12 2024-11-19    2024-11-11     <NA>
    ## 63:          0.002 sibling 2024-11-13 2024-11-20    2024-11-12     <NA>
    ## 64:          0.002  infant 2024-12-21 2025-01-27    2024-12-20     <NA>
    ## 65:          0.002   adult 2024-12-19 2024-12-26    2024-12-18     <NA>
    ## 66:          0.002   adult 2024-12-19 2024-12-26    2024-12-18     <NA>
    ## 67:          0.002 sibling 2024-12-26 2025-01-02    2024-12-25     <NA>
    ## 68:          0.002 sibling 2024-12-13 2024-12-20    2024-12-12     <NA>
    ## 69:          0.002 sibling 2024-12-27 2025-01-03    2024-12-26     <NA>
    ## 70:          0.002   elder 2024-12-08 2024-12-13    2024-12-01     <NA>
    ##     community.risk    role  T_FP_date  T_LP_date last_neg_date inf_date
    ##     inf_start_date inf_end_date inf_win_start inf_win_end infected is_index
    ##             <Date>       <Date>         <int>       <int>   <lgcl>   <lgcl>
    ##  1:     2024-11-27   2024-12-14            67          84     TRUE    FALSE
    ##  2:     2024-11-24   2024-11-27            64          67     TRUE     TRUE
    ##  3:     2024-11-26   2024-12-03            66          73     TRUE    FALSE
    ##  4:     2024-11-28   2024-12-05            68          75     TRUE    FALSE
    ##  5:     2024-12-13   2024-12-20            83          90     TRUE    FALSE
    ##  6:     2024-11-26   2024-12-03            66          73     TRUE    FALSE
    ##  7:     2024-11-25   2024-12-02            65          72     TRUE    FALSE
    ##  8:     2025-02-04   2025-03-13           136         173     TRUE    FALSE
    ##  9:     2025-01-30   2025-02-06           131         138     TRUE    FALSE
    ## 10:     2025-02-06   2025-02-13           138         145     TRUE    FALSE
    ## 11:     2025-01-31   2025-02-07           132         139     TRUE    FALSE
    ## 12:     2025-02-07   2025-02-14           139         146     TRUE    FALSE
    ## 13:     2025-01-26   2025-02-01           127         133     TRUE     TRUE
    ## 14:     2025-01-30   2025-02-06           131         138     TRUE    FALSE
    ## 15:     2024-10-03   2024-10-25            12          34     TRUE    FALSE
    ## 16:     2024-10-08   2024-10-15            17          24     TRUE    FALSE
    ## 17:     2024-10-04   2024-10-11            13          20     TRUE    FALSE
    ## 18:     2024-10-10   2024-10-17            19          26     TRUE    FALSE
    ## 19:     2024-10-09   2024-10-16            18          25     TRUE    FALSE
    ## 20:     2024-10-03   2024-10-10            12          19     TRUE    FALSE
    ## 21:     2024-09-29   2024-10-05             8          14     TRUE     TRUE
    ## 22:     2024-10-13   2024-10-28            22          37     TRUE    FALSE
    ## 23:     2024-10-23   2024-10-30            32          39     TRUE    FALSE
    ## 24:     2024-10-07   2024-10-14            16          23     TRUE    FALSE
    ## 25:     2024-10-06   2024-10-09            15          18     TRUE     TRUE
    ## 26:     2024-10-20   2024-10-27            29          36     TRUE    FALSE
    ## 27:     2024-10-12   2024-10-19            21          28     TRUE    FALSE
    ## 28:     2024-10-14   2024-10-21            23          30     TRUE    FALSE
    ## 29:     2024-10-22   2024-12-18            31          88     TRUE    FALSE
    ## 30:     2024-10-13   2024-10-17            22          26     TRUE     TRUE
    ## 31:     2024-10-27   2024-11-03            36          43     TRUE    FALSE
    ## 32:     2024-10-20   2024-10-27            29          36     TRUE    FALSE
    ## 33:     2024-10-15   2024-10-22            24          31     TRUE    FALSE
    ## 34:     2024-10-21   2024-10-28            30          37     TRUE    FALSE
    ## 35:     2024-10-15   2024-10-22            24          31     TRUE    FALSE
    ## 36:     2024-10-27   2024-11-13            36          53     TRUE     TRUE
    ## 37:     2024-11-01   2024-11-08            41          48     TRUE    FALSE
    ## 38:     2024-10-29   2024-11-05            38          45     TRUE    FALSE
    ## 39:     2024-10-28   2024-11-04            37          44     TRUE    FALSE
    ## 40:     2024-11-03   2024-11-10            43          50     TRUE    FALSE
    ## 41:     2024-10-30   2024-11-06            39          46     TRUE    FALSE
    ## 42:     2024-11-05   2024-11-12            45          52     TRUE    FALSE
    ## 43:     2024-10-05   2024-11-19            14          59     TRUE    FALSE
    ## 44:     2024-09-29   2024-10-03             8          12     TRUE     TRUE
    ## 45:     2024-10-02   2024-10-09            11          18     TRUE    FALSE
    ## 46:     2024-10-09   2024-10-16            18          25     TRUE    FALSE
    ## 47:     2024-10-11   2024-10-18            20          27     TRUE    FALSE
    ## 48:     2024-10-02   2024-10-09            11          18     TRUE    FALSE
    ## 49:     2024-10-07   2024-10-14            16          23     TRUE    FALSE
    ## 50:     2024-12-21   2025-01-26            91         127     TRUE    FALSE
    ## 51:     2024-12-26   2025-01-02            96         103     TRUE    FALSE
    ## 52:     2024-12-26   2025-01-02            96         103     TRUE    FALSE
    ## 53:     2024-12-15   2024-12-21            85          91     TRUE     TRUE
    ## 54:     2024-12-26   2025-01-02            96         103     TRUE    FALSE
    ## 55:     2024-12-28   2025-01-04            98         105     TRUE    FALSE
    ## 56:     2024-12-22   2024-12-29            92          99     TRUE    FALSE
    ## 57:     2024-11-03   2024-12-04            43          74     TRUE     TRUE
    ## 58:     2024-11-09   2024-11-16            49          56     TRUE    FALSE
    ## 59:     2024-11-13   2024-11-20            53          60     TRUE    FALSE
    ## 60:     2024-11-13   2024-11-20            53          60     TRUE    FALSE
    ## 61:     2024-11-08   2024-11-15            48          55     TRUE    FALSE
    ## 62:     2024-11-12   2024-11-19            52          59     TRUE    FALSE
    ## 63:     2024-11-13   2024-11-20            53          60     TRUE    FALSE
    ## 64:     2024-12-21   2025-01-27            91         128     TRUE    FALSE
    ## 65:     2024-12-19   2024-12-26            89          96     TRUE    FALSE
    ## 66:     2024-12-19   2024-12-26            89          96     TRUE    FALSE
    ## 67:     2024-12-26   2025-01-02            96         103     TRUE    FALSE
    ## 68:     2024-12-13   2024-12-20            83          90     TRUE    FALSE
    ## 69:     2024-12-27   2025-01-03            97         104     TRUE    FALSE
    ## 70:     2024-12-08   2024-12-13            78          83     TRUE     TRUE
    ##     inf_start_date inf_end_date inf_win_start inf_win_end infected is_index
    ##     age_cat inf_day_rl infectious_day_rl infectious_end_day_rl ID_indiv
    ##       <int>      <int>             <int>                 <int>   <char>
    ##  1:       1         NA                67                    84 HH001_01
    ##  2:       3         NA                64                    67 HH001_02
    ##  3:       3         NA                66                    73 HH001_03
    ##  4:       2         NA                68                    75 HH001_04
    ##  5:       2         NA                83                    90 HH001_05
    ##  6:       2         NA                66                    73 HH001_06
    ##  7:       4         NA                65                    72 HH001_07
    ##  8:       1         NA               136                   173 HH002_01
    ##  9:       3         NA               131                   138 HH002_02
    ## 10:       3         NA               138                   145 HH002_03
    ## 11:       2         NA               132                   139 HH002_04
    ## 12:       2         NA               139                   146 HH002_05
    ## 13:       2         NA               127                   133 HH002_06
    ## 14:       2         NA               131                   138 HH002_07
    ## 15:       1         NA                12                    34 HH003_01
    ## 16:       3         NA                17                    24 HH003_02
    ## 17:       3         NA                13                    20 HH003_03
    ## 18:       2         NA                19                    26 HH003_04
    ## 19:       2         NA                18                    25 HH003_05
    ## 20:       4         NA                12                    19 HH003_06
    ## 21:       4         NA                 8                    14 HH003_07
    ## 22:       1         NA                22                    37 HH004_01
    ## 23:       3         NA                32                    39 HH004_02
    ## 24:       3         NA                16                    23 HH004_03
    ## 25:       2         NA                15                    18 HH004_04
    ## 26:       2         NA                29                    36 HH004_05
    ## 27:       2         NA                21                    28 HH004_06
    ## 28:       2         NA                23                    30 HH004_07
    ## 29:       1         NA                31                    88 HH005_01
    ## 30:       3         NA                22                    26 HH005_02
    ## 31:       3         NA                36                    43 HH005_03
    ## 32:       2         NA                29                    36 HH005_04
    ## 33:       2         NA                24                    31 HH005_05
    ## 34:       2         NA                30                    37 HH005_06
    ## 35:       4         NA                24                    31 HH005_07
    ## 36:       1         NA                36                    53 HH006_01
    ## 37:       3         NA                41                    48 HH006_02
    ## 38:       3         NA                38                    45 HH006_03
    ## 39:       2         NA                37                    44 HH006_04
    ## 40:       2         NA                43                    50 HH006_05
    ## 41:       2         NA                39                    46 HH006_06
    ## 42:       2         NA                45                    52 HH006_07
    ## 43:       1         NA                14                    59 HH007_01
    ## 44:       3         NA                 8                    12 HH007_02
    ## 45:       3         NA                11                    18 HH007_03
    ## 46:       2         NA                18                    25 HH007_04
    ## 47:       2         NA                20                    27 HH007_05
    ## 48:       4         NA                11                    18 HH007_06
    ## 49:       4         NA                16                    23 HH007_07
    ## 50:       1         NA                91                   127 HH008_01
    ## 51:       3         NA                96                   103 HH008_02
    ## 52:       3         NA                96                   103 HH008_03
    ## 53:       2         NA                85                    91 HH008_04
    ## 54:       2         NA                96                   103 HH008_05
    ## 55:       4         NA                98                   105 HH008_06
    ## 56:       4         NA                92                    99 HH008_07
    ## 57:       1         NA                43                    74 HH009_01
    ## 58:       3         NA                49                    56 HH009_02
    ## 59:       3         NA                53                    60 HH009_03
    ## 60:       2         NA                53                    60 HH009_04
    ## 61:       2         NA                48                    55 HH009_05
    ## 62:       2         NA                52                    59 HH009_06
    ## 63:       2         NA                53                    60 HH009_07
    ## 64:       1         NA                91                   128 HH010_01
    ## 65:       3         NA                89                    96 HH010_02
    ## 66:       3         NA                89                    96 HH010_03
    ## 67:       2         NA                96                   103 HH010_04
    ## 68:       2         NA                83                    90 HH010_05
    ## 69:       2         NA                97                   104 HH010_06
    ## 70:       4         NA                78                    83 HH010_07
    ##     age_cat inf_day_rl infectious_day_rl infectious_end_day_rl ID_indiv
    ##     obs_start_date obs_end_date latent_delay report_delay infect_period
    ##             <Date>       <Date>       <lgcl>       <lgcl>        <lgcl>
    ##  1:     2024-09-21   2025-04-17           NA           NA            NA
    ##  2:     2024-09-21   2025-04-17           NA           NA            NA
    ##  3:     2024-09-21   2025-04-17           NA           NA            NA
    ##  4:     2024-09-21   2025-04-17           NA           NA            NA
    ##  5:     2024-09-21   2025-04-17           NA           NA            NA
    ##  6:     2024-09-21   2025-04-17           NA           NA            NA
    ##  7:     2024-09-21   2025-04-17           NA           NA            NA
    ##  8:     2024-09-21   2025-04-17           NA           NA            NA
    ##  9:     2024-09-21   2025-04-17           NA           NA            NA
    ## 10:     2024-09-21   2025-04-17           NA           NA            NA
    ## 11:     2024-09-21   2025-04-17           NA           NA            NA
    ## 12:     2024-09-21   2025-04-17           NA           NA            NA
    ## 13:     2024-09-21   2025-04-17           NA           NA            NA
    ## 14:     2024-09-21   2025-04-17           NA           NA            NA
    ## 15:     2024-09-21   2025-04-17           NA           NA            NA
    ## 16:     2024-09-21   2025-04-17           NA           NA            NA
    ## 17:     2024-09-21   2025-04-17           NA           NA            NA
    ## 18:     2024-09-21   2025-04-17           NA           NA            NA
    ## 19:     2024-09-21   2025-04-17           NA           NA            NA
    ## 20:     2024-09-21   2025-04-17           NA           NA            NA
    ## 21:     2024-09-21   2025-04-17           NA           NA            NA
    ## 22:     2024-09-21   2025-04-17           NA           NA            NA
    ## 23:     2024-09-21   2025-04-17           NA           NA            NA
    ## 24:     2024-09-21   2025-04-17           NA           NA            NA
    ## 25:     2024-09-21   2025-04-17           NA           NA            NA
    ## 26:     2024-09-21   2025-04-17           NA           NA            NA
    ## 27:     2024-09-21   2025-04-17           NA           NA            NA
    ## 28:     2024-09-21   2025-04-17           NA           NA            NA
    ## 29:     2024-09-21   2025-04-17           NA           NA            NA
    ## 30:     2024-09-21   2025-04-17           NA           NA            NA
    ## 31:     2024-09-21   2025-04-17           NA           NA            NA
    ## 32:     2024-09-21   2025-04-17           NA           NA            NA
    ## 33:     2024-09-21   2025-04-17           NA           NA            NA
    ## 34:     2024-09-21   2025-04-17           NA           NA            NA
    ## 35:     2024-09-21   2025-04-17           NA           NA            NA
    ## 36:     2024-09-21   2025-04-17           NA           NA            NA
    ## 37:     2024-09-21   2025-04-17           NA           NA            NA
    ## 38:     2024-09-21   2025-04-17           NA           NA            NA
    ## 39:     2024-09-21   2025-04-17           NA           NA            NA
    ## 40:     2024-09-21   2025-04-17           NA           NA            NA
    ## 41:     2024-09-21   2025-04-17           NA           NA            NA
    ## 42:     2024-09-21   2025-04-17           NA           NA            NA
    ## 43:     2024-09-21   2025-04-17           NA           NA            NA
    ## 44:     2024-09-21   2025-04-17           NA           NA            NA
    ## 45:     2024-09-21   2025-04-17           NA           NA            NA
    ## 46:     2024-09-21   2025-04-17           NA           NA            NA
    ## 47:     2024-09-21   2025-04-17           NA           NA            NA
    ## 48:     2024-09-21   2025-04-17           NA           NA            NA
    ## 49:     2024-09-21   2025-04-17           NA           NA            NA
    ## 50:     2024-09-21   2025-04-17           NA           NA            NA
    ## 51:     2024-09-21   2025-04-17           NA           NA            NA
    ## 52:     2024-09-21   2025-04-17           NA           NA            NA
    ## 53:     2024-09-21   2025-04-17           NA           NA            NA
    ## 54:     2024-09-21   2025-04-17           NA           NA            NA
    ## 55:     2024-09-21   2025-04-17           NA           NA            NA
    ## 56:     2024-09-21   2025-04-17           NA           NA            NA
    ## 57:     2024-09-21   2025-04-17           NA           NA            NA
    ## 58:     2024-09-21   2025-04-17           NA           NA            NA
    ## 59:     2024-09-21   2025-04-17           NA           NA            NA
    ## 60:     2024-09-21   2025-04-17           NA           NA            NA
    ## 61:     2024-09-21   2025-04-17           NA           NA            NA
    ## 62:     2024-09-21   2025-04-17           NA           NA            NA
    ## 63:     2024-09-21   2025-04-17           NA           NA            NA
    ## 64:     2024-09-21   2025-04-17           NA           NA            NA
    ## 65:     2024-09-21   2025-04-17           NA           NA            NA
    ## 66:     2024-09-21   2025-04-17           NA           NA            NA
    ## 67:     2024-09-21   2025-04-17           NA           NA            NA
    ## 68:     2024-09-21   2025-04-17           NA           NA            NA
    ## 69:     2024-09-21   2025-04-17           NA           NA            NA
    ## 70:     2024-09-21   2025-04-17           NA           NA            NA
    ##     obs_start_date obs_end_date latent_delay report_delay infect_period
    ## 
    ## --- Post-processing of estimates ---
    ##    Parameter  Estimate           SD           SE       True      Bias
    ##       <char>     <num>        <num>        <num>      <num>     <num>
    ## 1:    delta0 -8.019918 1.924534e-04 6.085911e-05 -9.5460625  1.526144
    ## 2:    gamma2 -3.356002 4.848886e-04 1.533352e-04  1.4660192 -4.822021
    ## 3:    gamma3 -2.977057 7.241934e-04 2.290101e-04  0.6072984 -3.584356
    ## 4:    gamma4 -2.333142 1.878326e-03 5.939787e-04  0.6931472 -3.026290
    ## 5:    alpha0 -4.110231 2.641276e-05 8.352447e-06 -1.2416909 -2.868540
    ## 6:     z_sib -5.255642 6.207888e-04 1.963107e-04         NA        NA
    ## 7:      z_ad -4.859477 5.890104e-04 1.862614e-04         NA        NA
    ## 8:      z_el -4.237764 9.255839e-04 2.926953e-04         NA        NA
    ##       RelBias     Block   Role
    ##         <num>    <char> <char>
    ## 1:  0.1598716 Community       
    ## 2: -3.2891936 Community       
    ## 3: -5.9021324 Community       
    ## 4: -4.3660129 Community       
    ## 5: -2.3101881 Household       
    ## 6:         NA Household       
    ## 7:         NA Household       
    ## 8:         NA Household

``` r
out1$results$person_day[1:5, ]
```

    ##    agegrp2 agegrp3 agegrp4 n_inf n_inf_infant n_inf_sibling n_inf_adult
    ##      <int>   <int>   <int> <int>        <int>         <int>       <int>
    ## 1:       0       0       0     0            0             0           0
    ## 2:       0       0       0     0            0             0           0
    ## 3:       0       0       0     0            0             0           0
    ## 4:       0       0       0     0            0             0           0
    ## 5:       0       0       0     0            0             0           0
    ##    n_inf_elder     cases event ID_indiv ID_hh   day
    ##          <int>     <num> <int>   <char> <int> <int>
    ## 1:           0 0.0000000     0 HH001_01     1     0
    ## 2:           0 0.0000000     0 HH001_01     1     1
    ## 3:           0 0.0000000     0 HH001_01     1     2
    ## 4:           0 0.0000000     0 HH001_01     1     3
    ## 5:           0 0.1395349     0 HH001_01     1     4

``` r
out1$results$summarized_data[1:5,]
```

    ##    ID_hh indiv.index n.true.infection n.detected.infection
    ##    <int>       <int>            <int>                <int>
    ## 1:     1           1                1                    1
    ## 2:     1           2                1                    1
    ## 3:     1           3                1                    1
    ## 4:     1           4                1                    1
    ## 5:     1           5                1                    1
    ##    infection.detected.start infection.detected.end infection.true.duration
    ##                       <int>                  <int>                   <num>
    ## 1:                       67                     84                      18
    ## 2:                       64                     67                       4
    ## 3:                       66                     73                       8
    ## 4:                       68                     75                       8
    ## 5:                       83                     90                       8
    ##    last_negative                              infection.infectious.day
    ##            <int>                                                <char>
    ## 1:            66 67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84
    ## 2:            57                                           64,65,66,67
    ## 3:            65                               66,67,68,69,70,71,72,73
    ## 4:            67                               68,69,70,71,72,73,74,75
    ## 5:            82                               83,84,85,86,87,88,89,90
    ##    community.risk    role  T_FP_date  T_LP_date last_neg_date inf_date
    ##             <num>  <char>     <Date>     <Date>        <Date>   <Date>
    ## 1:          0.002  infant 2024-11-27 2024-12-14    2024-11-26     <NA>
    ## 2:          0.002   adult 2024-11-24 2024-11-27    2024-11-17     <NA>
    ## 3:          0.002   adult 2024-11-26 2024-12-03    2024-11-25     <NA>
    ## 4:          0.002 sibling 2024-11-28 2024-12-05    2024-11-27     <NA>
    ## 5:          0.002 sibling 2024-12-13 2024-12-20    2024-12-12     <NA>
    ##    inf_start_date inf_end_date inf_win_start inf_win_end infected is_index
    ##            <Date>       <Date>         <int>       <int>   <lgcl>   <lgcl>
    ## 1:     2024-11-27   2024-12-14            67          84     TRUE    FALSE
    ## 2:     2024-11-24   2024-11-27            64          67     TRUE     TRUE
    ## 3:     2024-11-26   2024-12-03            66          73     TRUE    FALSE
    ## 4:     2024-11-28   2024-12-05            68          75     TRUE    FALSE
    ## 5:     2024-12-13   2024-12-20            83          90     TRUE    FALSE
    ##    age_cat inf_day_rl infectious_day_rl infectious_end_day_rl ID_indiv
    ##      <int>      <int>             <int>                 <int>   <char>
    ## 1:       1         NA                67                    84 HH001_01
    ## 2:       3         NA                64                    67 HH001_02
    ## 3:       3         NA                66                    73 HH001_03
    ## 4:       2         NA                68                    75 HH001_04
    ## 5:       2         NA                83                    90 HH001_05
    ##    obs_start_date obs_end_date latent_delay report_delay infect_period
    ##            <Date>       <Date>       <lgcl>       <lgcl>        <lgcl>
    ## 1:     2024-09-21   2025-04-17           NA           NA            NA
    ## 2:     2024-09-21   2025-04-17           NA           NA            NA
    ## 3:     2024-09-21   2025-04-17           NA           NA            NA
    ## 4:     2024-09-21   2025-04-17           NA           NA            NA
    ## 5:     2024-09-21   2025-04-17           NA           NA            NA

``` r
# 2) Estimate from your own long-format data
HH = c(rep(1L, 6), rep(2L, 6))
individual_ID = c(1,1,2,2,3,3, 1,1,2,2,3,3)
role <- c("infant","infant","adult","adult","sibling","sibling",
"infant","infant","adult","adult","elder","elder")
test_date = c(1,8,1,8,1,8, 1,8,1,8,1,8) 
infection_status = c(0,1,0,0,0,0, 0,0,0,1,0,0) 
community_risk = rep(0.001, length(HH))

df = data.frame(HH, individual_ID, role, test_date, infection_status, community_risk)

out2 <- TransmissionChainAnalysis(
  user_data = df,                 # see required columns in ?TransmissionChainAnalysis
  n_runs    = 20
)
```

    ## Initialized start_par of length 8 (based on available covariates).

    ## 
    ## --- Post-processing of estimates ---
    ##    Parameter      Estimate           SD           SE  True  Bias RelBias
    ##       <char>         <num>        <num>        <num> <num> <num>   <num>
    ## 1:    delta0 -6.965705e+00 2.329693e-05 5.209351e-06    NA    NA      NA
    ## 2:    gamma2 -1.736633e+00 1.688152e-04 3.774823e-05    NA    NA      NA
    ## 3:    gamma3 -1.736668e+00 1.348878e-04 3.016184e-05    NA    NA      NA
    ## 4:    gamma4 -1.736582e+00 1.387672e-04 3.102929e-05    NA    NA      NA
    ## 5:    alpha0 -1.429554e+00 7.472496e-06 1.670901e-06    NA    NA      NA
    ## 6:     z_sib  9.882851e-05 3.823161e-04 8.548848e-05    NA    NA      NA
    ## 7:      z_ad -2.313573e+00 2.597893e-04 5.809064e-05    NA    NA      NA
    ## 8:      z_el  7.670987e-05 1.343273e-04 3.003651e-05    NA    NA      NA
    ##        Block   Role
    ##       <char> <char>
    ## 1: Community       
    ## 2: Community       
    ## 3: Community       
    ## 4: Community       
    ## 5: Household       
    ## 6: Household       
    ## 7: Household       
    ## 8: Household

## Inputs

- GenSyn() — for synthetic data only (errors if synthetic_data = FALSE).
- TransmissionChainAnalysis() — for user data with required columns (HH,
  individual_ID, role, test_date, infection_status, community_risk).
  Additional columns can be mapped as covariates.

## Outputs

Both functions return a list:

- Results: raw simulations (if synthetic), summaries, person–day table,
  and estimates;
- Postprocessing: a comparison table of mean estimates versus ground
  truth values (for synthetic data) or summary statistics (for user
  data)
>>>>>>> 68b0dfc (Initial commit: R package skeleton)
