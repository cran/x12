\name{AirPassengersX12Batch}
\alias{AirPassengersX12Batch}
\docType{data}
\title{
x12Batch object
}
\description{
x12Batch object of four AirPassengers series with paramters and output objects
}
\usage{data(AirPassengersX12Batch)}
\format{
  The format is:
Formal class 'x12Batch' [package "x12"] with 2 slots
  ..@ x12List    :Formal class 'x12List' [package "x12"] with 1 slots
  .. .. ..@ .Data:List of 4
  .. .. .. ..$ :Formal class 'x12Single' [package "x12"] with 7 slots
  .. .. .. .. .. ..@ ts             : Time-Series [1:144] from 1949 to 1961: 112 118 132 129 121 135 148 148 136 119 ...
  .. .. .. .. .. ..@ x12Parameter   :Formal class 'x12Parameter' [package "x12"] with 42 slots
  .. .. .. .. .. .. .. ..@ span            : NULL
  .. .. .. .. .. .. .. ..@ modelspan       : NULL
  .. .. .. .. .. .. .. ..@ decimals        : num 2
  .. .. .. .. .. .. .. ..@ transform       : chr "auto"
  .. .. .. .. .. .. .. ..@ arima           : num [1:3] 1 1 0
  .. .. .. .. .. .. .. ..@ sarima          : num [1:3] 1 1 0
  .. .. .. .. .. .. .. ..@ automdl         : logi TRUE
  .. .. .. .. .. .. .. ..@ acceptdefault   : logi FALSE
  .. .. .. .. .. .. .. ..@ balanced        : logi TRUE
  .. .. .. .. .. .. .. ..@ maxorder        : num [1:2] 3 2
  .. .. .. .. .. .. .. ..@ maxdiff         : num [1:2] 1 1
  .. .. .. .. .. .. .. ..@ regvariables    : NULL
  .. .. .. .. .. .. .. ..@ reguser         : NULL
  .. .. .. .. .. .. .. ..@ regfile         : NULL
  .. .. .. .. .. .. .. ..@ usertype        : NULL
  .. .. .. .. .. .. .. ..@ centeruser      : NULL
  .. .. .. .. .. .. .. ..@ regfilestart    : NULL
  .. .. .. .. .. .. .. ..@ seats           : logi FALSE
  .. .. .. .. .. .. .. ..@ seatsparameter  : NULL
  .. .. .. .. .. .. .. ..@ sigmalim        : NULL
  .. .. .. .. .. .. .. ..@ outlier         : NULL
  .. .. .. .. .. .. .. ..@ critical        : NULL
  .. .. .. .. .. .. .. ..@ outlier_span    : NULL
  .. .. .. .. .. .. .. ..@ outlier_method  : NULL
  .. .. .. .. .. .. .. ..@ forecast_years  : NULL
  .. .. .. .. .. .. .. ..@ backcast_years  : NULL
  .. .. .. .. .. .. .. ..@ forecast_conf   : num 0.95
  .. .. .. .. .. .. .. ..@ estimate        : logi FALSE
  .. .. .. .. .. .. .. ..@ estOutofsample  : logi TRUE
  .. .. .. .. .. .. .. ..@ slidingspans    : logi FALSE
  .. .. .. .. .. .. .. ..@ aictest         : NULL
  .. .. .. .. .. .. .. ..@ onlytd          : logi FALSE
  .. .. .. .. .. .. .. ..@ sfshort         : logi FALSE
  .. .. .. .. .. .. .. ..@ samode          : NULL
  .. .. .. .. .. .. .. ..@ seasonalma      : NULL
  .. .. .. .. .. .. .. ..@ trendma         : NULL
  .. .. .. .. .. .. .. ..@ x11appendfcst   : logi TRUE
  .. .. .. .. .. .. .. ..@ x11appendbcst   : logi FALSE
  .. .. .. .. .. .. .. ..@ x11calendarsigma: NULL
  .. .. .. .. .. .. .. ..@ x11excludefcst  : logi TRUE
  .. .. .. .. .. .. .. ..@ x11final        : chr "user"
  .. .. .. .. .. .. .. ..@ x11regress      : logi FALSE
  .. .. .. .. .. ..@ x12Output      :Formal class 'x12Output' [package "x12"] with 22 slots
  .. .. .. .. .. .. .. ..@ a1       : Time-Series [1:144] from 1949 to 1961: 112 118 132 129 121 135 148 148 136 119 ...
  .. .. .. .. .. .. .. ..@ d10      : Time-Series [1:156] from 1949 to 1962: 0.899 0.947 1.057 0.999 0.967 ...
  .. .. .. .. .. .. .. ..@ d11      : Time-Series [1:144] from 1949 to 1961: 125 125 125 129 125 ...
  .. .. .. .. .. .. .. ..@ d12      : Time-Series [1:144] from 1949 to 1961: 124 125 126 126 126 ...
  .. .. .. .. .. .. .. ..@ d13      : Time-Series [1:144] from 1949 to 1961: 1.001 0.997 0.993 1.022 0.99 ...
  .. .. .. .. .. .. .. ..@ d16      : Time-Series [1:156] from 1949 to 1962: 0.899 0.947 1.057 0.999 0.967 ...
  .. .. .. .. .. .. .. ..@ c17      : Time-Series [1:144] from 1949 to 1961: 1 1 1 0.833 1 ...
  .. .. .. .. .. .. .. ..@ d9       : Time-Series [1:144] from 1949 to 1961: NA NA NA 1.02 NA ...
  .. .. .. .. .. .. .. ..@ e2       : Time-Series [1:144] from 1949 to 1961: 125 125 125 129 125 ...
  .. .. .. .. .. .. .. ..@ d8       : Time-Series [1:144] from 1949 to 1961: 0.903 0.944 1.049 1.02 0.957 ...
  .. .. .. .. .. .. .. ..@ b1       : Time-Series [1:156] from 1949 to 1962: 112 118 132 129 121 135 148 148 136 119 ...
  .. .. .. .. .. .. .. ..@ sp0      :Formal class 'spectrum' [package "x12"] with 2 slots
  .. .. .. .. .. .. .. .. .. ..@ frequency: num [1:61] 0 0.00833 0.01667 0.025 0.03333 ...
  .. .. .. .. .. .. .. .. .. ..@ spectrum : num [1:61] -36.1 -35.4 -33.4 -31.3 -30.7 ...
  .. .. .. .. .. .. .. ..@ sp1      :Formal class 'spectrum' [package "x12"] with 2 slots
  .. .. .. .. .. .. .. .. .. ..@ frequency: num [1:61] 0 0.00833 0.01667 0.025 0.03333 ...
  .. .. .. .. .. .. .. .. .. ..@ spectrum : num [1:61] -42.2 -40.8 -36.4 -33.8 -37.1 ...
  .. .. .. .. .. .. .. ..@ sp2      :Formal class 'spectrum' [package "x12"] with 2 slots
  .. .. .. .. .. .. .. .. .. ..@ frequency: num [1:61] 0 0.00833 0.01667 0.025 0.03333 ...
  .. .. .. .. .. .. .. .. .. ..@ spectrum : num [1:61] -42.6 -46.6 -48.3 -45.5 -47.4 ...
  .. .. .. .. .. .. .. ..@ spr      :Formal class 'spectrum' [package "x12"] with 2 slots
  .. .. .. .. .. .. .. .. .. ..@ frequency: num [1:61] 0 0.00833 0.01667 0.025 0.03333 ...
  .. .. .. .. .. .. .. .. .. ..@ spectrum : num [1:61] -37.3 -35.8 -31.1 -26.2 -30.2 ...
  .. .. .. .. .. .. .. ..@ forecast :Formal class 'fbcast' [package "x12"] with 3 slots
  .. .. .. .. .. .. .. .. .. ..@ estimate: Time-Series [1:12] from 1961 to 1962: 450 426 479 492 509 ...
  .. .. .. .. .. .. .. .. .. ..@ lowerci : Time-Series [1:12] from 1961 to 1962: 419 391 436 444 455 ...
  .. .. .. .. .. .. .. .. .. ..@ upperci : Time-Series [1:12] from 1961 to 1962: 484 463 526 546 570 ...
  .. .. .. .. .. .. .. ..@ backcast :Formal class 'fbcast' [package "x12"] with 3 slots
  .. .. .. .. .. .. .. .. .. ..@ estimate:Formal class 'ts' [package "methods"] with 3 slots
  .. .. .. .. .. .. .. .. .. .. .. ..@ .Data   : logi NA
  .. .. .. .. .. .. .. .. .. .. .. ..@ tsp     : num [1:3] 1 1 1
  .. .. .. .. .. .. .. .. .. .. .. ..@ .S3Class: chr "ts"
  .. .. .. .. .. .. .. .. .. ..@ lowerci :Formal class 'ts' [package "methods"] with 3 slots
  .. .. .. .. .. .. .. .. .. .. .. ..@ .Data   : logi NA
  .. .. .. .. .. .. .. .. .. .. .. ..@ tsp     : num [1:3] 1 1 1
  .. .. .. .. .. .. .. .. .. .. .. ..@ .S3Class: chr "ts"
  .. .. .. .. .. .. .. .. .. ..@ upperci :Formal class 'ts' [package "methods"] with 3 slots
  .. .. .. .. .. .. .. .. .. .. .. ..@ .Data   : logi NA
  .. .. .. .. .. .. .. .. .. .. .. ..@ tsp     : num [1:3] 1 1 1
  .. .. .. .. .. .. .. .. .. .. .. ..@ .S3Class: chr "ts"
  .. .. .. .. .. .. .. ..@ dg       :List of 38
  .. .. .. .. .. .. .. .. ..$ x11regress   :List of 1
  .. .. .. .. .. .. .. .. .. ..$ x11regress: chr "no"
  .. .. .. .. .. .. .. .. ..$ transform    :List of 1
  .. .. .. .. .. .. .. .. .. ..$ transform: chr "Automatic selection"
  .. .. .. .. .. .. .. .. ..$ samode       :List of 2
  .. .. .. .. .. .. .. .. .. ..$ samode     : chr "auto-mode seasonal adjustment"
  .. .. .. .. .. .. .. .. .. ..$ finalsamode: chr "multiplicative"
  .. .. .. .. .. .. .. .. ..$ seasonalma   :List of 2
  .. .. .. .. .. .. .. .. .. ..$ seasonalma     : chr "M.S.R."
  .. .. .. .. .. .. .. .. .. ..$ finalseasonalma: chr "3x3"
  .. .. .. .. .. .. .. .. ..$ trendma      :List of 2
  .. .. .. .. .. .. .. .. .. ..$ trendma     : chr "default"
  .. .. .. .. .. .. .. .. .. ..$ finaltrendma: num 9
  .. .. .. .. .. .. .. .. ..$ arimamdl     :List of 1
  .. .. .. .. .. .. .. .. .. ..$ arimamdl: chr "(0 1 1)(0 1 1)"
  .. .. .. .. .. .. .. .. ..$ automdl      :List of 1
  .. .. .. .. .. .. .. .. .. ..$ automdl: chr "(0 1 1)(0 1 1)"
  .. .. .. .. .. .. .. .. ..$ regmdl       :List of 1
  .. .. .. .. .. .. .. .. .. ..$ regmdl: chr "none"
  .. .. .. .. .. .. .. .. ..$ nout         :List of 1
  .. .. .. .. .. .. .. .. .. ..$ nout: num 0
  .. .. .. .. .. .. .. .. ..$ nautoout     :List of 1
  .. .. .. .. .. .. .. .. .. ..$ nautoout: chr "-"
  .. .. .. .. .. .. .. .. ..$ nalmostout   :List of 1
  .. .. .. .. .. .. .. .. .. ..$ nalmostout: chr "-"
  .. .. .. .. .. .. .. .. ..$ almostoutlier:List of 1
  .. .. .. .. .. .. .. .. .. ..$ almostoutlier: chr "-"
  .. .. .. .. .. .. .. .. ..$ crit         :List of 1
  .. .. .. .. .. .. .. .. .. ..$ crit: chr "-"
  .. .. .. .. .. .. .. .. ..$ outlier      :List of 1
  .. .. .. .. .. .. .. .. .. ..$ outlier: chr "-"
  .. .. .. .. .. .. .. .. ..$ userdefined  :List of 1
  .. .. .. .. .. .. .. .. .. ..$ userdefined: chr "-"
  .. .. .. .. .. .. .. .. ..$ autooutlier  :List of 1
  .. .. .. .. .. .. .. .. .. ..$ autooutlier: chr "-"
  .. .. .. .. .. .. .. .. ..$ peaks.seas   :List of 1
  .. .. .. .. .. .. .. .. .. ..$ peaks.seas: chr "rsd"
  .. .. .. .. .. .. .. .. ..$ peaks.td     :List of 1
  .. .. .. .. .. .. .. .. .. ..$ peaks.td: chr "sa irr"
  .. .. .. .. .. .. .. .. ..$ id.seas      :List of 1
  .. .. .. .. .. .. .. .. .. ..$ id.seas: chr "yes"
  .. .. .. .. .. .. .. .. ..$ id.rsdseas   :List of 1
  .. .. .. .. .. .. .. .. .. ..$ id.rsdseas: chr "none"
  .. .. .. .. .. .. .. .. ..$ spcrsd       :List of 11
  .. .. .. .. .. .. .. .. .. ..$ spcrsd.t1   :List of 2
  .. .. .. .. .. .. .. .. .. .. ..$ : num 0.7
  .. .. .. .. .. .. .. .. .. .. ..$ : chr "+"
  .. .. .. .. .. .. .. .. .. ..$ spcrsd.t2   :List of 2
  .. .. .. .. .. .. .. .. .. .. ..$ : num 5
  .. .. .. .. .. .. .. .. .. .. ..$ : chr "+"
  .. .. .. .. .. .. .. .. .. ..$ spcrsd.t.dom: chr "t1"
  .. .. .. .. .. .. .. .. .. ..$ spcrsd.s1   :List of 2
  .. .. .. .. .. .. .. .. .. .. ..$ : num 16.9
  .. .. .. .. .. .. .. .. .. .. ..$ : chr "+"
  .. .. .. .. .. .. .. .. .. ..$ spcrsd.s2   : chr "nopeak"
  .. .. .. .. .. .. .. .. .. ..$ spcrsd.s3   : chr "nopeak"
  .. .. .. .. .. .. .. .. .. ..$ spcrsd.s4   : chr "nopeak"
  .. .. .. .. .. .. .. .. .. ..$ spcrsd.s5   : chr "nopeak"
  .. .. .. .. .. .. .. .. .. ..$ spcrsd.s6   : chr "nopeak"
  .. .. .. .. .. .. .. .. .. ..$ spcrsd.s.dom: chr "s1"
  .. .. .. .. .. .. .. .. .. ..$ spcrsd.dom  : chr "s1"
  .. .. .. .. .. .. .. .. ..$ spcori       :List of 11
  .. .. .. .. .. .. .. .. .. ..$ spcori.t1   : chr "nopeak"
  .. .. .. .. .. .. .. .. .. ..$ spcori.t2   : chr "nopeak"
  .. .. .. .. .. .. .. .. .. ..$ spcori.t.dom: chr "no"
  .. .. .. .. .. .. .. .. .. ..$ spcori.s1   :List of 2
  .. .. .. .. .. .. .. .. .. .. ..$ : num 20.7
  .. .. .. .. .. .. .. .. .. .. ..$ : chr "+"
  .. .. .. .. .. .. .. .. .. ..$ spcori.s2   :List of 2
  .. .. .. .. .. .. .. .. .. .. ..$ : num 17.7
  .. .. .. .. .. .. .. .. .. .. ..$ : chr "+"
  .. .. .. .. .. .. .. .. .. ..$ spcori.s3   :List of 2
  .. .. .. .. .. .. .. .. .. .. ..$ : num 13.8
  .. .. .. .. .. .. .. .. .. .. ..$ : chr "+"
  .. .. .. .. .. .. .. .. .. ..$ spcori.s4   :List of 2
  .. .. .. .. .. .. .. .. .. .. ..$ : num 16.5
  .. .. .. .. .. .. .. .. .. .. ..$ : chr "+"
  .. .. .. .. .. .. .. .. .. ..$ spcori.s5   :List of 2
  .. .. .. .. .. .. .. .. .. .. ..$ : num 8.5
  .. .. .. .. .. .. .. .. .. .. ..$ : chr "+"
  .. .. .. .. .. .. .. .. .. ..$ spcori.s6   : chr "nopeak"
  .. .. .. .. .. .. .. .. .. ..$ spcori.s.dom: chr "s1"
  .. .. .. .. .. .. .. .. .. ..$ spcori.dom  : chr "s1"
  .. .. .. .. .. .. .. .. ..$ spcsa        :List of 11
  .. .. .. .. .. .. .. .. .. ..$ spcsa.t1   : chr "nopeak"
  .. .. .. .. .. .. .. .. .. ..$ spcsa.t2   :List of 2
  .. .. .. .. .. .. .. .. .. .. ..$ : num 13.5
  .. .. .. .. .. .. .. .. .. .. ..$ : chr "+"
  .. .. .. .. .. .. .. .. .. ..$ spcsa.t.dom: chr "t2"
  .. .. .. .. .. .. .. .. .. ..$ spcsa.s1   :List of 2
  .. .. .. .. .. .. .. .. .. .. ..$ : num 5.7
  .. .. .. .. .. .. .. .. .. .. ..$ : chr "+"
  .. .. .. .. .. .. .. .. .. ..$ spcsa.s2   : chr "nopeak"
  .. .. .. .. .. .. .. .. .. ..$ spcsa.s3   : chr "nopeak"
  .. .. .. .. .. .. .. .. .. ..$ spcsa.s4   : chr "nopeak"
  .. .. .. .. .. .. .. .. .. ..$ spcsa.s5   : chr "nopeak"
  .. .. .. .. .. .. .. .. .. ..$ spcsa.s6   : chr "nopeak"
  .. .. .. .. .. .. .. .. .. ..$ spcsa.s.dom: chr "s1"
  .. .. .. .. .. .. .. .. .. ..$ spcsa.dom  : chr "no"
  .. .. .. .. .. .. .. .. ..$ spcirr       :List of 11
  .. .. .. .. .. .. .. .. .. ..$ spcirr.t1   : chr "nopeak"
  .. .. .. .. .. .. .. .. .. ..$ spcirr.t2   :List of 2
  .. .. .. .. .. .. .. .. .. .. ..$ : num 11
  .. .. .. .. .. .. .. .. .. .. ..$ : chr "+"
  .. .. .. .. .. .. .. .. .. ..$ spcirr.t.dom: chr "t2"
  .. .. .. .. .. .. .. .. .. ..$ spcirr.s1   : chr "nopeak"
  .. .. .. .. .. .. .. .. .. ..$ spcirr.s2   : chr "nopeak"
  .. .. .. .. .. .. .. .. .. ..$ spcirr.s3   : chr "nopeak"
  .. .. .. .. .. .. .. .. .. ..$ spcirr.s4   : chr "nopeak"
  .. .. .. .. .. .. .. .. .. ..$ spcirr.s5   : chr "nopeak"
  .. .. .. .. .. .. .. .. .. ..$ spcirr.s6   : chr "nopeak"
  .. .. .. .. .. .. .. .. .. ..$ spcirr.s.dom: chr "no"
  .. .. .. .. .. .. .. .. .. ..$ spcirr.dom  : chr "no"
  .. .. .. .. .. .. .. .. ..$ q            :List of 1
  .. .. .. .. .. .. .. .. .. ..$ q: num 0.26
  .. .. .. .. .. .. .. .. ..$ q2           :List of 1
  .. .. .. .. .. .. .. .. .. ..$ q2: num 0.29
  .. .. .. .. .. .. .. .. ..$ nmfail       :List of 1
  .. .. .. .. .. .. .. .. .. ..$ nmfail: num 0
  .. .. .. .. .. .. .. .. ..$ loglikelihood:List of 1
  .. .. .. .. .. .. .. .. .. ..$ loglikelihood: num 245
  .. .. .. .. .. .. .. .. ..$ aic          :List of 1
  .. .. .. .. .. .. .. .. .. ..$ aic: num 987
  .. .. .. .. .. .. .. .. ..$ aicc         :List of 1
  .. .. .. .. .. .. .. .. .. ..$ aicc: num 987
  .. .. .. .. .. .. .. .. ..$ bic          :List of 1
  .. .. .. .. .. .. .. .. .. ..$ bic: num 996
  .. .. .. .. .. .. .. .. ..$ hq           :List of 1
  .. .. .. .. .. .. .. .. .. ..$ hq: num 991
  .. .. .. .. .. .. .. .. ..$ aape         :List of 5
  .. .. .. .. .. .. .. .. .. ..$ aape.mode: chr "withinsample"
  .. .. .. .. .. .. .. .. .. ..$ aape.0   : num 5.63
  .. .. .. .. .. .. .. .. .. ..$ aape.1   : num 2.81
  .. .. .. .. .. .. .. .. .. ..$ aape.2   : num 6.38
  .. .. .. .. .. .. .. .. .. ..$ aape.3   : num 7.69
  .. .. .. .. .. .. .. .. ..$ autotransform:List of 1
  .. .. .. .. .. .. .. .. .. ..$ autotransform: chr "Log(y)"
  .. .. .. .. .. .. .. .. ..$ ifout        : chr "No outlier detection performed"
  .. .. .. .. .. .. .. .. ..$ rsd.acf      :'data.frame':	24 obs. of  6 variables:
  .. .. .. .. .. .. .. .. .. ..$ lag        : num [1:24] 1 2 3 4 5 6 7 8 9 10 ...
  .. .. .. .. .. .. .. .. .. ..$ sample.acf : num [1:24] 0.0121 0.0222 -0.1284 -0.1255 0.0599 ...
  .. .. .. .. .. .. .. .. .. ..$ stderr.acf : num [1:24] 0.0874 0.0874 0.0874 0.0889 0.0902 ...
  .. .. .. .. .. .. .. .. .. ..$ Ljung-Box.q: num [1:24] 0.0195 0.0858 2.3301 4.4896 4.9856 ...
  .. .. .. .. .. .. .. .. .. ..$ df.q       : num [1:24] 0 0 1 2 3 4 5 6 7 8 ...
  .. .. .. .. .. .. .. .. .. ..$ pval       : num [1:24] 0 0 0.127 0.106 0.173 ...
  .. .. .. .. .. .. .. .. ..$ rsd.pacf     :'data.frame':	24 obs. of  3 variables:
  .. .. .. .. .. .. .. .. .. ..$ lag        : num [1:24] 1 2 3 4 5 6 7 8 9 10 ...
  .. .. .. .. .. .. .. .. .. ..$ sample.pacf: num [1:24] 0.0121 0.022 -0.129 -0.1247 0.0697 ...
  .. .. .. .. .. .. .. .. .. ..$ stderr.pacf: num [1:24] 0.0874 0.0874 0.0874 0.0874 0.0874 ...
  .. .. .. .. .. .. .. .. ..$ rsd.acf2     :'data.frame':	12 obs. of  3 variables:
  .. .. .. .. .. .. .. .. .. ..$ lag        : num [1:12] 1 2 3 4 5 6 7 8 9 10 ...
  .. .. .. .. .. .. .. .. .. ..$ sample.acf2: num [1:12] 0.11821 0.00644 0.02263 -0.03607 0.09799 ...
  .. .. .. .. .. .. .. .. .. ..$ stderr.acf2: num [1:12] 0.0874 0.0886 0.0886 0.0886 0.0887 ...
  .. .. .. .. .. .. .. ..@ seats    : logi FALSE
  .. .. .. .. .. .. .. ..@ file     : chr "Series_1"
  .. .. .. .. .. .. .. ..@ tblnames : chr [1:11] "a1" "d10" "d11" "d12" ...
  .. .. .. .. .. .. .. ..@ Rtblnames: chr [1:11] "Original series" "Final seasonal factors" "Final seasonally adjusted data" "Final trend cycle" ...
  .. .. .. .. .. ..@ x12OldParameter:List of 1
  .. .. .. .. .. .. ..$ :Formal class 'x12Parameter' [package "x12"] with 42 slots
  .. .. .. .. .. .. .. .. ..@ span            : NULL
  .. .. .. .. .. .. .. .. ..@ modelspan       : NULL
  .. .. .. .. .. .. .. .. ..@ decimals        : num 2
  .. .. .. .. .. .. .. .. ..@ transform       : chr "auto"
  .. .. .. .. .. .. .. .. ..@ arima           : num [1:3] 1 1 0
  .. .. .. .. .. .. .. .. ..@ sarima          : num [1:3] 1 1 0
  .. .. .. .. .. .. .. .. ..@ automdl         : logi TRUE
  .. .. .. .. .. .. .. .. ..@ acceptdefault   : logi FALSE
  .. .. .. .. .. .. .. .. ..@ balanced        : logi TRUE
  .. .. .. .. .. .. .. .. ..@ maxorder        : num [1:2] 3 2
  .. .. .. .. .. .. .. .. ..@ maxdiff         : num [1:2] 1 1
  .. .. .. .. .. .. .. .. ..@ regvariables    : NULL
  .. .. .. .. .. .. .. .. ..@ reguser         : NULL
  .. .. .. .. .. .. .. .. ..@ regfile         : NULL
  .. .. .. .. .. .. .. .. ..@ usertype        : NULL
  .. .. .. .. .. .. .. .. ..@ centeruser      : NULL
  .. .. .. .. .. .. .. .. ..@ regfilestart    : NULL
  .. .. .. .. .. .. .. .. ..@ seats           : logi FALSE
  .. .. .. .. .. .. .. .. ..@ seatsparameter  : NULL
  .. .. .. .. .. .. .. .. ..@ sigmalim        : NULL
  .. .. .. .. .. .. .. .. ..@ outlier         : NULL
  .. .. .. .. .. .. .. .. ..@ critical        : NULL
  .. .. .. .. .. .. .. .. ..@ outlier_span    : NULL
  .. .. .. .. .. .. .. .. ..@ outlier_method  : NULL
  .. .. .. .. .. .. .. .. ..@ forecast_years  : NULL
  .. .. .. .. .. .. .. .. ..@ backcast_years  : NULL
  .. .. .. .. .. .. .. .. ..@ forecast_conf   : num 0.95
  .. .. .. .. .. .. .. .. ..@ estimate        : logi FALSE
  .. .. .. .. .. .. .. .. ..@ estOutofsample  : logi TRUE
  .. .. .. .. .. .. .. .. ..@ slidingspans    : logi FALSE
  .. .. .. .. .. .. .. .. ..@ aictest         : NULL
  .. .. .. .. .. .. .. .. ..@ onlytd          : logi FALSE
  .. .. .. .. .. .. .. .. ..@ sfshort         : logi FALSE
  .. .. .. .. .. .. .. .. ..@ samode          : NULL
  .. .. .. .. .. .. .. .. ..@ seasonalma      : NULL
  .. .. .. .. .. .. .. .. ..@ trendma         : NULL
  .. .. .. .. .. .. .. .. ..@ x11appendfcst   : logi TRUE
  .. .. .. .. .. .. .. .. ..@ x11appendbcst   : logi FALSE
  .. .. .. .. .. .. .. .. ..@ x11calendarsigma: NULL
  .. .. .. .. .. .. .. .. ..@ x11excludefcst  : logi TRUE
  .. .. .. .. .. .. .. .. ..@ x11final        : chr "user"
  .. .. .. .. .. .. .. .. ..@ x11regress      : logi FALSE
  .. .. .. .. .. ..@ x12OldOutput   :List of 1
  .. .. .. .. .. .. ..$ :Formal class 'x12Output' [package "x12"] with 22 slots
  .. .. .. .. .. .. .. .. ..@ a1       : Time-Series [1:144] from 1949 to 1961: 112 118 132 129 121 135 148 148 136 119 ...
  .. .. .. .. .. .. .. .. ..@ d10      : Time-Series [1:156] from 1949 to 1962: 0.899 0.947 1.057 0.999 0.967 ...
  .. .. .. .. .. .. .. .. ..@ d11      : Time-Series [1:144] from 1949 to 1961: 125 125 125 129 125 ...
  .. .. .. .. .. .. .. .. ..@ d12      : Time-Series [1:144] from 1949 to 1961: 124 125 126 126 126 ...
  .. .. .. .. .. .. .. .. ..@ d13      : Time-Series [1:144] from 1949 to 1961: 1.001 0.997 0.993 1.022 0.99 ...
  .. .. .. .. .. .. .. .. ..@ d16      : Time-Series [1:156] from 1949 to 1962: 0.899 0.947 1.057 0.999 0.967 ...
  .. .. .. .. .. .. .. .. ..@ c17      : Time-Series [1:144] from 1949 to 1961: 1 1 1 0.833 1 ...
  .. .. .. .. .. .. .. .. ..@ d9       : Time-Series [1:144] from 1949 to 1961: NA NA NA 1.02 NA ...
  .. .. .. .. .. .. .. .. ..@ e2       : Time-Series [1:144] from 1949 to 1961: 125 125 125 129 125 ...
  .. .. .. .. .. .. .. .. ..@ d8       : Time-Series [1:144] from 1949 to 1961: 0.903 0.944 1.049 1.02 0.957 ...
  .. .. .. .. .. .. .. .. ..@ b1       : Time-Series [1:156] from 1949 to 1962: 112 118 132 129 121 135 148 148 136 119 ...
  .. .. .. .. .. .. .. .. ..@ sp0      :Formal class 'spectrum' [package "x12"] with 2 slots
  .. .. .. .. .. .. .. .. .. .. ..@ frequency: num [1:61] 0 0.00833 0.01667 0.025 0.03333 ...
  .. .. .. .. .. .. .. .. .. .. ..@ spectrum : num [1:61] -36.1 -35.4 -33.4 -31.3 -30.7 ...
  .. .. .. .. .. .. .. .. ..@ sp1      :Formal class 'spectrum' [package "x12"] with 2 slots
  .. .. .. .. .. .. .. .. .. .. ..@ frequency: num [1:61] 0 0.00833 0.01667 0.025 0.03333 ...
  .. .. .. .. .. .. .. .. .. .. ..@ spectrum : num [1:61] -42.2 -40.8 -36.5 -33.8 -37.1 ...
  .. .. .. .. .. .. .. .. ..@ sp2      :Formal class 'spectrum' [package "x12"] with 2 slots
  .. .. .. .. .. .. .. .. .. .. ..@ frequency: num [1:61] 0 0.00833 0.01667 0.025 0.03333 ...
  .. .. .. .. .. .. .. .. .. .. ..@ spectrum : num [1:61] -42.4 -46.7 -48.3 -45.4 -47.5 ...
  .. .. .. .. .. .. .. .. ..@ spr      :Formal class 'spectrum' [package "x12"] with 2 slots
  .. .. .. .. .. .. .. .. .. .. ..@ frequency: num [1:61] 0 0.00833 0.01667 0.025 0.03333 ...
  .. .. .. .. .. .. .. .. .. .. ..@ spectrum : num [1:61] -37.6 -36 -30.3 -26.8 -32.8 ...
  .. .. .. .. .. .. .. .. ..@ forecast :Formal class 'fbcast' [package "x12"] with 3 slots
  .. .. .. .. .. .. .. .. .. .. ..@ estimate: Time-Series [1:12] from 1961 to 1962: 452 426 479 499 519 ...
  .. .. .. .. .. .. .. .. .. .. ..@ lowerci : Time-Series [1:12] from 1961 to 1962: 419 390 431 443 456 ...
  .. .. .. .. .. .. .. .. .. .. ..@ upperci : Time-Series [1:12] from 1961 to 1962: 487 466 532 561 591 ...
  .. .. .. .. .. .. .. .. ..@ backcast :Formal class 'fbcast' [package "x12"] with 3 slots
  .. .. .. .. .. .. .. .. .. .. ..@ estimate:Formal class 'ts' [package "methods"] with 3 slots
  .. .. .. .. .. .. .. .. .. .. .. .. ..@ .Data   : logi NA
  .. .. .. .. .. .. .. .. .. .. .. .. ..@ tsp     : num [1:3] 1 1 1
  .. .. .. .. .. .. .. .. .. .. .. .. ..@ .S3Class: chr "ts"
  .. .. .. .. .. .. .. .. .. .. ..@ lowerci :Formal class 'ts' [package "methods"] with 3 slots
  .. .. .. .. .. .. .. .. .. .. .. .. ..@ .Data   : logi NA
  .. .. .. .. .. .. .. .. .. .. .. .. ..@ tsp     : num [1:3] 1 1 1
  .. .. .. .. .. .. .. .. .. .. .. .. ..@ .S3Class: chr "ts"
  .. .. .. .. .. .. .. .. .. .. ..@ upperci :Formal class 'ts' [package "methods"] with 3 slots
  .. .. .. .. .. .. .. .. .. .. .. .. ..@ .Data   : logi NA
  .. .. .. .. .. .. .. .. .. .. .. .. ..@ tsp     : num [1:3] 1 1 1
  .. .. .. .. .. .. .. .. .. .. .. .. ..@ .S3Class: chr "ts"
  .. .. .. .. .. .. .. .. ..@ dg       :List of 38
  .. .. .. .. .. .. .. .. .. ..$ x11regress   :List of 1
  .. .. .. .. .. .. .. .. .. .. ..$ x11regress: chr "no"
  .. .. .. .. .. .. .. .. .. ..$ transform    :List of 1
  .. .. .. .. .. .. .. .. .. .. ..$ transform: chr "Automatic selection"
  .. .. .. .. .. .. .. .. .. ..$ samode       :List of 2
  .. .. .. .. .. .. .. .. .. .. ..$ samode     : chr "auto-mode seasonal adjustment"
  .. .. .. .. .. .. .. .. .. .. ..$ finalsamode: chr "multiplicative"
  .. .. .. .. .. .. .. .. .. ..$ seasonalma   :List of 2
  .. .. .. .. .. .. .. .. .. .. ..$ seasonalma     : chr "M.S.R."
  .. .. .. .. .. .. .. .. .. .. ..$ finalseasonalma: chr "3x3"
  .. .. .. .. .. .. .. .. .. ..$ trendma      :List of 2
  .. .. .. .. .. .. .. .. .. .. ..$ trendma     : chr "default"
  .. .. .. .. .. .. .. .. .. .. ..$ finaltrendma: num 9
  .. .. .. .. .. .. .. .. .. ..$ arimamdl     :List of 1
  .. .. .. .. .. .. .. .. .. .. ..$ arimamdl: chr "(1,1,0)(1,1,0)"
  .. .. .. .. .. .. .. .. .. ..$ automdl      :List of 1
  .. .. .. .. .. .. .. .. .. .. ..$ automdl: chr "-"
  .. .. .. .. .. .. .. .. .. ..$ regmdl       :List of 1
  .. .. .. .. .. .. .. .. .. .. ..$ regmdl: chr "none"
  .. .. .. .. .. .. .. .. .. ..$ nout         :List of 1
  .. .. .. .. .. .. .. .. .. .. ..$ nout: num 0
  .. .. .. .. .. .. .. .. .. ..$ nautoout     :List of 1
  .. .. .. .. .. .. .. .. .. .. ..$ nautoout: chr "-"
  .. .. .. .. .. .. .. .. .. ..$ nalmostout   :List of 1
  .. .. .. .. .. .. .. .. .. .. ..$ nalmostout: chr "-"
  .. .. .. .. .. .. .. .. .. ..$ almostoutlier:List of 1
  .. .. .. .. .. .. .. .. .. .. ..$ almostoutlier: chr "-"
  .. .. .. .. .. .. .. .. .. ..$ crit         :List of 1
  .. .. .. .. .. .. .. .. .. .. ..$ crit: chr "-"
  .. .. .. .. .. .. .. .. .. ..$ outlier      :List of 1
  .. .. .. .. .. .. .. .. .. .. ..$ outlier: chr "-"
  .. .. .. .. .. .. .. .. .. ..$ userdefined  :List of 1
  .. .. .. .. .. .. .. .. .. .. ..$ userdefined: chr "-"
  .. .. .. .. .. .. .. .. .. ..$ autooutlier  :List of 1
  .. .. .. .. .. .. .. .. .. .. ..$ autooutlier: chr "-"
  .. .. .. .. .. .. .. .. .. ..$ peaks.seas   :List of 1
  .. .. .. .. .. .. .. .. .. .. ..$ peaks.seas: chr "none"
  .. .. .. .. .. .. .. .. .. ..$ peaks.td     :List of 1
  .. .. .. .. .. .. .. .. .. .. ..$ peaks.td: chr "sa irr"
  .. .. .. .. .. .. .. .. .. ..$ id.seas      :List of 1
  .. .. .. .. .. .. .. .. .. .. ..$ id.seas: chr "yes"
  .. .. .. .. .. .. .. .. .. ..$ id.rsdseas   :List of 1
  .. .. .. .. .. .. .. .. .. .. ..$ id.rsdseas: chr "none"
  .. .. .. .. .. .. .. .. .. ..$ spcrsd       :List of 11
  .. .. .. .. .. .. .. .. .. .. ..$ spcrsd.t1   :List of 2
  .. .. .. .. .. .. .. .. .. .. .. ..$ : num 5.8
  .. .. .. .. .. .. .. .. .. .. .. ..$ : chr "+"
  .. .. .. .. .. .. .. .. .. .. ..$ spcrsd.t2   :List of 2
  .. .. .. .. .. .. .. .. .. .. .. ..$ : num 3.2
  .. .. .. .. .. .. .. .. .. .. .. ..$ : chr "+"
  .. .. .. .. .. .. .. .. .. .. ..$ spcrsd.t.dom: chr "t1"
  .. .. .. .. .. .. .. .. .. .. ..$ spcrsd.s1   :List of 2
  .. .. .. .. .. .. .. .. .. .. .. ..$ : num 2.5
  .. .. .. .. .. .. .. .. .. .. .. ..$ : chr "+"
  .. .. .. .. .. .. .. .. .. .. ..$ spcrsd.s2   : chr "nopeak"
  .. .. .. .. .. .. .. .. .. .. ..$ spcrsd.s3   : chr "nopeak"
  .. .. .. .. .. .. .. .. .. .. ..$ spcrsd.s4   : chr "nopeak"
  .. .. .. .. .. .. .. .. .. .. ..$ spcrsd.s5   : chr "nopeak"
  .. .. .. .. .. .. .. .. .. .. ..$ spcrsd.s6   : chr "nopeak"
  .. .. .. .. .. .. .. .. .. .. ..$ spcrsd.s.dom: chr "s1"
  .. .. .. .. .. .. .. .. .. .. ..$ spcrsd.dom  : chr "t1"
  .. .. .. .. .. .. .. .. .. ..$ spcori       :List of 11
  .. .. .. .. .. .. .. .. .. .. ..$ spcori.t1   : chr "nopeak"
  .. .. .. .. .. .. .. .. .. .. ..$ spcori.t2   : chr "nopeak"
  .. .. .. .. .. .. .. .. .. .. ..$ spcori.t.dom: chr "no"
  .. .. .. .. .. .. .. .. .. .. ..$ spcori.s1   :List of 2
  .. .. .. .. .. .. .. .. .. .. .. ..$ : num 20.7
  .. .. .. .. .. .. .. .. .. .. .. ..$ : chr "+"
  .. .. .. .. .. .. .. .. .. .. ..$ spcori.s2   :List of 2
  .. .. .. .. .. .. .. .. .. .. .. ..$ : num 17.7
  .. .. .. .. .. .. .. .. .. .. .. ..$ : chr "+"
  .. .. .. .. .. .. .. .. .. .. ..$ spcori.s3   :List of 2
  .. .. .. .. .. .. .. .. .. .. .. ..$ : num 13.8
  .. .. .. .. .. .. .. .. .. .. .. ..$ : chr "+"
  .. .. .. .. .. .. .. .. .. .. ..$ spcori.s4   :List of 2
  .. .. .. .. .. .. .. .. .. .. .. ..$ : num 16.5
  .. .. .. .. .. .. .. .. .. .. .. ..$ : chr "+"
  .. .. .. .. .. .. .. .. .. .. ..$ spcori.s5   :List of 2
  .. .. .. .. .. .. .. .. .. .. .. ..$ : num 8.5
  .. .. .. .. .. .. .. .. .. .. .. ..$ : chr "+"
  .. .. .. .. .. .. .. .. .. .. ..$ spcori.s6   : chr "nopeak"
  .. .. .. .. .. .. .. .. .. .. ..$ spcori.s.dom: chr "s1"
  .. .. .. .. .. .. .. .. .. .. ..$ spcori.dom  : chr "s1"
  .. .. .. .. .. .. .. .. .. ..$ spcsa        :List of 11
  .. .. .. .. .. .. .. .. .. .. ..$ spcsa.t1   : chr "nopeak"
  .. .. .. .. .. .. .. .. .. .. ..$ spcsa.t2   :List of 2
  .. .. .. .. .. .. .. .. .. .. .. ..$ : num 13.5
  .. .. .. .. .. .. .. .. .. .. .. ..$ : chr "+"
  .. .. .. .. .. .. .. .. .. .. ..$ spcsa.t.dom: chr "t2"
  .. .. .. .. .. .. .. .. .. .. ..$ spcsa.s1   :List of 2
  .. .. .. .. .. .. .. .. .. .. .. ..$ : num 5.6
  .. .. .. .. .. .. .. .. .. .. .. ..$ : chr "+"
  .. .. .. .. .. .. .. .. .. .. ..$ spcsa.s2   : chr "nopeak"
  .. .. .. .. .. .. .. .. .. .. ..$ spcsa.s3   : chr "nopeak"
  .. .. .. .. .. .. .. .. .. .. ..$ spcsa.s4   : chr "nopeak"
  .. .. .. .. .. .. .. .. .. .. ..$ spcsa.s5   : chr "nopeak"
  .. .. .. .. .. .. .. .. .. .. ..$ spcsa.s6   : chr "nopeak"
  .. .. .. .. .. .. .. .. .. .. ..$ spcsa.s.dom: chr "s1"
  .. .. .. .. .. .. .. .. .. .. ..$ spcsa.dom  : chr "no"
  .. .. .. .. .. .. .. .. .. ..$ spcirr       :List of 11
  .. .. .. .. .. .. .. .. .. .. ..$ spcirr.t1   : chr "nopeak"
  .. .. .. .. .. .. .. .. .. .. ..$ spcirr.t2   :List of 2
  .. .. .. .. .. .. .. .. .. .. .. ..$ : num 9.9
  .. .. .. .. .. .. .. .. .. .. .. ..$ : chr "+"
  .. .. .. .. .. .. .. .. .. .. ..$ spcirr.t.dom: chr "t2"
  .. .. .. .. .. .. .. .. .. .. ..$ spcirr.s1   : chr "nopeak"
  .. .. .. .. .. .. .. .. .. .. ..$ spcirr.s2   : chr "nopeak"
  .. .. .. .. .. .. .. .. .. .. ..$ spcirr.s3   : chr "nopeak"
  .. .. .. .. .. .. .. .. .. .. ..$ spcirr.s4   : chr "nopeak"
  .. .. .. .. .. .. .. .. .. .. ..$ spcirr.s5   : chr "nopeak"
  .. .. .. .. .. .. .. .. .. .. ..$ spcirr.s6   : num 0.2
  .. .. .. .. .. .. .. .. .. .. ..$ spcirr.s.dom: chr "no"
  .. .. .. .. .. .. .. .. .. .. ..$ spcirr.dom  : chr "no"
  .. .. .. .. .. .. .. .. .. ..$ q            :List of 1
  .. .. .. .. .. .. .. .. .. .. ..$ q: num 0.26
  .. .. .. .. .. .. .. .. .. ..$ q2           :List of 1
  .. .. .. .. .. .. .. .. .. .. ..$ q2: num 0.29
  .. .. .. .. .. .. .. .. .. ..$ nmfail       :List of 1
  .. .. .. .. .. .. .. .. .. .. ..$ nmfail: num 0
  .. .. .. .. .. .. .. .. .. ..$ loglikelihood:List of 1
  .. .. .. .. .. .. .. .. .. .. ..$ loglikelihood: num 240
  .. .. .. .. .. .. .. .. .. ..$ aic          :List of 1
  .. .. .. .. .. .. .. .. .. .. ..$ aic: num 996
  .. .. .. .. .. .. .. .. .. ..$ aicc         :List of 1
  .. .. .. .. .. .. .. .. .. .. ..$ aicc: num 996
  .. .. .. .. .. .. .. .. .. ..$ bic          :List of 1
  .. .. .. .. .. .. .. .. .. .. ..$ bic: num 1004
  .. .. .. .. .. .. .. .. .. ..$ hq           :List of 1
  .. .. .. .. .. .. .. .. .. .. ..$ hq: num 999
  .. .. .. .. .. .. .. .. .. ..$ aape         :List of 5
  .. .. .. .. .. .. .. .. .. .. ..$ aape.mode: chr "withinsample"
  .. .. .. .. .. .. .. .. .. .. ..$ aape.0   : num 5.65
  .. .. .. .. .. .. .. .. .. .. ..$ aape.1   : num 2.6
  .. .. .. .. .. .. .. .. .. .. ..$ aape.2   : num 7.61
  .. .. .. .. .. .. .. .. .. .. ..$ aape.3   : num 6.73
  .. .. .. .. .. .. .. .. .. ..$ autotransform:List of 1
  .. .. .. .. .. .. .. .. .. .. ..$ autotransform: chr "Log(y)"
  .. .. .. .. .. .. .. .. .. ..$ ifout        : chr "No outlier detection performed"
  .. .. .. .. .. .. .. .. .. ..$ rsd.acf      :'data.frame':	24 obs. of  6 variables:
  .. .. .. .. .. .. .. .. .. .. ..$ lag        : num [1:24] 1 2 3 4 5 6 7 8 9 10 ...
  .. .. .. .. .. .. .. .. .. .. ..$ sample.acf : num [1:24] -0.0294 -0.1072 -0.1345 -0.0779 0.1332 ...
  .. .. .. .. .. .. .. .. .. .. ..$ stderr.acf : num [1:24] 0.0874 0.0874 0.0884 0.09 0.0905 ...
  .. .. .. .. .. .. .. .. .. .. ..$ Ljung-Box.q: num [1:24] 0.116 1.667 4.131 4.963 7.418 ...
  .. .. .. .. .. .. .. .. .. .. ..$ df.q       : num [1:24] 0 0 1 2 3 4 5 6 7 8 ...
  .. .. .. .. .. .. .. .. .. .. ..$ pval       : num [1:24] 0 0 0.0421 0.0836 0.0597 ...
  .. .. .. .. .. .. .. .. .. ..$ rsd.pacf     :'data.frame':	24 obs. of  3 variables:
  .. .. .. .. .. .. .. .. .. .. ..$ lag        : num [1:24] 1 2 3 4 5 6 7 8 9 10 ...
  .. .. .. .. .. .. .. .. .. .. ..$ sample.pacf: num [1:24] -0.0294 -0.1081 -0.143 -0.1041 0.0968 ...
  .. .. .. .. .. .. .. .. .. .. ..$ stderr.pacf: num [1:24] 0.0874 0.0874 0.0874 0.0874 0.0874 ...
  .. .. .. .. .. .. .. .. .. ..$ rsd.acf2     :'data.frame':	12 obs. of  3 variables:
  .. .. .. .. .. .. .. .. .. .. ..$ lag        : num [1:12] 1 2 3 4 5 6 7 8 9 10 ...
  .. .. .. .. .. .. .. .. .. .. ..$ sample.acf2: num [1:12] 0.14497 -0.01054 0.00704 0.00531 0.11714 ...
  .. .. .. .. .. .. .. .. .. .. ..$ stderr.acf2: num [1:12] 0.0874 0.0892 0.0892 0.0892 0.0892 ...
  .. .. .. .. .. .. .. .. ..@ seats    : logi FALSE
  .. .. .. .. .. .. .. .. ..@ file     : chr "Series_1"
  .. .. .. .. .. .. .. .. ..@ tblnames : chr [1:11] "a1" "d10" "d11" "d12" ...
  .. .. .. .. .. .. .. .. ..@ Rtblnames: chr [1:11] "Original series" "Final seasonal factors" "Final seasonally adjusted data" "Final trend cycle" ...
  .. .. .. .. .. ..@ tsName         : chr "Series_1"
  .. .. .. .. .. ..@ firstRun       : logi TRUE
  .. .. .. ..$ :Formal class 'x12Single' [package "x12"] with 7 slots
  .. .. .. .. .. ..@ ts             : Time-Series [1:144] from 1949 to 1961: 112 118 132 129 121 135 148 148 136 119 ...
  .. .. .. .. .. ..@ x12Parameter   :Formal class 'x12Parameter' [package "x12"] with 42 slots
  .. .. .. .. .. .. .. ..@ span            : NULL
  .. .. .. .. .. .. .. ..@ modelspan       : NULL
  .. .. .. .. .. .. .. ..@ decimals        : num 2
  .. .. .. .. .. .. .. ..@ transform       : chr "auto"
  .. .. .. .. .. .. .. ..@ arima           : num [1:3] 0 1 1
  .. .. .. .. .. .. .. ..@ sarima          : num [1:3] 0 1 1
  .. .. .. .. .. .. .. ..@ automdl         : logi FALSE
  .. .. .. .. .. .. .. ..@ acceptdefault   : logi FALSE
  .. .. .. .. .. .. .. ..@ balanced        : logi TRUE
  .. .. .. .. .. .. .. ..@ maxorder        : num [1:2] 3 2
  .. .. .. .. .. .. .. ..@ maxdiff         : num [1:2] 1 1
  .. .. .. .. .. .. .. ..@ regvariables    : NULL
  .. .. .. .. .. .. .. ..@ reguser         : NULL
  .. .. .. .. .. .. .. ..@ regfile         : NULL
  .. .. .. .. .. .. .. ..@ usertype        : NULL
  .. .. .. .. .. .. .. ..@ centeruser      : NULL
  .. .. .. .. .. .. .. ..@ regfilestart    : NULL
  .. .. .. .. .. .. .. ..@ seats           : logi FALSE
  .. .. .. .. .. .. .. ..@ seatsparameter  : NULL
  .. .. .. .. .. .. .. ..@ sigmalim        : NULL
  .. .. .. .. .. .. .. ..@ outlier         : NULL
  .. .. .. .. .. .. .. ..@ critical        : NULL
  .. .. .. .. .. .. .. ..@ outlier_span    : NULL
  .. .. .. .. .. .. .. ..@ outlier_method  : NULL
  .. .. .. .. .. .. .. ..@ forecast_years  : NULL
  .. .. .. .. .. .. .. ..@ backcast_years  : NULL
  .. .. .. .. .. .. .. ..@ forecast_conf   : num 0.95
  .. .. .. .. .. .. .. ..@ estimate        : logi FALSE
  .. .. .. .. .. .. .. ..@ estOutofsample  : logi TRUE
  .. .. .. .. .. .. .. ..@ slidingspans    : logi FALSE
  .. .. .. .. .. .. .. ..@ aictest         : NULL
  .. .. .. .. .. .. .. ..@ onlytd          : logi FALSE
  .. .. .. .. .. .. .. ..@ sfshort         : logi FALSE
  .. .. .. .. .. .. .. ..@ samode          : NULL
  .. .. .. .. .. .. .. ..@ seasonalma      : NULL
  .. .. .. .. .. .. .. ..@ trendma         : NULL
  .. .. .. .. .. .. .. ..@ x11appendfcst   : logi TRUE
  .. .. .. .. .. .. .. ..@ x11appendbcst   : logi FALSE
  .. .. .. .. .. .. .. ..@ x11calendarsigma: NULL
  .. .. .. .. .. .. .. ..@ x11excludefcst  : logi TRUE
  .. .. .. .. .. .. .. ..@ x11final        : chr "user"
  .. .. .. .. .. .. .. ..@ x11regress      : logi FALSE
  .. .. .. .. .. ..@ x12Output      :Formal class 'x12Output' [package "x12"] with 22 slots
  .. .. .. .. .. .. .. ..@ a1       : Time-Series [1:144] from 1949 to 1961: 112 118 132 129 121 135 148 148 136 119 ...
  .. .. .. .. .. .. .. ..@ d10      : Time-Series [1:156] from 1949 to 1962: 0.899 0.947 1.057 0.999 0.967 ...
  .. .. .. .. .. .. .. ..@ d11      : Time-Series [1:144] from 1949 to 1961: 125 125 125 129 125 ...
  .. .. .. .. .. .. .. ..@ d12      : Time-Series [1:144] from 1949 to 1961: 124 125 126 126 126 ...
  .. .. .. .. .. .. .. ..@ d13      : Time-Series [1:144] from 1949 to 1961: 1.001 0.997 0.993 1.022 0.99 ...
  .. .. .. .. .. .. .. ..@ d16      : Time-Series [1:156] from 1949 to 1962: 0.899 0.947 1.057 0.999 0.967 ...
  .. .. .. .. .. .. .. ..@ c17      : Time-Series [1:144] from 1949 to 1961: 1 1 1 0.833 1 ...
  .. .. .. .. .. .. .. ..@ d9       : Time-Series [1:144] from 1949 to 1961: NA NA NA 1.02 NA ...
  .. .. .. .. .. .. .. ..@ e2       : Time-Series [1:144] from 1949 to 1961: 125 125 125 129 125 ...
  .. .. .. .. .. .. .. ..@ d8       : Time-Series [1:144] from 1949 to 1961: 0.903 0.944 1.049 1.02 0.957 ...
  .. .. .. .. .. .. .. ..@ b1       : Time-Series [1:156] from 1949 to 1962: 112 118 132 129 121 135 148 148 136 119 ...
  .. .. .. .. .. .. .. ..@ sp0      :Formal class 'spectrum' [package "x12"] with 2 slots
  .. .. .. .. .. .. .. .. .. ..@ frequency: num [1:61] 0 0.00833 0.01667 0.025 0.03333 ...
  .. .. .. .. .. .. .. .. .. ..@ spectrum : num [1:61] -36.1 -35.4 -33.4 -31.3 -30.7 ...
  .. .. .. .. .. .. .. ..@ sp1      :Formal class 'spectrum' [package "x12"] with 2 slots
  .. .. .. .. .. .. .. .. .. ..@ frequency: num [1:61] 0 0.00833 0.01667 0.025 0.03333 ...
  .. .. .. .. .. .. .. .. .. ..@ spectrum : num [1:61] -42.2 -40.8 -36.4 -33.8 -37.1 ...
  .. .. .. .. .. .. .. ..@ sp2      :Formal class 'spectrum' [package "x12"] with 2 slots
  .. .. .. .. .. .. .. .. .. ..@ frequency: num [1:61] 0 0.00833 0.01667 0.025 0.03333 ...
  .. .. .. .. .. .. .. .. .. ..@ spectrum : num [1:61] -42.6 -46.6 -48.3 -45.5 -47.4 ...
  .. .. .. .. .. .. .. ..@ spr      :Formal class 'spectrum' [package "x12"] with 2 slots
  .. .. .. .. .. .. .. .. .. ..@ frequency: num [1:61] 0 0.00833 0.01667 0.025 0.03333 ...
  .. .. .. .. .. .. .. .. .. ..@ spectrum : num [1:61] -37.3 -35.8 -31.1 -26.2 -30.2 ...
  .. .. .. .. .. .. .. ..@ forecast :Formal class 'fbcast' [package "x12"] with 3 slots
  .. .. .. .. .. .. .. .. .. ..@ estimate: Time-Series [1:12] from 1961 to 1962: 450 426 479 492 509 ...
  .. .. .. .. .. .. .. .. .. ..@ lowerci : Time-Series [1:12] from 1961 to 1962: 419 391 436 444 455 ...
  .. .. .. .. .. .. .. .. .. ..@ upperci : Time-Series [1:12] from 1961 to 1962: 484 463 526 546 570 ...
  .. .. .. .. .. .. .. ..@ backcast :Formal class 'fbcast' [package "x12"] with 3 slots
  .. .. .. .. .. .. .. .. .. ..@ estimate:Formal class 'ts' [package "methods"] with 3 slots
  .. .. .. .. .. .. .. .. .. .. .. ..@ .Data   : logi NA
  .. .. .. .. .. .. .. .. .. .. .. ..@ tsp     : num [1:3] 1 1 1
  .. .. .. .. .. .. .. .. .. .. .. ..@ .S3Class: chr "ts"
  .. .. .. .. .. .. .. .. .. ..@ lowerci :Formal class 'ts' [package "methods"] with 3 slots
  .. .. .. .. .. .. .. .. .. .. .. ..@ .Data   : logi NA
  .. .. .. .. .. .. .. .. .. .. .. ..@ tsp     : num [1:3] 1 1 1
  .. .. .. .. .. .. .. .. .. .. .. ..@ .S3Class: chr "ts"
  .. .. .. .. .. .. .. .. .. ..@ upperci :Formal class 'ts' [package "methods"] with 3 slots
  .. .. .. .. .. .. .. .. .. .. .. ..@ .Data   : logi NA
  .. .. .. .. .. .. .. .. .. .. .. ..@ tsp     : num [1:3] 1 1 1
  .. .. .. .. .. .. .. .. .. .. .. ..@ .S3Class: chr "ts"
  .. .. .. .. .. .. .. ..@ dg       :List of 38
  .. .. .. .. .. .. .. .. ..$ x11regress   :List of 1
  .. .. .. .. .. .. .. .. .. ..$ x11regress: chr "no"
  .. .. .. .. .. .. .. .. ..$ transform    :List of 1
  .. .. .. .. .. .. .. .. .. ..$ transform: chr "Automatic selection"
  .. .. .. .. .. .. .. .. ..$ samode       :List of 2
  .. .. .. .. .. .. .. .. .. ..$ samode     : chr "auto-mode seasonal adjustment"
  .. .. .. .. .. .. .. .. .. ..$ finalsamode: chr "multiplicative"
  .. .. .. .. .. .. .. .. ..$ seasonalma   :List of 2
  .. .. .. .. .. .. .. .. .. ..$ seasonalma     : chr "M.S.R."
  .. .. .. .. .. .. .. .. .. ..$ finalseasonalma: chr "3x3"
  .. .. .. .. .. .. .. .. ..$ trendma      :List of 2
  .. .. .. .. .. .. .. .. .. ..$ trendma     : chr "default"
  .. .. .. .. .. .. .. .. .. ..$ finaltrendma: num 9
  .. .. .. .. .. .. .. .. ..$ arimamdl     :List of 1
  .. .. .. .. .. .. .. .. .. ..$ arimamdl: chr "(0,1,1)(0,1,1)"
  .. .. .. .. .. .. .. .. ..$ automdl      :List of 1
  .. .. .. .. .. .. .. .. .. ..$ automdl: chr "-"
  .. .. .. .. .. .. .. .. ..$ regmdl       :List of 1
  .. .. .. .. .. .. .. .. .. ..$ regmdl: chr "none"
  .. .. .. .. .. .. .. .. ..$ nout         :List of 1
  .. .. .. .. .. .. .. .. .. ..$ nout: num 0
  .. .. .. .. .. .. .. .. ..$ nautoout     :List of 1
  .. .. .. .. .. .. .. .. .. ..$ nautoout: chr "-"
  .. .. .. .. .. .. .. .. ..$ nalmostout   :List of 1
  .. .. .. .. .. .. .. .. .. ..$ nalmostout: chr "-"
  .. .. .. .. .. .. .. .. ..$ almostoutlier:List of 1
  .. .. .. .. .. .. .. .. .. ..$ almostoutlier: chr "-"
  .. .. .. .. .. .. .. .. ..$ crit         :List of 1
  .. .. .. .. .. .. .. .. .. ..$ crit: chr "-"
  .. .. .. .. .. .. .. .. ..$ outlier      :List of 1
  .. .. .. .. .. .. .. .. .. ..$ outlier: chr "-"
  .. .. .. .. .. .. .. .. ..$ userdefined  :List of 1
  .. .. .. .. .. .. .. .. .. ..$ userdefined: chr "-"
  .. .. .. .. .. .. .. .. ..$ autooutlier  :List of 1
  .. .. .. .. .. .. .. .. .. ..$ autooutlier: chr "-"
  .. .. .. .. .. .. .. .. ..$ peaks.seas   :List of 1
  .. .. .. .. .. .. .. .. .. ..$ peaks.seas: chr "rsd"
  .. .. .. .. .. .. .. .. ..$ peaks.td     :List of 1
  .. .. .. .. .. .. .. .. .. ..$ peaks.td: chr "sa irr"
  .. .. .. .. .. .. .. .. ..$ id.seas      :List of 1
  .. .. .. .. .. .. .. .. .. ..$ id.seas: chr "yes"
  .. .. .. .. .. .. .. .. ..$ id.rsdseas   :List of 1
  .. .. .. .. .. .. .. .. .. ..$ id.rsdseas: chr "none"
  .. .. .. .. .. .. .. .. ..$ spcrsd       :List of 11
  .. .. .. .. .. .. .. .. .. ..$ spcrsd.t1   :List of 2
  .. .. .. .. .. .. .. .. .. .. ..$ : num 0.7
  .. .. .. .. .. .. .. .. .. .. ..$ : chr "+"
  .. .. .. .. .. .. .. .. .. ..$ spcrsd.t2   :List of 2
  .. .. .. .. .. .. .. .. .. .. ..$ : num 5
  .. .. .. .. .. .. .. .. .. .. ..$ : chr "+"
  .. .. .. .. .. .. .. .. .. ..$ spcrsd.t.dom: chr "t1"
  .. .. .. .. .. .. .. .. .. ..$ spcrsd.s1   :List of 2
  .. .. .. .. .. .. .. .. .. .. ..$ : num 16.9
  .. .. .. .. .. .. .. .. .. .. ..$ : chr "+"
  .. .. .. .. .. .. .. .. .. ..$ spcrsd.s2   : chr "nopeak"
  .. .. .. .. .. .. .. .. .. ..$ spcrsd.s3   : chr "nopeak"
  .. .. .. .. .. .. .. .. .. ..$ spcrsd.s4   : chr "nopeak"
  .. .. .. .. .. .. .. .. .. ..$ spcrsd.s5   : chr "nopeak"
  .. .. .. .. .. .. .. .. .. ..$ spcrsd.s6   : chr "nopeak"
  .. .. .. .. .. .. .. .. .. ..$ spcrsd.s.dom: chr "s1"
  .. .. .. .. .. .. .. .. .. ..$ spcrsd.dom  : chr "s1"
  .. .. .. .. .. .. .. .. ..$ spcori       :List of 11
  .. .. .. .. .. .. .. .. .. ..$ spcori.t1   : chr "nopeak"
  .. .. .. .. .. .. .. .. .. ..$ spcori.t2   : chr "nopeak"
  .. .. .. .. .. .. .. .. .. ..$ spcori.t.dom: chr "no"
  .. .. .. .. .. .. .. .. .. ..$ spcori.s1   :List of 2
  .. .. .. .. .. .. .. .. .. .. ..$ : num 20.7
  .. .. .. .. .. .. .. .. .. .. ..$ : chr "+"
  .. .. .. .. .. .. .. .. .. ..$ spcori.s2   :List of 2
  .. .. .. .. .. .. .. .. .. .. ..$ : num 17.7
  .. .. .. .. .. .. .. .. .. .. ..$ : chr "+"
  .. .. .. .. .. .. .. .. .. ..$ spcori.s3   :List of 2
  .. .. .. .. .. .. .. .. .. .. ..$ : num 13.8
  .. .. .. .. .. .. .. .. .. .. ..$ : chr "+"
  .. .. .. .. .. .. .. .. .. ..$ spcori.s4   :List of 2
  .. .. .. .. .. .. .. .. .. .. ..$ : num 16.5
  .. .. .. .. .. .. .. .. .. .. ..$ : chr "+"
  .. .. .. .. .. .. .. .. .. ..$ spcori.s5   :List of 2
  .. .. .. .. .. .. .. .. .. .. ..$ : num 8.5
  .. .. .. .. .. .. .. .. .. .. ..$ : chr "+"
  .. .. .. .. .. .. .. .. .. ..$ spcori.s6   : chr "nopeak"
  .. .. .. .. .. .. .. .. .. ..$ spcori.s.dom: chr "s1"
  .. .. .. .. .. .. .. .. .. ..$ spcori.dom  : chr "s1"
  .. .. .. .. .. .. .. .. ..$ spcsa        :List of 11
  .. .. .. .. .. .. .. .. .. ..$ spcsa.t1   : chr "nopeak"
  .. .. .. .. .. .. .. .. .. ..$ spcsa.t2   :List of 2
  .. .. .. .. .. .. .. .. .. .. ..$ : num 13.5
  .. .. .. .. .. .. .. .. .. .. ..$ : chr "+"
  .. .. .. .. .. .. .. .. .. ..$ spcsa.t.dom: chr "t2"
  .. .. .. .. .. .. .. .. .. ..$ spcsa.s1   :List of 2
  .. .. .. .. .. .. .. .. .. .. ..$ : num 5.7
  .. .. .. .. .. .. .. .. .. .. ..$ : chr "+"
  .. .. .. .. .. .. .. .. .. ..$ spcsa.s2   : chr "nopeak"
  .. .. .. .. .. .. .. .. .. ..$ spcsa.s3   : chr "nopeak"
  .. .. .. .. .. .. .. .. .. ..$ spcsa.s4   : chr "nopeak"
  .. .. .. .. .. .. .. .. .. ..$ spcsa.s5   : chr "nopeak"
  .. .. .. .. .. .. .. .. .. ..$ spcsa.s6   : chr "nopeak"
  .. .. .. .. .. .. .. .. .. ..$ spcsa.s.dom: chr "s1"
  .. .. .. .. .. .. .. .. .. ..$ spcsa.dom  : chr "no"
  .. .. .. .. .. .. .. .. ..$ spcirr       :List of 11
  .. .. .. .. .. .. .. .. .. ..$ spcirr.t1   : chr "nopeak"
  .. .. .. .. .. .. .. .. .. ..$ spcirr.t2   :List of 2
  .. .. .. .. .. .. .. .. .. .. ..$ : num 11
  .. .. .. .. .. .. .. .. .. .. ..$ : chr "+"
  .. .. .. .. .. .. .. .. .. ..$ spcirr.t.dom: chr "t2"
  .. .. .. .. .. .. .. .. .. ..$ spcirr.s1   : chr "nopeak"
  .. .. .. .. .. .. .. .. .. ..$ spcirr.s2   : chr "nopeak"
  .. .. .. .. .. .. .. .. .. ..$ spcirr.s3   : chr "nopeak"
  .. .. .. .. .. .. .. .. .. ..$ spcirr.s4   : chr "nopeak"
  .. .. .. .. .. .. .. .. .. ..$ spcirr.s5   : chr "nopeak"
  .. .. .. .. .. .. .. .. .. ..$ spcirr.s6   : chr "nopeak"
  .. .. .. .. .. .. .. .. .. ..$ spcirr.s.dom: chr "no"
  .. .. .. .. .. .. .. .. .. ..$ spcirr.dom  : chr "no"
  .. .. .. .. .. .. .. .. ..$ q            :List of 1
  .. .. .. .. .. .. .. .. .. ..$ q: num 0.26
  .. .. .. .. .. .. .. .. ..$ q2           :List of 1
  .. .. .. .. .. .. .. .. .. ..$ q2: num 0.29
  .. .. .. .. .. .. .. .. ..$ nmfail       :List of 1
  .. .. .. .. .. .. .. .. .. ..$ nmfail: num 0
  .. .. .. .. .. .. .. .. ..$ loglikelihood:List of 1
  .. .. .. .. .. .. .. .. .. ..$ loglikelihood: num 245
  .. .. .. .. .. .. .. .. ..$ aic          :List of 1
  .. .. .. .. .. .. .. .. .. ..$ aic: num 987
  .. .. .. .. .. .. .. .. ..$ aicc         :List of 1
  .. .. .. .. .. .. .. .. .. ..$ aicc: num 987
  .. .. .. .. .. .. .. .. ..$ bic          :List of 1
  .. .. .. .. .. .. .. .. .. ..$ bic: num 996
  .. .. .. .. .. .. .. .. ..$ hq           :List of 1
  .. .. .. .. .. .. .. .. .. ..$ hq: num 991
  .. .. .. .. .. .. .. .. ..$ aape         :List of 5
  .. .. .. .. .. .. .. .. .. ..$ aape.mode: chr "withinsample"
  .. .. .. .. .. .. .. .. .. ..$ aape.0   : num 5.63
  .. .. .. .. .. .. .. .. .. ..$ aape.1   : num 2.81
  .. .. .. .. .. .. .. .. .. ..$ aape.2   : num 6.38
  .. .. .. .. .. .. .. .. .. ..$ aape.3   : num 7.69
  .. .. .. .. .. .. .. .. ..$ autotransform:List of 1
  .. .. .. .. .. .. .. .. .. ..$ autotransform: chr "Log(y)"
  .. .. .. .. .. .. .. .. ..$ ifout        : chr "No outlier detection performed"
  .. .. .. .. .. .. .. .. ..$ rsd.acf      :'data.frame':	24 obs. of  6 variables:
  .. .. .. .. .. .. .. .. .. ..$ lag        : num [1:24] 1 2 3 4 5 6 7 8 9 10 ...
  .. .. .. .. .. .. .. .. .. ..$ sample.acf : num [1:24] 0.0121 0.0222 -0.1284 -0.1255 0.0599 ...
  .. .. .. .. .. .. .. .. .. ..$ stderr.acf : num [1:24] 0.0874 0.0874 0.0874 0.0889 0.0902 ...
  .. .. .. .. .. .. .. .. .. ..$ Ljung-Box.q: num [1:24] 0.0195 0.0859 2.3301 4.4896 4.9855 ...
  .. .. .. .. .. .. .. .. .. ..$ df.q       : num [1:24] 0 0 1 2 3 4 5 6 7 8 ...
  .. .. .. .. .. .. .. .. .. ..$ pval       : num [1:24] 0 0 0.127 0.106 0.173 ...
  .. .. .. .. .. .. .. .. ..$ rsd.pacf     :'data.frame':	24 obs. of  3 variables:
  .. .. .. .. .. .. .. .. .. ..$ lag        : num [1:24] 1 2 3 4 5 6 7 8 9 10 ...
  .. .. .. .. .. .. .. .. .. ..$ sample.pacf: num [1:24] 0.0121 0.022 -0.129 -0.1247 0.0697 ...
  .. .. .. .. .. .. .. .. .. ..$ stderr.pacf: num [1:24] 0.0874 0.0874 0.0874 0.0874 0.0874 ...
  .. .. .. .. .. .. .. .. ..$ rsd.acf2     :'data.frame':	12 obs. of  3 variables:
  .. .. .. .. .. .. .. .. .. ..$ lag        : num [1:12] 1 2 3 4 5 6 7 8 9 10 ...
  .. .. .. .. .. .. .. .. .. ..$ sample.acf2: num [1:12] 0.1182 0.00644 0.02262 -0.03606 0.098 ...
  .. .. .. .. .. .. .. .. .. ..$ stderr.acf2: num [1:12] 0.0874 0.0886 0.0886 0.0886 0.0887 ...
  .. .. .. .. .. .. .. ..@ seats    : logi FALSE
  .. .. .. .. .. .. .. ..@ file     : chr "Series_2"
  .. .. .. .. .. .. .. ..@ tblnames : chr [1:11] "a1" "d10" "d11" "d12" ...
  .. .. .. .. .. .. .. ..@ Rtblnames: chr [1:11] "Original series" "Final seasonal factors" "Final seasonally adjusted data" "Final trend cycle" ...
  .. .. .. .. .. ..@ x12OldParameter:List of 1
  .. .. .. .. .. .. ..$ :Formal class 'x12Parameter' [package "x12"] with 42 slots
  .. .. .. .. .. .. .. .. ..@ span            : NULL
  .. .. .. .. .. .. .. .. ..@ modelspan       : NULL
  .. .. .. .. .. .. .. .. ..@ decimals        : num 2
  .. .. .. .. .. .. .. .. ..@ transform       : chr "auto"
  .. .. .. .. .. .. .. .. ..@ arima           : num [1:3] 0 1 1
  .. .. .. .. .. .. .. .. ..@ sarima          : num [1:3] 0 1 1
  .. .. .. .. .. .. .. .. ..@ automdl         : logi FALSE
  .. .. .. .. .. .. .. .. ..@ acceptdefault   : logi FALSE
  .. .. .. .. .. .. .. .. ..@ balanced        : logi TRUE
  .. .. .. .. .. .. .. .. ..@ maxorder        : num [1:2] 3 2
  .. .. .. .. .. .. .. .. ..@ maxdiff         : num [1:2] 1 1
  .. .. .. .. .. .. .. .. ..@ regvariables    : NULL
  .. .. .. .. .. .. .. .. ..@ reguser         : NULL
  .. .. .. .. .. .. .. .. ..@ regfile         : NULL
  .. .. .. .. .. .. .. .. ..@ usertype        : NULL
  .. .. .. .. .. .. .. .. ..@ centeruser      : NULL
  .. .. .. .. .. .. .. .. ..@ regfilestart    : NULL
  .. .. .. .. .. .. .. .. ..@ seats           : logi FALSE
  .. .. .. .. .. .. .. .. ..@ seatsparameter  : NULL
  .. .. .. .. .. .. .. .. ..@ sigmalim        : NULL
  .. .. .. .. .. .. .. .. ..@ outlier         : NULL
  .. .. .. .. .. .. .. .. ..@ critical        : NULL
  .. .. .. .. .. .. .. .. ..@ outlier_span    : NULL
  .. .. .. .. .. .. .. .. ..@ outlier_method  : NULL
  .. .. .. .. .. .. .. .. ..@ forecast_years  : NULL
  .. .. .. .. .. .. .. .. ..@ backcast_years  : NULL
  .. .. .. .. .. .. .. .. ..@ forecast_conf   : num 0.95
  .. .. .. .. .. .. .. .. ..@ estimate        : logi FALSE
  .. .. .. .. .. .. .. .. ..@ estOutofsample  : logi TRUE
  .. .. .. .. .. .. .. .. ..@ slidingspans    : logi FALSE
  .. .. .. .. .. .. .. .. ..@ aictest         : NULL
  .. .. .. .. .. .. .. .. ..@ onlytd          : logi FALSE
  .. .. .. .. .. .. .. .. ..@ sfshort         : logi FALSE
  .. .. .. .. .. .. .. .. ..@ samode          : NULL
  .. .. .. .. .. .. .. .. ..@ seasonalma      : NULL
  .. .. .. .. .. .. .. .. ..@ trendma         : NULL
  .. .. .. .. .. .. .. .. ..@ x11appendfcst   : logi TRUE
  .. .. .. .. .. .. .. .. ..@ x11appendbcst   : logi FALSE
  .. .. .. .. .. .. .. .. ..@ x11calendarsigma: NULL
  .. .. .. .. .. .. .. .. ..@ x11excludefcst  : logi TRUE
  .. .. .. .. .. .. .. .. ..@ x11final        : chr "user"
  .. .. .. .. .. .. .. .. ..@ x11regress      : logi FALSE
  .. .. .. .. .. ..@ x12OldOutput   :List of 1
  .. .. .. .. .. .. ..$ :Formal class 'x12Output' [package "x12"] with 22 slots
  .. .. .. .. .. .. .. .. ..@ a1       : Time-Series [1:144] from 1949 to 1961: 112 118 132 129 121 135 148 148 136 119 ...
  .. .. .. .. .. .. .. .. ..@ d10      : Time-Series [1:156] from 1949 to 1962: 0.899 0.947 1.057 0.999 0.967 ...
  .. .. .. .. .. .. .. .. ..@ d11      : Time-Series [1:144] from 1949 to 1961: 125 125 125 129 125 ...
  .. .. .. .. .. .. .. .. ..@ d12      : Time-Series [1:144] from 1949 to 1961: 124 125 126 126 126 ...
  .. .. .. .. .. .. .. .. ..@ d13      : Time-Series [1:144] from 1949 to 1961: 1.001 0.997 0.993 1.022 0.99 ...
  .. .. .. .. .. .. .. .. ..@ d16      : Time-Series [1:156] from 1949 to 1962: 0.899 0.947 1.057 0.999 0.967 ...
  .. .. .. .. .. .. .. .. ..@ c17      : Time-Series [1:144] from 1949 to 1961: 1 1 1 0.833 1 ...
  .. .. .. .. .. .. .. .. ..@ d9       : Time-Series [1:144] from 1949 to 1961: NA NA NA 1.02 NA ...
  .. .. .. .. .. .. .. .. ..@ e2       : Time-Series [1:144] from 1949 to 1961: 125 125 125 129 125 ...
  .. .. .. .. .. .. .. .. ..@ d8       : Time-Series [1:144] from 1949 to 1961: 0.903 0.944 1.049 1.02 0.957 ...
  .. .. .. .. .. .. .. .. ..@ b1       : Time-Series [1:156] from 1949 to 1962: 112 118 132 129 121 135 148 148 136 119 ...
  .. .. .. .. .. .. .. .. ..@ sp0      :Formal class 'spectrum' [package "x12"] with 2 slots
  .. .. .. .. .. .. .. .. .. .. ..@ frequency: num [1:61] 0 0.00833 0.01667 0.025 0.03333 ...
  .. .. .. .. .. .. .. .. .. .. ..@ spectrum : num [1:61] -36.1 -35.4 -33.4 -31.3 -30.7 ...
  .. .. .. .. .. .. .. .. ..@ sp1      :Formal class 'spectrum' [package "x12"] with 2 slots
  .. .. .. .. .. .. .. .. .. .. ..@ frequency: num [1:61] 0 0.00833 0.01667 0.025 0.03333 ...
  .. .. .. .. .. .. .. .. .. .. ..@ spectrum : num [1:61] -42.2 -40.8 -36.4 -33.8 -37.1 ...
  .. .. .. .. .. .. .. .. ..@ sp2      :Formal class 'spectrum' [package "x12"] with 2 slots
  .. .. .. .. .. .. .. .. .. .. ..@ frequency: num [1:61] 0 0.00833 0.01667 0.025 0.03333 ...
  .. .. .. .. .. .. .. .. .. .. ..@ spectrum : num [1:61] -42.6 -46.6 -48.3 -45.5 -47.4 ...
  .. .. .. .. .. .. .. .. ..@ spr      :Formal class 'spectrum' [package "x12"] with 2 slots
  .. .. .. .. .. .. .. .. .. .. ..@ frequency: num [1:61] 0 0.00833 0.01667 0.025 0.03333 ...
  .. .. .. .. .. .. .. .. .. .. ..@ spectrum : num [1:61] -37.3 -35.8 -31.1 -26.2 -30.2 ...
  .. .. .. .. .. .. .. .. ..@ forecast :Formal class 'fbcast' [package "x12"] with 3 slots
  .. .. .. .. .. .. .. .. .. .. ..@ estimate: Time-Series [1:12] from 1961 to 1962: 450 426 479 492 509 ...
  .. .. .. .. .. .. .. .. .. .. ..@ lowerci : Time-Series [1:12] from 1961 to 1962: 419 391 436 444 455 ...
  .. .. .. .. .. .. .. .. .. .. ..@ upperci : Time-Series [1:12] from 1961 to 1962: 484 463 526 546 570 ...
  .. .. .. .. .. .. .. .. ..@ backcast :Formal class 'fbcast' [package "x12"] with 3 slots
  .. .. .. .. .. .. .. .. .. .. ..@ estimate:Formal class 'ts' [package "methods"] with 3 slots
  .. .. .. .. .. .. .. .. .. .. .. .. ..@ .Data   : logi NA
  .. .. .. .. .. .. .. .. .. .. .. .. ..@ tsp     : num [1:3] 1 1 1
  .. .. .. .. .. .. .. .. .. .. .. .. ..@ .S3Class: chr "ts"
  .. .. .. .. .. .. .. .. .. .. ..@ lowerci :Formal class 'ts' [package "methods"] with 3 slots
  .. .. .. .. .. .. .. .. .. .. .. .. ..@ .Data   : logi NA
  .. .. .. .. .. .. .. .. .. .. .. .. ..@ tsp     : num [1:3] 1 1 1
  .. .. .. .. .. .. .. .. .. .. .. .. ..@ .S3Class: chr "ts"
  .. .. .. .. .. .. .. .. .. .. ..@ upperci :Formal class 'ts' [package "methods"] with 3 slots
  .. .. .. .. .. .. .. .. .. .. .. .. ..@ .Data   : logi NA
  .. .. .. .. .. .. .. .. .. .. .. .. ..@ tsp     : num [1:3] 1 1 1
  .. .. .. .. .. .. .. .. .. .. .. .. ..@ .S3Class: chr "ts"
  .. .. .. .. .. .. .. .. ..@ dg       :List of 38
  .. .. .. .. .. .. .. .. .. ..$ x11regress   :List of 1
  .. .. .. .. .. .. .. .. .. .. ..$ x11regress: chr "no"
  .. .. .. .. .. .. .. .. .. ..$ transform    :List of 1
  .. .. .. .. .. .. .. .. .. .. ..$ transform: chr "Automatic selection"
  .. .. .. .. .. .. .. .. .. ..$ samode       :List of 2
  .. .. .. .. .. .. .. .. .. .. ..$ samode     : chr "auto-mode seasonal adjustment"
  .. .. .. .. .. .. .. .. .. .. ..$ finalsamode: chr "multiplicative"
  .. .. .. .. .. .. .. .. .. ..$ seasonalma   :List of 2
  .. .. .. .. .. .. .. .. .. .. ..$ seasonalma     : chr "M.S.R."
  .. .. .. .. .. .. .. .. .. .. ..$ finalseasonalma: chr "3x3"
  .. .. .. .. .. .. .. .. .. ..$ trendma      :List of 2
  .. .. .. .. .. .. .. .. .. .. ..$ trendma     : chr "default"
  .. .. .. .. .. .. .. .. .. .. ..$ finaltrendma: num 9
  .. .. .. .. .. .. .. .. .. ..$ arimamdl     :List of 1
  .. .. .. .. .. .. .. .. .. .. ..$ arimamdl: chr "(0,1,1)(0,1,1)"
  .. .. .. .. .. .. .. .. .. ..$ automdl      :List of 1
  .. .. .. .. .. .. .. .. .. .. ..$ automdl: chr "-"
  .. .. .. .. .. .. .. .. .. ..$ regmdl       :List of 1
  .. .. .. .. .. .. .. .. .. .. ..$ regmdl: chr "none"
  .. .. .. .. .. .. .. .. .. ..$ nout         :List of 1
  .. .. .. .. .. .. .. .. .. .. ..$ nout: num 0
  .. .. .. .. .. .. .. .. .. ..$ nautoout     :List of 1
  .. .. .. .. .. .. .. .. .. .. ..$ nautoout: chr "-"
  .. .. .. .. .. .. .. .. .. ..$ nalmostout   :List of 1
  .. .. .. .. .. .. .. .. .. .. ..$ nalmostout: chr "-"
  .. .. .. .. .. .. .. .. .. ..$ almostoutlier:List of 1
  .. .. .. .. .. .. .. .. .. .. ..$ almostoutlier: chr "-"
  .. .. .. .. .. .. .. .. .. ..$ crit         :List of 1
  .. .. .. .. .. .. .. .. .. .. ..$ crit: chr "-"
  .. .. .. .. .. .. .. .. .. ..$ outlier      :List of 1
  .. .. .. .. .. .. .. .. .. .. ..$ outlier: chr "-"
  .. .. .. .. .. .. .. .. .. ..$ userdefined  :List of 1
  .. .. .. .. .. .. .. .. .. .. ..$ userdefined: chr "-"
  .. .. .. .. .. .. .. .. .. ..$ autooutlier  :List of 1
  .. .. .. .. .. .. .. .. .. .. ..$ autooutlier: chr "-"
  .. .. .. .. .. .. .. .. .. ..$ peaks.seas   :List of 1
  .. .. .. .. .. .. .. .. .. .. ..$ peaks.seas: chr "rsd"
  .. .. .. .. .. .. .. .. .. ..$ peaks.td     :List of 1
  .. .. .. .. .. .. .. .. .. .. ..$ peaks.td: chr "sa irr"
  .. .. .. .. .. .. .. .. .. ..$ id.seas      :List of 1
  .. .. .. .. .. .. .. .. .. .. ..$ id.seas: chr "yes"
  .. .. .. .. .. .. .. .. .. ..$ id.rsdseas   :List of 1
  .. .. .. .. .. .. .. .. .. .. ..$ id.rsdseas: chr "none"
  .. .. .. .. .. .. .. .. .. ..$ spcrsd       :List of 11
  .. .. .. .. .. .. .. .. .. .. ..$ spcrsd.t1   :List of 2
  .. .. .. .. .. .. .. .. .. .. .. ..$ : num 0.7
  .. .. .. .. .. .. .. .. .. .. .. ..$ : chr "+"
  .. .. .. .. .. .. .. .. .. .. ..$ spcrsd.t2   :List of 2
  .. .. .. .. .. .. .. .. .. .. .. ..$ : num 5
  .. .. .. .. .. .. .. .. .. .. .. ..$ : chr "+"
  .. .. .. .. .. .. .. .. .. .. ..$ spcrsd.t.dom: chr "t1"
  .. .. .. .. .. .. .. .. .. .. ..$ spcrsd.s1   :List of 2
  .. .. .. .. .. .. .. .. .. .. .. ..$ : num 16.9
  .. .. .. .. .. .. .. .. .. .. .. ..$ : chr "+"
  .. .. .. .. .. .. .. .. .. .. ..$ spcrsd.s2   : chr "nopeak"
  .. .. .. .. .. .. .. .. .. .. ..$ spcrsd.s3   : chr "nopeak"
  .. .. .. .. .. .. .. .. .. .. ..$ spcrsd.s4   : chr "nopeak"
  .. .. .. .. .. .. .. .. .. .. ..$ spcrsd.s5   : chr "nopeak"
  .. .. .. .. .. .. .. .. .. .. ..$ spcrsd.s6   : chr "nopeak"
  .. .. .. .. .. .. .. .. .. .. ..$ spcrsd.s.dom: chr "s1"
  .. .. .. .. .. .. .. .. .. .. ..$ spcrsd.dom  : chr "s1"
  .. .. .. .. .. .. .. .. .. ..$ spcori       :List of 11
  .. .. .. .. .. .. .. .. .. .. ..$ spcori.t1   : chr "nopeak"
  .. .. .. .. .. .. .. .. .. .. ..$ spcori.t2   : chr "nopeak"
  .. .. .. .. .. .. .. .. .. .. ..$ spcori.t.dom: chr "no"
  .. .. .. .. .. .. .. .. .. .. ..$ spcori.s1   :List of 2
  .. .. .. .. .. .. .. .. .. .. .. ..$ : num 20.7
  .. .. .. .. .. .. .. .. .. .. .. ..$ : chr "+"
  .. .. .. .. .. .. .. .. .. .. ..$ spcori.s2   :List of 2
  .. .. .. .. .. .. .. .. .. .. .. ..$ : num 17.7
  .. .. .. .. .. .. .. .. .. .. .. ..$ : chr "+"
  .. .. .. .. .. .. .. .. .. .. ..$ spcori.s3   :List of 2
  .. .. .. .. .. .. .. .. .. .. .. ..$ : num 13.8
  .. .. .. .. .. .. .. .. .. .. .. ..$ : chr "+"
  .. .. .. .. .. .. .. .. .. .. ..$ spcori.s4   :List of 2
  .. .. .. .. .. .. .. .. .. .. .. ..$ : num 16.5
  .. .. .. .. .. .. .. .. .. .. .. ..$ : chr "+"
  .. .. .. .. .. .. .. .. .. .. ..$ spcori.s5   :List of 2
  .. .. .. .. .. .. .. .. .. .. .. ..$ : num 8.5
  .. .. .. .. .. .. .. .. .. .. .. ..$ : chr "+"
  .. .. .. .. .. .. .. .. .. .. ..$ spcori.s6   : chr "nopeak"
  .. .. .. .. .. .. .. .. .. .. ..$ spcori.s.dom: chr "s1"
  .. .. .. .. .. .. .. .. .. .. ..$ spcori.dom  : chr "s1"
  .. .. .. .. .. .. .. .. .. ..$ spcsa        :List of 11
  .. .. .. .. .. .. .. .. .. .. ..$ spcsa.t1   : chr "nopeak"
  .. .. .. .. .. .. .. .. .. .. ..$ spcsa.t2   :List of 2
  .. .. .. .. .. .. .. .. .. .. .. ..$ : num 13.5
  .. .. .. .. .. .. .. .. .. .. .. ..$ : chr "+"
  .. .. .. .. .. .. .. .. .. .. ..$ spcsa.t.dom: chr "t2"
  .. .. .. .. .. .. .. .. .. .. ..$ spcsa.s1   :List of 2
  .. .. .. .. .. .. .. .. .. .. .. ..$ : num 5.7
  .. .. .. .. .. .. .. .. .. .. .. ..$ : chr "+"
  .. .. .. .. .. .. .. .. .. .. ..$ spcsa.s2   : chr "nopeak"
  .. .. .. .. .. .. .. .. .. .. ..$ spcsa.s3   : chr "nopeak"
  .. .. .. .. .. .. .. .. .. .. ..$ spcsa.s4   : chr "nopeak"
  .. .. .. .. .. .. .. .. .. .. ..$ spcsa.s5   : chr "nopeak"
  .. .. .. .. .. .. .. .. .. .. ..$ spcsa.s6   : chr "nopeak"
  .. .. .. .. .. .. .. .. .. .. ..$ spcsa.s.dom: chr "s1"
  .. .. .. .. .. .. .. .. .. .. ..$ spcsa.dom  : chr "no"
  .. .. .. .. .. .. .. .. .. ..$ spcirr       :List of 11
  .. .. .. .. .. .. .. .. .. .. ..$ spcirr.t1   : chr "nopeak"
  .. .. .. .. .. .. .. .. .. .. ..$ spcirr.t2   :List of 2
  .. .. .. .. .. .. .. .. .. .. .. ..$ : num 11
  .. .. .. .. .. .. .. .. .. .. .. ..$ : chr "+"
  .. .. .. .. .. .. .. .. .. .. ..$ spcirr.t.dom: chr "t2"
  .. .. .. .. .. .. .. .. .. .. ..$ spcirr.s1   : chr "nopeak"
  .. .. .. .. .. .. .. .. .. .. ..$ spcirr.s2   : chr "nopeak"
  .. .. .. .. .. .. .. .. .. .. ..$ spcirr.s3   : chr "nopeak"
  .. .. .. .. .. .. .. .. .. .. ..$ spcirr.s4   : chr "nopeak"
  .. .. .. .. .. .. .. .. .. .. ..$ spcirr.s5   : chr "nopeak"
  .. .. .. .. .. .. .. .. .. .. ..$ spcirr.s6   : chr "nopeak"
  .. .. .. .. .. .. .. .. .. .. ..$ spcirr.s.dom: chr "no"
  .. .. .. .. .. .. .. .. .. .. ..$ spcirr.dom  : chr "no"
  .. .. .. .. .. .. .. .. .. ..$ q            :List of 1
  .. .. .. .. .. .. .. .. .. .. ..$ q: num 0.26
  .. .. .. .. .. .. .. .. .. ..$ q2           :List of 1
  .. .. .. .. .. .. .. .. .. .. ..$ q2: num 0.29
  .. .. .. .. .. .. .. .. .. ..$ nmfail       :List of 1
  .. .. .. .. .. .. .. .. .. .. ..$ nmfail: num 0
  .. .. .. .. .. .. .. .. .. ..$ loglikelihood:List of 1
  .. .. .. .. .. .. .. .. .. .. ..$ loglikelihood: num 245
  .. .. .. .. .. .. .. .. .. ..$ aic          :List of 1
  .. .. .. .. .. .. .. .. .. .. ..$ aic: num 987
  .. .. .. .. .. .. .. .. .. ..$ aicc         :List of 1
  .. .. .. .. .. .. .. .. .. .. ..$ aicc: num 987
  .. .. .. .. .. .. .. .. .. ..$ bic          :List of 1
  .. .. .. .. .. .. .. .. .. .. ..$ bic: num 996
  .. .. .. .. .. .. .. .. .. ..$ hq           :List of 1
  .. .. .. .. .. .. .. .. .. .. ..$ hq: num 991
  .. .. .. .. .. .. .. .. .. ..$ aape         :List of 5
  .. .. .. .. .. .. .. .. .. .. ..$ aape.mode: chr "withinsample"
  .. .. .. .. .. .. .. .. .. .. ..$ aape.0   : num 5.63
  .. .. .. .. .. .. .. .. .. .. ..$ aape.1   : num 2.81
  .. .. .. .. .. .. .. .. .. .. ..$ aape.2   : num 6.38
  .. .. .. .. .. .. .. .. .. .. ..$ aape.3   : num 7.69
  .. .. .. .. .. .. .. .. .. ..$ autotransform:List of 1
  .. .. .. .. .. .. .. .. .. .. ..$ autotransform: chr "Log(y)"
  .. .. .. .. .. .. .. .. .. ..$ ifout        : chr "No outlier detection performed"
  .. .. .. .. .. .. .. .. .. ..$ rsd.acf      :'data.frame':	24 obs. of  6 variables:
  .. .. .. .. .. .. .. .. .. .. ..$ lag        : num [1:24] 1 2 3 4 5 6 7 8 9 10 ...
  .. .. .. .. .. .. .. .. .. .. ..$ sample.acf : num [1:24] 0.0121 0.0222 -0.1284 -0.1255 0.0599 ...
  .. .. .. .. .. .. .. .. .. .. ..$ stderr.acf : num [1:24] 0.0874 0.0874 0.0874 0.0889 0.0902 ...
  .. .. .. .. .. .. .. .. .. .. ..$ Ljung-Box.q: num [1:24] 0.0195 0.0859 2.3301 4.4896 4.9855 ...
  .. .. .. .. .. .. .. .. .. .. ..$ df.q       : num [1:24] 0 0 1 2 3 4 5 6 7 8 ...
  .. .. .. .. .. .. .. .. .. .. ..$ pval       : num [1:24] 0 0 0.127 0.106 0.173 ...
  .. .. .. .. .. .. .. .. .. ..$ rsd.pacf     :'data.frame':	24 obs. of  3 variables:
  .. .. .. .. .. .. .. .. .. .. ..$ lag        : num [1:24] 1 2 3 4 5 6 7 8 9 10 ...
  .. .. .. .. .. .. .. .. .. .. ..$ sample.pacf: num [1:24] 0.0121 0.022 -0.129 -0.1247 0.0697 ...
  .. .. .. .. .. .. .. .. .. .. ..$ stderr.pacf: num [1:24] 0.0874 0.0874 0.0874 0.0874 0.0874 ...
  .. .. .. .. .. .. .. .. .. ..$ rsd.acf2     :'data.frame':	12 obs. of  3 variables:
  .. .. .. .. .. .. .. .. .. .. ..$ lag        : num [1:12] 1 2 3 4 5 6 7 8 9 10 ...
  .. .. .. .. .. .. .. .. .. .. ..$ sample.acf2: num [1:12] 0.1182 0.00644 0.02262 -0.03606 0.098 ...
  .. .. .. .. .. .. .. .. .. .. ..$ stderr.acf2: num [1:12] 0.0874 0.0886 0.0886 0.0886 0.0887 ...
  .. .. .. .. .. .. .. .. ..@ seats    : logi FALSE
  .. .. .. .. .. .. .. .. ..@ file     : chr "Series_2"
  .. .. .. .. .. .. .. .. ..@ tblnames : chr [1:11] "a1" "d10" "d11" "d12" ...
  .. .. .. .. .. .. .. .. ..@ Rtblnames: chr [1:11] "Original series" "Final seasonal factors" "Final seasonally adjusted data" "Final trend cycle" ...
  .. .. .. .. .. ..@ tsName         : chr "Series_2"
  .. .. .. .. .. ..@ firstRun       : logi TRUE
  .. .. .. ..$ :Formal class 'x12Single' [package "x12"] with 7 slots
  .. .. .. .. .. ..@ ts             : Time-Series [1:144] from 1949 to 1961: 112 118 132 129 121 135 148 148 136 119 ...
  .. .. .. .. .. ..@ x12Parameter   :Formal class 'x12Parameter' [package "x12"] with 42 slots
  .. .. .. .. .. .. .. ..@ span            : NULL
  .. .. .. .. .. .. .. ..@ modelspan       : NULL
  .. .. .. .. .. .. .. ..@ decimals        : num 2
  .. .. .. .. .. .. .. ..@ transform       : chr "auto"
  .. .. .. .. .. .. .. ..@ arima           : num [1:3] 0 1 1
  .. .. .. .. .. .. .. ..@ sarima          : num [1:3] 1 1 1
  .. .. .. .. .. .. .. ..@ automdl         : logi FALSE
  .. .. .. .. .. .. .. ..@ acceptdefault   : logi FALSE
  .. .. .. .. .. .. .. ..@ balanced        : logi TRUE
  .. .. .. .. .. .. .. ..@ maxorder        : num [1:2] 3 2
  .. .. .. .. .. .. .. ..@ maxdiff         : num [1:2] 1 1
  .. .. .. .. .. .. .. ..@ regvariables    : NULL
  .. .. .. .. .. .. .. ..@ reguser         : NULL
  .. .. .. .. .. .. .. ..@ regfile         : NULL
  .. .. .. .. .. .. .. ..@ usertype        : NULL
  .. .. .. .. .. .. .. ..@ centeruser      : NULL
  .. .. .. .. .. .. .. ..@ regfilestart    : NULL
  .. .. .. .. .. .. .. ..@ seats           : logi FALSE
  .. .. .. .. .. .. .. ..@ seatsparameter  : NULL
  .. .. .. .. .. .. .. ..@ sigmalim        : NULL
  .. .. .. .. .. .. .. ..@ outlier         : NULL
  .. .. .. .. .. .. .. ..@ critical        : NULL
  .. .. .. .. .. .. .. ..@ outlier_span    : NULL
  .. .. .. .. .. .. .. ..@ outlier_method  : NULL
  .. .. .. .. .. .. .. ..@ forecast_years  : NULL
  .. .. .. .. .. .. .. ..@ backcast_years  : NULL
  .. .. .. .. .. .. .. ..@ forecast_conf   : num 0.95
  .. .. .. .. .. .. .. ..@ estimate        : logi FALSE
  .. .. .. .. .. .. .. ..@ estOutofsample  : logi TRUE
  .. .. .. .. .. .. .. ..@ slidingspans    : logi FALSE
  .. .. .. .. .. .. .. ..@ aictest         : NULL
  .. .. .. .. .. .. .. ..@ onlytd          : logi FALSE
  .. .. .. .. .. .. .. ..@ sfshort         : logi FALSE
  .. .. .. .. .. .. .. ..@ samode          : NULL
  .. .. .. .. .. .. .. ..@ seasonalma      : NULL
  .. .. .. .. .. .. .. ..@ trendma         : NULL
  .. .. .. .. .. .. .. ..@ x11appendfcst   : logi TRUE
  .. .. .. .. .. .. .. ..@ x11appendbcst   : logi FALSE
  .. .. .. .. .. .. .. ..@ x11calendarsigma: NULL
  .. .. .. .. .. .. .. ..@ x11excludefcst  : logi TRUE
  .. .. .. .. .. .. .. ..@ x11final        : chr "user"
  .. .. .. .. .. .. .. ..@ x11regress      : logi FALSE
  .. .. .. .. .. ..@ x12Output      :Formal class 'x12Output' [package "x12"] with 22 slots
  .. .. .. .. .. .. .. ..@ a1       : Time-Series [1:144] from 1949 to 1961: 112 118 132 129 121 135 148 148 136 119 ...
  .. .. .. .. .. .. .. ..@ d10      : Time-Series [1:156] from 1949 to 1962: 0.899 0.947 1.057 0.999 0.967 ...
  .. .. .. .. .. .. .. ..@ d11      : Time-Series [1:144] from 1949 to 1961: 125 125 125 129 125 ...
  .. .. .. .. .. .. .. ..@ d12      : Time-Series [1:144] from 1949 to 1961: 124 125 126 126 126 ...
  .. .. .. .. .. .. .. ..@ d13      : Time-Series [1:144] from 1949 to 1961: 1.001 0.997 0.993 1.022 0.99 ...
  .. .. .. .. .. .. .. ..@ d16      : Time-Series [1:156] from 1949 to 1962: 0.899 0.947 1.057 0.999 0.967 ...
  .. .. .. .. .. .. .. ..@ c17      : Time-Series [1:144] from 1949 to 1961: 1 1 1 0.833 1 ...
  .. .. .. .. .. .. .. ..@ d9       : Time-Series [1:144] from 1949 to 1961: NA NA NA 1.02 NA ...
  .. .. .. .. .. .. .. ..@ e2       : Time-Series [1:144] from 1949 to 1961: 125 125 125 129 125 ...
  .. .. .. .. .. .. .. ..@ d8       : Time-Series [1:144] from 1949 to 1961: 0.903 0.944 1.049 1.02 0.957 ...
  .. .. .. .. .. .. .. ..@ b1       : Time-Series [1:156] from 1949 to 1962: 112 118 132 129 121 135 148 148 136 119 ...
  .. .. .. .. .. .. .. ..@ sp0      :Formal class 'spectrum' [package "x12"] with 2 slots
  .. .. .. .. .. .. .. .. .. ..@ frequency: num [1:61] 0 0.00833 0.01667 0.025 0.03333 ...
  .. .. .. .. .. .. .. .. .. ..@ spectrum : num [1:61] -36.1 -35.4 -33.4 -31.3 -30.7 ...
  .. .. .. .. .. .. .. ..@ sp1      :Formal class 'spectrum' [package "x12"] with 2 slots
  .. .. .. .. .. .. .. .. .. ..@ frequency: num [1:61] 0 0.00833 0.01667 0.025 0.03333 ...
  .. .. .. .. .. .. .. .. .. ..@ spectrum : num [1:61] -42.2 -40.8 -36.4 -33.8 -37.1 ...
  .. .. .. .. .. .. .. ..@ sp2      :Formal class 'spectrum' [package "x12"] with 2 slots
  .. .. .. .. .. .. .. .. .. ..@ frequency: num [1:61] 0 0.00833 0.01667 0.025 0.03333 ...
  .. .. .. .. .. .. .. .. .. ..@ spectrum : num [1:61] -42.5 -46.2 -47.9 -45.5 -46.9 ...
  .. .. .. .. .. .. .. ..@ spr      :Formal class 'spectrum' [package "x12"] with 2 slots
  .. .. .. .. .. .. .. .. .. ..@ frequency: num [1:61] 0 0.00833 0.01667 0.025 0.03333 ...
  .. .. .. .. .. .. .. .. .. ..@ spectrum : num [1:61] -36.4 -34.9 -29.7 -25.9 -30.7 ...
  .. .. .. .. .. .. .. ..@ forecast :Formal class 'fbcast' [package "x12"] with 3 slots
  .. .. .. .. .. .. .. .. .. ..@ estimate: Time-Series [1:12] from 1961 to 1962: 451 426 481 492 510 ...
  .. .. .. .. .. .. .. .. .. ..@ lowerci : Time-Series [1:12] from 1961 to 1962: 420 392 438 444 457 ...
  .. .. .. .. .. .. .. .. .. ..@ upperci : Time-Series [1:12] from 1961 to 1962: 484 463 528 545 570 ...
  .. .. .. .. .. .. .. ..@ backcast :Formal class 'fbcast' [package "x12"] with 3 slots
  .. .. .. .. .. .. .. .. .. ..@ estimate:Formal class 'ts' [package "methods"] with 3 slots
  .. .. .. .. .. .. .. .. .. .. .. ..@ .Data   : logi NA
  .. .. .. .. .. .. .. .. .. .. .. ..@ tsp     : num [1:3] 1 1 1
  .. .. .. .. .. .. .. .. .. .. .. ..@ .S3Class: chr "ts"
  .. .. .. .. .. .. .. .. .. ..@ lowerci :Formal class 'ts' [package "methods"] with 3 slots
  .. .. .. .. .. .. .. .. .. .. .. ..@ .Data   : logi NA
  .. .. .. .. .. .. .. .. .. .. .. ..@ tsp     : num [1:3] 1 1 1
  .. .. .. .. .. .. .. .. .. .. .. ..@ .S3Class: chr "ts"
  .. .. .. .. .. .. .. .. .. ..@ upperci :Formal class 'ts' [package "methods"] with 3 slots
  .. .. .. .. .. .. .. .. .. .. .. ..@ .Data   : logi NA
  .. .. .. .. .. .. .. .. .. .. .. ..@ tsp     : num [1:3] 1 1 1
  .. .. .. .. .. .. .. .. .. .. .. ..@ .S3Class: chr "ts"
  .. .. .. .. .. .. .. ..@ dg       :List of 38
  .. .. .. .. .. .. .. .. ..$ x11regress   :List of 1
  .. .. .. .. .. .. .. .. .. ..$ x11regress: chr "no"
  .. .. .. .. .. .. .. .. ..$ transform    :List of 1
  .. .. .. .. .. .. .. .. .. ..$ transform: chr "Automatic selection"
  .. .. .. .. .. .. .. .. ..$ samode       :List of 2
  .. .. .. .. .. .. .. .. .. ..$ samode     : chr "auto-mode seasonal adjustment"
  .. .. .. .. .. .. .. .. .. ..$ finalsamode: chr "multiplicative"
  .. .. .. .. .. .. .. .. ..$ seasonalma   :List of 2
  .. .. .. .. .. .. .. .. .. ..$ seasonalma     : chr "M.S.R."
  .. .. .. .. .. .. .. .. .. ..$ finalseasonalma: chr "3x3"
  .. .. .. .. .. .. .. .. ..$ trendma      :List of 2
  .. .. .. .. .. .. .. .. .. ..$ trendma     : chr "default"
  .. .. .. .. .. .. .. .. .. ..$ finaltrendma: num 9
  .. .. .. .. .. .. .. .. ..$ arimamdl     :List of 1
  .. .. .. .. .. .. .. .. .. ..$ arimamdl: chr "(0,1,1)(1,1,1)"
  .. .. .. .. .. .. .. .. ..$ automdl      :List of 1
  .. .. .. .. .. .. .. .. .. ..$ automdl: chr "-"
  .. .. .. .. .. .. .. .. ..$ regmdl       :List of 1
  .. .. .. .. .. .. .. .. .. ..$ regmdl: chr "none"
  .. .. .. .. .. .. .. .. ..$ nout         :List of 1
  .. .. .. .. .. .. .. .. .. ..$ nout: num 0
  .. .. .. .. .. .. .. .. ..$ nautoout     :List of 1
  .. .. .. .. .. .. .. .. .. ..$ nautoout: chr "-"
  .. .. .. .. .. .. .. .. ..$ nalmostout   :List of 1
  .. .. .. .. .. .. .. .. .. ..$ nalmostout: chr "-"
  .. .. .. .. .. .. .. .. ..$ almostoutlier:List of 1
  .. .. .. .. .. .. .. .. .. ..$ almostoutlier: chr "-"
  .. .. .. .. .. .. .. .. ..$ crit         :List of 1
  .. .. .. .. .. .. .. .. .. ..$ crit: chr "-"
  .. .. .. .. .. .. .. .. ..$ outlier      :List of 1
  .. .. .. .. .. .. .. .. .. ..$ outlier: chr "-"
  .. .. .. .. .. .. .. .. ..$ userdefined  :List of 1
  .. .. .. .. .. .. .. .. .. ..$ userdefined: chr "-"
  .. .. .. .. .. .. .. .. ..$ autooutlier  :List of 1
  .. .. .. .. .. .. .. .. .. ..$ autooutlier: chr "-"
  .. .. .. .. .. .. .. .. ..$ peaks.seas   :List of 1
  .. .. .. .. .. .. .. .. .. ..$ peaks.seas: chr "rsd"
  .. .. .. .. .. .. .. .. ..$ peaks.td     :List of 1
  .. .. .. .. .. .. .. .. .. ..$ peaks.td: chr "sa irr"
  .. .. .. .. .. .. .. .. ..$ id.seas      :List of 1
  .. .. .. .. .. .. .. .. .. ..$ id.seas: chr "yes"
  .. .. .. .. .. .. .. .. ..$ id.rsdseas   :List of 1
  .. .. .. .. .. .. .. .. .. ..$ id.rsdseas: chr "none"
  .. .. .. .. .. .. .. .. ..$ spcrsd       :List of 11
  .. .. .. .. .. .. .. .. .. ..$ spcrsd.t1   :List of 2
  .. .. .. .. .. .. .. .. .. .. ..$ : num 2.7
  .. .. .. .. .. .. .. .. .. .. ..$ : chr "+"
  .. .. .. .. .. .. .. .. .. ..$ spcrsd.t2   :List of 2
  .. .. .. .. .. .. .. .. .. .. ..$ : num 5.4
  .. .. .. .. .. .. .. .. .. .. ..$ : chr "+"
  .. .. .. .. .. .. .. .. .. ..$ spcrsd.t.dom: chr "t1"
  .. .. .. .. .. .. .. .. .. ..$ spcrsd.s1   :List of 2
  .. .. .. .. .. .. .. .. .. .. ..$ : num 16.2
  .. .. .. .. .. .. .. .. .. .. ..$ : chr "+"
  .. .. .. .. .. .. .. .. .. ..$ spcrsd.s2   : chr "nopeak"
  .. .. .. .. .. .. .. .. .. ..$ spcrsd.s3   : chr "nopeak"
  .. .. .. .. .. .. .. .. .. ..$ spcrsd.s4   : chr "nopeak"
  .. .. .. .. .. .. .. .. .. ..$ spcrsd.s5   : chr "nopeak"
  .. .. .. .. .. .. .. .. .. ..$ spcrsd.s6   : chr "nopeak"
  .. .. .. .. .. .. .. .. .. ..$ spcrsd.s.dom: chr "s1"
  .. .. .. .. .. .. .. .. .. ..$ spcrsd.dom  : chr "s1"
  .. .. .. .. .. .. .. .. ..$ spcori       :List of 11
  .. .. .. .. .. .. .. .. .. ..$ spcori.t1   : chr "nopeak"
  .. .. .. .. .. .. .. .. .. ..$ spcori.t2   : chr "nopeak"
  .. .. .. .. .. .. .. .. .. ..$ spcori.t.dom: chr "no"
  .. .. .. .. .. .. .. .. .. ..$ spcori.s1   :List of 2
  .. .. .. .. .. .. .. .. .. .. ..$ : num 20.7
  .. .. .. .. .. .. .. .. .. .. ..$ : chr "+"
  .. .. .. .. .. .. .. .. .. ..$ spcori.s2   :List of 2
  .. .. .. .. .. .. .. .. .. .. ..$ : num 17.7
  .. .. .. .. .. .. .. .. .. .. ..$ : chr "+"
  .. .. .. .. .. .. .. .. .. ..$ spcori.s3   :List of 2
  .. .. .. .. .. .. .. .. .. .. ..$ : num 13.8
  .. .. .. .. .. .. .. .. .. .. ..$ : chr "+"
  .. .. .. .. .. .. .. .. .. ..$ spcori.s4   :List of 2
  .. .. .. .. .. .. .. .. .. .. ..$ : num 16.5
  .. .. .. .. .. .. .. .. .. .. ..$ : chr "+"
  .. .. .. .. .. .. .. .. .. ..$ spcori.s5   :List of 2
  .. .. .. .. .. .. .. .. .. .. ..$ : num 8.5
  .. .. .. .. .. .. .. .. .. .. ..$ : chr "+"
  .. .. .. .. .. .. .. .. .. ..$ spcori.s6   : chr "nopeak"
  .. .. .. .. .. .. .. .. .. ..$ spcori.s.dom: chr "s1"
  .. .. .. .. .. .. .. .. .. ..$ spcori.dom  : chr "s1"
  .. .. .. .. .. .. .. .. ..$ spcsa        :List of 11
  .. .. .. .. .. .. .. .. .. ..$ spcsa.t1   : chr "nopeak"
  .. .. .. .. .. .. .. .. .. ..$ spcsa.t2   :List of 2
  .. .. .. .. .. .. .. .. .. .. ..$ : num 13.8
  .. .. .. .. .. .. .. .. .. .. ..$ : chr "+"
  .. .. .. .. .. .. .. .. .. ..$ spcsa.t.dom: chr "t2"
  .. .. .. .. .. .. .. .. .. ..$ spcsa.s1   :List of 2
  .. .. .. .. .. .. .. .. .. .. ..$ : num 5.9
  .. .. .. .. .. .. .. .. .. .. ..$ : chr "+"
  .. .. .. .. .. .. .. .. .. ..$ spcsa.s2   : chr "nopeak"
  .. .. .. .. .. .. .. .. .. ..$ spcsa.s3   : chr "nopeak"
  .. .. .. .. .. .. .. .. .. ..$ spcsa.s4   : chr "nopeak"
  .. .. .. .. .. .. .. .. .. ..$ spcsa.s5   : chr "nopeak"
  .. .. .. .. .. .. .. .. .. ..$ spcsa.s6   : chr "nopeak"
  .. .. .. .. .. .. .. .. .. ..$ spcsa.s.dom: chr "s1"
  .. .. .. .. .. .. .. .. .. ..$ spcsa.dom  : chr "no"
  .. .. .. .. .. .. .. .. ..$ spcirr       :List of 11
  .. .. .. .. .. .. .. .. .. ..$ spcirr.t1   : chr "nopeak"
  .. .. .. .. .. .. .. .. .. ..$ spcirr.t2   :List of 2
  .. .. .. .. .. .. .. .. .. .. ..$ : num 11.2
  .. .. .. .. .. .. .. .. .. .. ..$ : chr "+"
  .. .. .. .. .. .. .. .. .. ..$ spcirr.t.dom: chr "t2"
  .. .. .. .. .. .. .. .. .. ..$ spcirr.s1   : chr "nopeak"
  .. .. .. .. .. .. .. .. .. ..$ spcirr.s2   : chr "nopeak"
  .. .. .. .. .. .. .. .. .. ..$ spcirr.s3   : chr "nopeak"
  .. .. .. .. .. .. .. .. .. ..$ spcirr.s4   : chr "nopeak"
  .. .. .. .. .. .. .. .. .. ..$ spcirr.s5   : chr "nopeak"
  .. .. .. .. .. .. .. .. .. ..$ spcirr.s6   : chr "nopeak"
  .. .. .. .. .. .. .. .. .. ..$ spcirr.s.dom: chr "no"
  .. .. .. .. .. .. .. .. .. ..$ spcirr.dom  : chr "no"
  .. .. .. .. .. .. .. .. ..$ q            :List of 1
  .. .. .. .. .. .. .. .. .. ..$ q: num 0.26
  .. .. .. .. .. .. .. .. ..$ q2           :List of 1
  .. .. .. .. .. .. .. .. .. ..$ q2: num 0.29
  .. .. .. .. .. .. .. .. ..$ nmfail       :List of 1
  .. .. .. .. .. .. .. .. .. ..$ nmfail: num 0
  .. .. .. .. .. .. .. .. ..$ loglikelihood:List of 1
  .. .. .. .. .. .. .. .. .. ..$ loglikelihood: num 245
  .. .. .. .. .. .. .. .. ..$ aic          :List of 1
  .. .. .. .. .. .. .. .. .. ..$ aic: num 989
  .. .. .. .. .. .. .. .. ..$ aicc         :List of 1
  .. .. .. .. .. .. .. .. .. ..$ aicc: num 989
  .. .. .. .. .. .. .. .. ..$ bic          :List of 1
  .. .. .. .. .. .. .. .. .. ..$ bic: num 1000
  .. .. .. .. .. .. .. .. ..$ hq           :List of 1
  .. .. .. .. .. .. .. .. .. ..$ hq: num 993
  .. .. .. .. .. .. .. .. ..$ aape         :List of 5
  .. .. .. .. .. .. .. .. .. ..$ aape.mode: chr "withinsample"
  .. .. .. .. .. .. .. .. .. ..$ aape.0   : num 5.48
  .. .. .. .. .. .. .. .. .. ..$ aape.1   : num 2.58
  .. .. .. .. .. .. .. .. .. ..$ aape.2   : num 6.04
  .. .. .. .. .. .. .. .. .. ..$ aape.3   : num 7.82
  .. .. .. .. .. .. .. .. ..$ autotransform:List of 1
  .. .. .. .. .. .. .. .. .. ..$ autotransform: chr "Log(y)"
  .. .. .. .. .. .. .. .. ..$ ifout        : chr "No outlier detection performed"
  .. .. .. .. .. .. .. .. ..$ rsd.acf      :'data.frame':	24 obs. of  6 variables:
  .. .. .. .. .. .. .. .. .. ..$ lag        : num [1:24] 1 2 3 4 5 6 7 8 9 10 ...
  .. .. .. .. .. .. .. .. .. ..$ sample.acf : num [1:24] -0.00737 0.01487 -0.13069 -0.15544 0.0502 ...
  .. .. .. .. .. .. .. .. .. ..$ stderr.acf : num [1:24] 0.0874 0.0874 0.0874 0.0889 0.0909 ...
  .. .. .. .. .. .. .. .. .. ..$ Ljung-Box.q: num [1:24] 0.00728 0.03716 2.36193 5.67666 6.02517 ...
  .. .. .. .. .. .. .. .. .. ..$ df.q       : num [1:24] 0 0 0 1 2 3 4 5 6 7 ...
  .. .. .. .. .. .. .. .. .. ..$ pval       : num [1:24] 0 0 0 0.0172 0.0492 ...
  .. .. .. .. .. .. .. .. ..$ rsd.pacf     :'data.frame':	24 obs. of  3 variables:
  .. .. .. .. .. .. .. .. .. ..$ lag        : num [1:24] 1 2 3 4 5 6 7 8 9 10 ...
  .. .. .. .. .. .. .. .. .. ..$ sample.pacf: num [1:24] -0.00737 0.01482 -0.13051 -0.16007 0.05118 ...
  .. .. .. .. .. .. .. .. .. ..$ stderr.pacf: num [1:24] 0.0874 0.0874 0.0874 0.0874 0.0874 ...
  .. .. .. .. .. .. .. .. ..$ rsd.acf2     :'data.frame':	12 obs. of  3 variables:
  .. .. .. .. .. .. .. .. .. ..$ lag        : num [1:12] 1 2 3 4 5 6 7 8 9 10 ...
  .. .. .. .. .. .. .. .. .. ..$ sample.acf2: num [1:12] 0.1299 0.0148 0.0441 -0.0584 0.0776 ...
  .. .. .. .. .. .. .. .. .. ..$ stderr.acf2: num [1:12] 0.0874 0.0888 0.0889 0.089 0.0893 ...
  .. .. .. .. .. .. .. ..@ seats    : logi FALSE
  .. .. .. .. .. .. .. ..@ file     : chr "Series_3"
  .. .. .. .. .. .. .. ..@ tblnames : chr [1:11] "a1" "d10" "d11" "d12" ...
  .. .. .. .. .. .. .. ..@ Rtblnames: chr [1:11] "Original series" "Final seasonal factors" "Final seasonally adjusted data" "Final trend cycle" ...
  .. .. .. .. .. ..@ x12OldParameter:List of 1
  .. .. .. .. .. .. ..$ :Formal class 'x12Parameter' [package "x12"] with 42 slots
  .. .. .. .. .. .. .. .. ..@ span            : NULL
  .. .. .. .. .. .. .. .. ..@ modelspan       : NULL
  .. .. .. .. .. .. .. .. ..@ decimals        : num 2
  .. .. .. .. .. .. .. .. ..@ transform       : chr "auto"
  .. .. .. .. .. .. .. .. ..@ arima           : num [1:3] 0 1 1
  .. .. .. .. .. .. .. .. ..@ sarima          : num [1:3] 1 1 1
  .. .. .. .. .. .. .. .. ..@ automdl         : logi FALSE
  .. .. .. .. .. .. .. .. ..@ acceptdefault   : logi FALSE
  .. .. .. .. .. .. .. .. ..@ balanced        : logi TRUE
  .. .. .. .. .. .. .. .. ..@ maxorder        : num [1:2] 3 2
  .. .. .. .. .. .. .. .. ..@ maxdiff         : num [1:2] 1 1
  .. .. .. .. .. .. .. .. ..@ regvariables    : NULL
  .. .. .. .. .. .. .. .. ..@ reguser         : NULL
  .. .. .. .. .. .. .. .. ..@ regfile         : NULL
  .. .. .. .. .. .. .. .. ..@ usertype        : NULL
  .. .. .. .. .. .. .. .. ..@ centeruser      : NULL
  .. .. .. .. .. .. .. .. ..@ regfilestart    : NULL
  .. .. .. .. .. .. .. .. ..@ seats           : logi FALSE
  .. .. .. .. .. .. .. .. ..@ seatsparameter  : NULL
  .. .. .. .. .. .. .. .. ..@ sigmalim        : NULL
  .. .. .. .. .. .. .. .. ..@ outlier         : NULL
  .. .. .. .. .. .. .. .. ..@ critical        : NULL
  .. .. .. .. .. .. .. .. ..@ outlier_span    : NULL
  .. .. .. .. .. .. .. .. ..@ outlier_method  : NULL
  .. .. .. .. .. .. .. .. ..@ forecast_years  : NULL
  .. .. .. .. .. .. .. .. ..@ backcast_years  : NULL
  .. .. .. .. .. .. .. .. ..@ forecast_conf   : num 0.95
  .. .. .. .. .. .. .. .. ..@ estimate        : logi FALSE
  .. .. .. .. .. .. .. .. ..@ estOutofsample  : logi TRUE
  .. .. .. .. .. .. .. .. ..@ slidingspans    : logi FALSE
  .. .. .. .. .. .. .. .. ..@ aictest         : NULL
  .. .. .. .. .. .. .. .. ..@ onlytd          : logi FALSE
  .. .. .. .. .. .. .. .. ..@ sfshort         : logi FALSE
  .. .. .. .. .. .. .. .. ..@ samode          : NULL
  .. .. .. .. .. .. .. .. ..@ seasonalma      : NULL
  .. .. .. .. .. .. .. .. ..@ trendma         : NULL
  .. .. .. .. .. .. .. .. ..@ x11appendfcst   : logi TRUE
  .. .. .. .. .. .. .. .. ..@ x11appendbcst   : logi FALSE
  .. .. .. .. .. .. .. .. ..@ x11calendarsigma: NULL
  .. .. .. .. .. .. .. .. ..@ x11excludefcst  : logi TRUE
  .. .. .. .. .. .. .. .. ..@ x11final        : chr "user"
  .. .. .. .. .. .. .. .. ..@ x11regress      : logi FALSE
  .. .. .. .. .. ..@ x12OldOutput   :List of 1
  .. .. .. .. .. .. ..$ :Formal class 'x12Output' [package "x12"] with 22 slots
  .. .. .. .. .. .. .. .. ..@ a1       : Time-Series [1:144] from 1949 to 1961: 112 118 132 129 121 135 148 148 136 119 ...
  .. .. .. .. .. .. .. .. ..@ d10      : Time-Series [1:156] from 1949 to 1962: 0.899 0.947 1.057 0.999 0.967 ...
  .. .. .. .. .. .. .. .. ..@ d11      : Time-Series [1:144] from 1949 to 1961: 125 125 125 129 125 ...
  .. .. .. .. .. .. .. .. ..@ d12      : Time-Series [1:144] from 1949 to 1961: 124 125 126 126 126 ...
  .. .. .. .. .. .. .. .. ..@ d13      : Time-Series [1:144] from 1949 to 1961: 1.001 0.997 0.993 1.022 0.99 ...
  .. .. .. .. .. .. .. .. ..@ d16      : Time-Series [1:156] from 1949 to 1962: 0.899 0.947 1.057 0.999 0.967 ...
  .. .. .. .. .. .. .. .. ..@ c17      : Time-Series [1:144] from 1949 to 1961: 1 1 1 0.833 1 ...
  .. .. .. .. .. .. .. .. ..@ d9       : Time-Series [1:144] from 1949 to 1961: NA NA NA 1.02 NA ...
  .. .. .. .. .. .. .. .. ..@ e2       : Time-Series [1:144] from 1949 to 1961: 125 125 125 129 125 ...
  .. .. .. .. .. .. .. .. ..@ d8       : Time-Series [1:144] from 1949 to 1961: 0.903 0.944 1.049 1.02 0.957 ...
  .. .. .. .. .. .. .. .. ..@ b1       : Time-Series [1:156] from 1949 to 1962: 112 118 132 129 121 135 148 148 136 119 ...
  .. .. .. .. .. .. .. .. ..@ sp0      :Formal class 'spectrum' [package "x12"] with 2 slots
  .. .. .. .. .. .. .. .. .. .. ..@ frequency: num [1:61] 0 0.00833 0.01667 0.025 0.03333 ...
  .. .. .. .. .. .. .. .. .. .. ..@ spectrum : num [1:61] -36.1 -35.4 -33.4 -31.3 -30.7 ...
  .. .. .. .. .. .. .. .. ..@ sp1      :Formal class 'spectrum' [package "x12"] with 2 slots
  .. .. .. .. .. .. .. .. .. .. ..@ frequency: num [1:61] 0 0.00833 0.01667 0.025 0.03333 ...
  .. .. .. .. .. .. .. .. .. .. ..@ spectrum : num [1:61] -42.2 -40.8 -36.4 -33.8 -37.1 ...
  .. .. .. .. .. .. .. .. ..@ sp2      :Formal class 'spectrum' [package "x12"] with 2 slots
  .. .. .. .. .. .. .. .. .. .. ..@ frequency: num [1:61] 0 0.00833 0.01667 0.025 0.03333 ...
  .. .. .. .. .. .. .. .. .. .. ..@ spectrum : num [1:61] -42.5 -46.2 -47.9 -45.5 -46.9 ...
  .. .. .. .. .. .. .. .. ..@ spr      :Formal class 'spectrum' [package "x12"] with 2 slots
  .. .. .. .. .. .. .. .. .. .. ..@ frequency: num [1:61] 0 0.00833 0.01667 0.025 0.03333 ...
  .. .. .. .. .. .. .. .. .. .. ..@ spectrum : num [1:61] -36.4 -34.9 -29.7 -25.9 -30.7 ...
  .. .. .. .. .. .. .. .. ..@ forecast :Formal class 'fbcast' [package "x12"] with 3 slots
  .. .. .. .. .. .. .. .. .. .. ..@ estimate: Time-Series [1:12] from 1961 to 1962: 451 426 481 492 510 ...
  .. .. .. .. .. .. .. .. .. .. ..@ lowerci : Time-Series [1:12] from 1961 to 1962: 420 392 438 444 457 ...
  .. .. .. .. .. .. .. .. .. .. ..@ upperci : Time-Series [1:12] from 1961 to 1962: 484 463 528 545 570 ...
  .. .. .. .. .. .. .. .. ..@ backcast :Formal class 'fbcast' [package "x12"] with 3 slots
  .. .. .. .. .. .. .. .. .. .. ..@ estimate:Formal class 'ts' [package "methods"] with 3 slots
  .. .. .. .. .. .. .. .. .. .. .. .. ..@ .Data   : logi NA
  .. .. .. .. .. .. .. .. .. .. .. .. ..@ tsp     : num [1:3] 1 1 1
  .. .. .. .. .. .. .. .. .. .. .. .. ..@ .S3Class: chr "ts"
  .. .. .. .. .. .. .. .. .. .. ..@ lowerci :Formal class 'ts' [package "methods"] with 3 slots
  .. .. .. .. .. .. .. .. .. .. .. .. ..@ .Data   : logi NA
  .. .. .. .. .. .. .. .. .. .. .. .. ..@ tsp     : num [1:3] 1 1 1
  .. .. .. .. .. .. .. .. .. .. .. .. ..@ .S3Class: chr "ts"
  .. .. .. .. .. .. .. .. .. .. ..@ upperci :Formal class 'ts' [package "methods"] with 3 slots
  .. .. .. .. .. .. .. .. .. .. .. .. ..@ .Data   : logi NA
  .. .. .. .. .. .. .. .. .. .. .. .. ..@ tsp     : num [1:3] 1 1 1
  .. .. .. .. .. .. .. .. .. .. .. .. ..@ .S3Class: chr "ts"
  .. .. .. .. .. .. .. .. ..@ dg       :List of 38
  .. .. .. .. .. .. .. .. .. ..$ x11regress   :List of 1
  .. .. .. .. .. .. .. .. .. .. ..$ x11regress: chr "no"
  .. .. .. .. .. .. .. .. .. ..$ transform    :List of 1
  .. .. .. .. .. .. .. .. .. .. ..$ transform: chr "Automatic selection"
  .. .. .. .. .. .. .. .. .. ..$ samode       :List of 2
  .. .. .. .. .. .. .. .. .. .. ..$ samode     : chr "auto-mode seasonal adjustment"
  .. .. .. .. .. .. .. .. .. .. ..$ finalsamode: chr "multiplicative"
  .. .. .. .. .. .. .. .. .. ..$ seasonalma   :List of 2
  .. .. .. .. .. .. .. .. .. .. ..$ seasonalma     : chr "M.S.R."
  .. .. .. .. .. .. .. .. .. .. ..$ finalseasonalma: chr "3x3"
  .. .. .. .. .. .. .. .. .. ..$ trendma      :List of 2
  .. .. .. .. .. .. .. .. .. .. ..$ trendma     : chr "default"
  .. .. .. .. .. .. .. .. .. .. ..$ finaltrendma: num 9
  .. .. .. .. .. .. .. .. .. ..$ arimamdl     :List of 1
  .. .. .. .. .. .. .. .. .. .. ..$ arimamdl: chr "(0,1,1)(1,1,1)"
  .. .. .. .. .. .. .. .. .. ..$ automdl      :List of 1
  .. .. .. .. .. .. .. .. .. .. ..$ automdl: chr "-"
  .. .. .. .. .. .. .. .. .. ..$ regmdl       :List of 1
  .. .. .. .. .. .. .. .. .. .. ..$ regmdl: chr "none"
  .. .. .. .. .. .. .. .. .. ..$ nout         :List of 1
  .. .. .. .. .. .. .. .. .. .. ..$ nout: num 0
  .. .. .. .. .. .. .. .. .. ..$ nautoout     :List of 1
  .. .. .. .. .. .. .. .. .. .. ..$ nautoout: chr "-"
  .. .. .. .. .. .. .. .. .. ..$ nalmostout   :List of 1
  .. .. .. .. .. .. .. .. .. .. ..$ nalmostout: chr "-"
  .. .. .. .. .. .. .. .. .. ..$ almostoutlier:List of 1
  .. .. .. .. .. .. .. .. .. .. ..$ almostoutlier: chr "-"
  .. .. .. .. .. .. .. .. .. ..$ crit         :List of 1
  .. .. .. .. .. .. .. .. .. .. ..$ crit: chr "-"
  .. .. .. .. .. .. .. .. .. ..$ outlier      :List of 1
  .. .. .. .. .. .. .. .. .. .. ..$ outlier: chr "-"
  .. .. .. .. .. .. .. .. .. ..$ userdefined  :List of 1
  .. .. .. .. .. .. .. .. .. .. ..$ userdefined: chr "-"
  .. .. .. .. .. .. .. .. .. ..$ autooutlier  :List of 1
  .. .. .. .. .. .. .. .. .. .. ..$ autooutlier: chr "-"
  .. .. .. .. .. .. .. .. .. ..$ peaks.seas   :List of 1
  .. .. .. .. .. .. .. .. .. .. ..$ peaks.seas: chr "rsd"
  .. .. .. .. .. .. .. .. .. ..$ peaks.td     :List of 1
  .. .. .. .. .. .. .. .. .. .. ..$ peaks.td: chr "sa irr"
  .. .. .. .. .. .. .. .. .. ..$ id.seas      :List of 1
  .. .. .. .. .. .. .. .. .. .. ..$ id.seas: chr "yes"
  .. .. .. .. .. .. .. .. .. ..$ id.rsdseas   :List of 1
  .. .. .. .. .. .. .. .. .. .. ..$ id.rsdseas: chr "none"
  .. .. .. .. .. .. .. .. .. ..$ spcrsd       :List of 11
  .. .. .. .. .. .. .. .. .. .. ..$ spcrsd.t1   :List of 2
  .. .. .. .. .. .. .. .. .. .. .. ..$ : num 2.7
  .. .. .. .. .. .. .. .. .. .. .. ..$ : chr "+"
  .. .. .. .. .. .. .. .. .. .. ..$ spcrsd.t2   :List of 2
  .. .. .. .. .. .. .. .. .. .. .. ..$ : num 5.4
  .. .. .. .. .. .. .. .. .. .. .. ..$ : chr "+"
  .. .. .. .. .. .. .. .. .. .. ..$ spcrsd.t.dom: chr "t1"
  .. .. .. .. .. .. .. .. .. .. ..$ spcrsd.s1   :List of 2
  .. .. .. .. .. .. .. .. .. .. .. ..$ : num 16.2
  .. .. .. .. .. .. .. .. .. .. .. ..$ : chr "+"
  .. .. .. .. .. .. .. .. .. .. ..$ spcrsd.s2   : chr "nopeak"
  .. .. .. .. .. .. .. .. .. .. ..$ spcrsd.s3   : chr "nopeak"
  .. .. .. .. .. .. .. .. .. .. ..$ spcrsd.s4   : chr "nopeak"
  .. .. .. .. .. .. .. .. .. .. ..$ spcrsd.s5   : chr "nopeak"
  .. .. .. .. .. .. .. .. .. .. ..$ spcrsd.s6   : chr "nopeak"
  .. .. .. .. .. .. .. .. .. .. ..$ spcrsd.s.dom: chr "s1"
  .. .. .. .. .. .. .. .. .. .. ..$ spcrsd.dom  : chr "s1"
  .. .. .. .. .. .. .. .. .. ..$ spcori       :List of 11
  .. .. .. .. .. .. .. .. .. .. ..$ spcori.t1   : chr "nopeak"
  .. .. .. .. .. .. .. .. .. .. ..$ spcori.t2   : chr "nopeak"
  .. .. .. .. .. .. .. .. .. .. ..$ spcori.t.dom: chr "no"
  .. .. .. .. .. .. .. .. .. .. ..$ spcori.s1   :List of 2
  .. .. .. .. .. .. .. .. .. .. .. ..$ : num 20.7
  .. .. .. .. .. .. .. .. .. .. .. ..$ : chr "+"
  .. .. .. .. .. .. .. .. .. .. ..$ spcori.s2   :List of 2
  .. .. .. .. .. .. .. .. .. .. .. ..$ : num 17.7
  .. .. .. .. .. .. .. .. .. .. .. ..$ : chr "+"
  .. .. .. .. .. .. .. .. .. .. ..$ spcori.s3   :List of 2
  .. .. .. .. .. .. .. .. .. .. .. ..$ : num 13.8
  .. .. .. .. .. .. .. .. .. .. .. ..$ : chr "+"
  .. .. .. .. .. .. .. .. .. .. ..$ spcori.s4   :List of 2
  .. .. .. .. .. .. .. .. .. .. .. ..$ : num 16.5
  .. .. .. .. .. .. .. .. .. .. .. ..$ : chr "+"
  .. .. .. .. .. .. .. .. .. .. ..$ spcori.s5   :List of 2
  .. .. .. .. .. .. .. .. .. .. .. ..$ : num 8.5
  .. .. .. .. .. .. .. .. .. .. .. ..$ : chr "+"
  .. .. .. .. .. .. .. .. .. .. ..$ spcori.s6   : chr "nopeak"
  .. .. .. .. .. .. .. .. .. .. ..$ spcori.s.dom: chr "s1"
  .. .. .. .. .. .. .. .. .. .. ..$ spcori.dom  : chr "s1"
  .. .. .. .. .. .. .. .. .. ..$ spcsa        :List of 11
  .. .. .. .. .. .. .. .. .. .. ..$ spcsa.t1   : chr "nopeak"
  .. .. .. .. .. .. .. .. .. .. ..$ spcsa.t2   :List of 2
  .. .. .. .. .. .. .. .. .. .. .. ..$ : num 13.8
  .. .. .. .. .. .. .. .. .. .. .. ..$ : chr "+"
  .. .. .. .. .. .. .. .. .. .. ..$ spcsa.t.dom: chr "t2"
  .. .. .. .. .. .. .. .. .. .. ..$ spcsa.s1   :List of 2
  .. .. .. .. .. .. .. .. .. .. .. ..$ : num 5.9
  .. .. .. .. .. .. .. .. .. .. .. ..$ : chr "+"
  .. .. .. .. .. .. .. .. .. .. ..$ spcsa.s2   : chr "nopeak"
  .. .. .. .. .. .. .. .. .. .. ..$ spcsa.s3   : chr "nopeak"
  .. .. .. .. .. .. .. .. .. .. ..$ spcsa.s4   : chr "nopeak"
  .. .. .. .. .. .. .. .. .. .. ..$ spcsa.s5   : chr "nopeak"
  .. .. .. .. .. .. .. .. .. .. ..$ spcsa.s6   : chr "nopeak"
  .. .. .. .. .. .. .. .. .. .. ..$ spcsa.s.dom: chr "s1"
  .. .. .. .. .. .. .. .. .. .. ..$ spcsa.dom  : chr "no"
  .. .. .. .. .. .. .. .. .. ..$ spcirr       :List of 11
  .. .. .. .. .. .. .. .. .. .. ..$ spcirr.t1   : chr "nopeak"
  .. .. .. .. .. .. .. .. .. .. ..$ spcirr.t2   :List of 2
  .. .. .. .. .. .. .. .. .. .. .. ..$ : num 11.2
  .. .. .. .. .. .. .. .. .. .. .. ..$ : chr "+"
  .. .. .. .. .. .. .. .. .. .. ..$ spcirr.t.dom: chr "t2"
  .. .. .. .. .. .. .. .. .. .. ..$ spcirr.s1   : chr "nopeak"
  .. .. .. .. .. .. .. .. .. .. ..$ spcirr.s2   : chr "nopeak"
  .. .. .. .. .. .. .. .. .. .. ..$ spcirr.s3   : chr "nopeak"
  .. .. .. .. .. .. .. .. .. .. ..$ spcirr.s4   : chr "nopeak"
  .. .. .. .. .. .. .. .. .. .. ..$ spcirr.s5   : chr "nopeak"
  .. .. .. .. .. .. .. .. .. .. ..$ spcirr.s6   : chr "nopeak"
  .. .. .. .. .. .. .. .. .. .. ..$ spcirr.s.dom: chr "no"
  .. .. .. .. .. .. .. .. .. .. ..$ spcirr.dom  : chr "no"
  .. .. .. .. .. .. .. .. .. ..$ q            :List of 1
  .. .. .. .. .. .. .. .. .. .. ..$ q: num 0.26
  .. .. .. .. .. .. .. .. .. ..$ q2           :List of 1
  .. .. .. .. .. .. .. .. .. .. ..$ q2: num 0.29
  .. .. .. .. .. .. .. .. .. ..$ nmfail       :List of 1
  .. .. .. .. .. .. .. .. .. .. ..$ nmfail: num 0
  .. .. .. .. .. .. .. .. .. ..$ loglikelihood:List of 1
  .. .. .. .. .. .. .. .. .. .. ..$ loglikelihood: num 245
  .. .. .. .. .. .. .. .. .. ..$ aic          :List of 1
  .. .. .. .. .. .. .. .. .. .. ..$ aic: num 989
  .. .. .. .. .. .. .. .. .. ..$ aicc         :List of 1
  .. .. .. .. .. .. .. .. .. .. ..$ aicc: num 989
  .. .. .. .. .. .. .. .. .. ..$ bic          :List of 1
  .. .. .. .. .. .. .. .. .. .. ..$ bic: num 1000
  .. .. .. .. .. .. .. .. .. ..$ hq           :List of 1
  .. .. .. .. .. .. .. .. .. .. ..$ hq: num 993
  .. .. .. .. .. .. .. .. .. ..$ aape         :List of 5
  .. .. .. .. .. .. .. .. .. .. ..$ aape.mode: chr "withinsample"
  .. .. .. .. .. .. .. .. .. .. ..$ aape.0   : num 5.48
  .. .. .. .. .. .. .. .. .. .. ..$ aape.1   : num 2.58
  .. .. .. .. .. .. .. .. .. .. ..$ aape.2   : num 6.04
  .. .. .. .. .. .. .. .. .. .. ..$ aape.3   : num 7.82
  .. .. .. .. .. .. .. .. .. ..$ autotransform:List of 1
  .. .. .. .. .. .. .. .. .. .. ..$ autotransform: chr "Log(y)"
  .. .. .. .. .. .. .. .. .. ..$ ifout        : chr "No outlier detection performed"
  .. .. .. .. .. .. .. .. .. ..$ rsd.acf      :'data.frame':	24 obs. of  6 variables:
  .. .. .. .. .. .. .. .. .. .. ..$ lag        : num [1:24] 1 2 3 4 5 6 7 8 9 10 ...
  .. .. .. .. .. .. .. .. .. .. ..$ sample.acf : num [1:24] -0.00737 0.01487 -0.13069 -0.15544 0.0502 ...
  .. .. .. .. .. .. .. .. .. .. ..$ stderr.acf : num [1:24] 0.0874 0.0874 0.0874 0.0889 0.0909 ...
  .. .. .. .. .. .. .. .. .. .. ..$ Ljung-Box.q: num [1:24] 0.00728 0.03716 2.36193 5.67666 6.02517 ...
  .. .. .. .. .. .. .. .. .. .. ..$ df.q       : num [1:24] 0 0 0 1 2 3 4 5 6 7 ...
  .. .. .. .. .. .. .. .. .. .. ..$ pval       : num [1:24] 0 0 0 0.0172 0.0492 ...
  .. .. .. .. .. .. .. .. .. ..$ rsd.pacf     :'data.frame':	24 obs. of  3 variables:
  .. .. .. .. .. .. .. .. .. .. ..$ lag        : num [1:24] 1 2 3 4 5 6 7 8 9 10 ...
  .. .. .. .. .. .. .. .. .. .. ..$ sample.pacf: num [1:24] -0.00737 0.01482 -0.13051 -0.16007 0.05118 ...
  .. .. .. .. .. .. .. .. .. .. ..$ stderr.pacf: num [1:24] 0.0874 0.0874 0.0874 0.0874 0.0874 ...
  .. .. .. .. .. .. .. .. .. ..$ rsd.acf2     :'data.frame':	12 obs. of  3 variables:
  .. .. .. .. .. .. .. .. .. .. ..$ lag        : num [1:12] 1 2 3 4 5 6 7 8 9 10 ...
  .. .. .. .. .. .. .. .. .. .. ..$ sample.acf2: num [1:12] 0.1299 0.0148 0.0441 -0.0584 0.0776 ...
  .. .. .. .. .. .. .. .. .. .. ..$ stderr.acf2: num [1:12] 0.0874 0.0888 0.0889 0.089 0.0893 ...
  .. .. .. .. .. .. .. .. ..@ seats    : logi FALSE
  .. .. .. .. .. .. .. .. ..@ file     : chr "Series_3"
  .. .. .. .. .. .. .. .. ..@ tblnames : chr [1:11] "a1" "d10" "d11" "d12" ...
  .. .. .. .. .. .. .. .. ..@ Rtblnames: chr [1:11] "Original series" "Final seasonal factors" "Final seasonally adjusted data" "Final trend cycle" ...
  .. .. .. .. .. ..@ tsName         : chr "Series_3"
  .. .. .. .. .. ..@ firstRun       : logi TRUE
  .. .. .. ..$ :Formal class 'x12Single' [package "x12"] with 7 slots
  .. .. .. .. .. ..@ ts             : Time-Series [1:144] from 1949 to 1961: 112 118 132 129 121 135 148 148 136 119 ...
  .. .. .. .. .. ..@ x12Parameter   :Formal class 'x12Parameter' [package "x12"] with 42 slots
  .. .. .. .. .. .. .. ..@ span            : NULL
  .. .. .. .. .. .. .. ..@ modelspan       : NULL
  .. .. .. .. .. .. .. ..@ decimals        : num 2
  .. .. .. .. .. .. .. ..@ transform       : chr "auto"
  .. .. .. .. .. .. .. ..@ arima           : num [1:3] 1 1 1
  .. .. .. .. .. .. .. ..@ sarima          : num [1:3] 1 1 1
  .. .. .. .. .. .. .. ..@ automdl         : logi FALSE
  .. .. .. .. .. .. .. ..@ acceptdefault   : logi FALSE
  .. .. .. .. .. .. .. ..@ balanced        : logi TRUE
  .. .. .. .. .. .. .. ..@ maxorder        : num [1:2] 3 2
  .. .. .. .. .. .. .. ..@ maxdiff         : num [1:2] 1 1
  .. .. .. .. .. .. .. ..@ regvariables    : NULL
  .. .. .. .. .. .. .. ..@ reguser         : NULL
  .. .. .. .. .. .. .. ..@ regfile         : NULL
  .. .. .. .. .. .. .. ..@ usertype        : NULL
  .. .. .. .. .. .. .. ..@ centeruser      : NULL
  .. .. .. .. .. .. .. ..@ regfilestart    : NULL
  .. .. .. .. .. .. .. ..@ seats           : logi FALSE
  .. .. .. .. .. .. .. ..@ seatsparameter  : NULL
  .. .. .. .. .. .. .. ..@ sigmalim        : NULL
  .. .. .. .. .. .. .. ..@ outlier         : NULL
  .. .. .. .. .. .. .. ..@ critical        : NULL
  .. .. .. .. .. .. .. ..@ outlier_span    : NULL
  .. .. .. .. .. .. .. ..@ outlier_method  : NULL
  .. .. .. .. .. .. .. ..@ forecast_years  : NULL
  .. .. .. .. .. .. .. ..@ backcast_years  : NULL
  .. .. .. .. .. .. .. ..@ forecast_conf   : num 0.95
  .. .. .. .. .. .. .. ..@ estimate        : logi FALSE
  .. .. .. .. .. .. .. ..@ estOutofsample  : logi TRUE
  .. .. .. .. .. .. .. ..@ slidingspans    : logi FALSE
  .. .. .. .. .. .. .. ..@ aictest         : NULL
  .. .. .. .. .. .. .. ..@ onlytd          : logi FALSE
  .. .. .. .. .. .. .. ..@ sfshort         : logi FALSE
  .. .. .. .. .. .. .. ..@ samode          : NULL
  .. .. .. .. .. .. .. ..@ seasonalma      : NULL
  .. .. .. .. .. .. .. ..@ trendma         : NULL
  .. .. .. .. .. .. .. ..@ x11appendfcst   : logi TRUE
  .. .. .. .. .. .. .. ..@ x11appendbcst   : logi FALSE
  .. .. .. .. .. .. .. ..@ x11calendarsigma: NULL
  .. .. .. .. .. .. .. ..@ x11excludefcst  : logi TRUE
  .. .. .. .. .. .. .. ..@ x11final        : chr "user"
  .. .. .. .. .. .. .. ..@ x11regress      : logi FALSE
  .. .. .. .. .. ..@ x12Output      :Formal class 'x12Output' [package "x12"] with 22 slots
  .. .. .. .. .. .. .. ..@ a1       : Time-Series [1:144] from 1949 to 1961: 112 118 132 129 121 135 148 148 136 119 ...
  .. .. .. .. .. .. .. ..@ d10      : Time-Series [1:156] from 1949 to 1962: 0.899 0.947 1.057 0.999 0.967 ...
  .. .. .. .. .. .. .. ..@ d11      : Time-Series [1:144] from 1949 to 1961: 125 125 125 129 125 ...
  .. .. .. .. .. .. .. ..@ d12      : Time-Series [1:144] from 1949 to 1961: 124 125 126 126 126 ...
  .. .. .. .. .. .. .. ..@ d13      : Time-Series [1:144] from 1949 to 1961: 1.001 0.997 0.993 1.022 0.99 ...
  .. .. .. .. .. .. .. ..@ d16      : Time-Series [1:156] from 1949 to 1962: 0.899 0.947 1.057 0.999 0.967 ...
  .. .. .. .. .. .. .. ..@ c17      : Time-Series [1:144] from 1949 to 1961: 1 1 1 0.833 1 ...
  .. .. .. .. .. .. .. ..@ d9       : Time-Series [1:144] from 1949 to 1961: NA NA NA 1.02 NA ...
  .. .. .. .. .. .. .. ..@ e2       : Time-Series [1:144] from 1949 to 1961: 125 125 125 129 125 ...
  .. .. .. .. .. .. .. ..@ d8       : Time-Series [1:144] from 1949 to 1961: 0.903 0.944 1.049 1.02 0.957 ...
  .. .. .. .. .. .. .. ..@ b1       : Time-Series [1:156] from 1949 to 1962: 112 118 132 129 121 135 148 148 136 119 ...
  .. .. .. .. .. .. .. ..@ sp0      :Formal class 'spectrum' [package "x12"] with 2 slots
  .. .. .. .. .. .. .. .. .. ..@ frequency: num [1:61] 0 0.00833 0.01667 0.025 0.03333 ...
  .. .. .. .. .. .. .. .. .. ..@ spectrum : num [1:61] -36.1 -35.4 -33.4 -31.3 -30.7 ...
  .. .. .. .. .. .. .. ..@ sp1      :Formal class 'spectrum' [package "x12"] with 2 slots
  .. .. .. .. .. .. .. .. .. ..@ frequency: num [1:61] 0 0.00833 0.01667 0.025 0.03333 ...
  .. .. .. .. .. .. .. .. .. ..@ spectrum : num [1:61] -42.2 -40.8 -36.4 -33.8 -37.1 ...
  .. .. .. .. .. .. .. ..@ sp2      :Formal class 'spectrum' [package "x12"] with 2 slots
  .. .. .. .. .. .. .. .. .. ..@ frequency: num [1:61] 0 0.00833 0.01667 0.025 0.03333 ...
  .. .. .. .. .. .. .. .. .. ..@ spectrum : num [1:61] -42.6 -46.2 -47.9 -45.6 -46.9 ...
  .. .. .. .. .. .. .. ..@ spr      :Formal class 'spectrum' [package "x12"] with 2 slots
  .. .. .. .. .. .. .. .. .. ..@ frequency: num [1:61] 0 0.00833 0.01667 0.025 0.03333 ...
  .. .. .. .. .. .. .. .. .. ..@ spectrum : num [1:61] -36 -34.5 -29.2 -25.2 -30.2 ...
  .. .. .. .. .. .. .. ..@ forecast :Formal class 'fbcast' [package "x12"] with 3 slots
  .. .. .. .. .. .. .. .. .. ..@ estimate: Time-Series [1:12] from 1961 to 1962: 451 427 482 493 511 ...
  .. .. .. .. .. .. .. .. .. ..@ lowerci : Time-Series [1:12] from 1961 to 1962: 420 392 440 446 459 ...
  .. .. .. .. .. .. .. .. .. ..@ upperci : Time-Series [1:12] from 1961 to 1962: 485 464 529 545 568 ...
  .. .. .. .. .. .. .. ..@ backcast :Formal class 'fbcast' [package "x12"] with 3 slots
  .. .. .. .. .. .. .. .. .. ..@ estimate:Formal class 'ts' [package "methods"] with 3 slots
  .. .. .. .. .. .. .. .. .. .. .. ..@ .Data   : logi NA
  .. .. .. .. .. .. .. .. .. .. .. ..@ tsp     : num [1:3] 1 1 1
  .. .. .. .. .. .. .. .. .. .. .. ..@ .S3Class: chr "ts"
  .. .. .. .. .. .. .. .. .. ..@ lowerci :Formal class 'ts' [package "methods"] with 3 slots
  .. .. .. .. .. .. .. .. .. .. .. ..@ .Data   : logi NA
  .. .. .. .. .. .. .. .. .. .. .. ..@ tsp     : num [1:3] 1 1 1
  .. .. .. .. .. .. .. .. .. .. .. ..@ .S3Class: chr "ts"
  .. .. .. .. .. .. .. .. .. ..@ upperci :Formal class 'ts' [package "methods"] with 3 slots
  .. .. .. .. .. .. .. .. .. .. .. ..@ .Data   : logi NA
  .. .. .. .. .. .. .. .. .. .. .. ..@ tsp     : num [1:3] 1 1 1
  .. .. .. .. .. .. .. .. .. .. .. ..@ .S3Class: chr "ts"
  .. .. .. .. .. .. .. ..@ dg       :List of 38
  .. .. .. .. .. .. .. .. ..$ x11regress   :List of 1
  .. .. .. .. .. .. .. .. .. ..$ x11regress: chr "no"
  .. .. .. .. .. .. .. .. ..$ transform    :List of 1
  .. .. .. .. .. .. .. .. .. ..$ transform: chr "Automatic selection"
  .. .. .. .. .. .. .. .. ..$ samode       :List of 2
  .. .. .. .. .. .. .. .. .. ..$ samode     : chr "auto-mode seasonal adjustment"
  .. .. .. .. .. .. .. .. .. ..$ finalsamode: chr "multiplicative"
  .. .. .. .. .. .. .. .. ..$ seasonalma   :List of 2
  .. .. .. .. .. .. .. .. .. ..$ seasonalma     : chr "M.S.R."
  .. .. .. .. .. .. .. .. .. ..$ finalseasonalma: chr "3x3"
  .. .. .. .. .. .. .. .. ..$ trendma      :List of 2
  .. .. .. .. .. .. .. .. .. ..$ trendma     : chr "default"
  .. .. .. .. .. .. .. .. .. ..$ finaltrendma: num 9
  .. .. .. .. .. .. .. .. ..$ arimamdl     :List of 1
  .. .. .. .. .. .. .. .. .. ..$ arimamdl: chr "(1,1,1)(1,1,1)"
  .. .. .. .. .. .. .. .. ..$ automdl      :List of 1
  .. .. .. .. .. .. .. .. .. ..$ automdl: chr "-"
  .. .. .. .. .. .. .. .. ..$ regmdl       :List of 1
  .. .. .. .. .. .. .. .. .. ..$ regmdl: chr "none"
  .. .. .. .. .. .. .. .. ..$ nout         :List of 1
  .. .. .. .. .. .. .. .. .. ..$ nout: num 0
  .. .. .. .. .. .. .. .. ..$ nautoout     :List of 1
  .. .. .. .. .. .. .. .. .. ..$ nautoout: chr "-"
  .. .. .. .. .. .. .. .. ..$ nalmostout   :List of 1
  .. .. .. .. .. .. .. .. .. ..$ nalmostout: chr "-"
  .. .. .. .. .. .. .. .. ..$ almostoutlier:List of 1
  .. .. .. .. .. .. .. .. .. ..$ almostoutlier: chr "-"
  .. .. .. .. .. .. .. .. ..$ crit         :List of 1
  .. .. .. .. .. .. .. .. .. ..$ crit: chr "-"
  .. .. .. .. .. .. .. .. ..$ outlier      :List of 1
  .. .. .. .. .. .. .. .. .. ..$ outlier: chr "-"
  .. .. .. .. .. .. .. .. ..$ userdefined  :List of 1
  .. .. .. .. .. .. .. .. .. ..$ userdefined: chr "-"
  .. .. .. .. .. .. .. .. ..$ autooutlier  :List of 1
  .. .. .. .. .. .. .. .. .. ..$ autooutlier: chr "-"
  .. .. .. .. .. .. .. .. ..$ peaks.seas   :List of 1
  .. .. .. .. .. .. .. .. .. ..$ peaks.seas: chr "rsd"
  .. .. .. .. .. .. .. .. ..$ peaks.td     :List of 1
  .. .. .. .. .. .. .. .. .. ..$ peaks.td: chr "sa irr"
  .. .. .. .. .. .. .. .. ..$ id.seas      :List of 1
  .. .. .. .. .. .. .. .. .. ..$ id.seas: chr "yes"
  .. .. .. .. .. .. .. .. ..$ id.rsdseas   :List of 1
  .. .. .. .. .. .. .. .. .. ..$ id.rsdseas: chr "none"
  .. .. .. .. .. .. .. .. ..$ spcrsd       :List of 11
  .. .. .. .. .. .. .. .. .. ..$ spcrsd.t1   :List of 2
  .. .. .. .. .. .. .. .. .. .. ..$ : num 2.6
  .. .. .. .. .. .. .. .. .. .. ..$ : chr "+"
  .. .. .. .. .. .. .. .. .. ..$ spcrsd.t2   :List of 2
  .. .. .. .. .. .. .. .. .. .. ..$ : num 5.7
  .. .. .. .. .. .. .. .. .. .. ..$ : chr "+"
  .. .. .. .. .. .. .. .. .. ..$ spcrsd.t.dom: chr "t1"
  .. .. .. .. .. .. .. .. .. ..$ spcrsd.s1   :List of 2
  .. .. .. .. .. .. .. .. .. .. ..$ : num 15.9
  .. .. .. .. .. .. .. .. .. .. ..$ : chr "+"
  .. .. .. .. .. .. .. .. .. ..$ spcrsd.s2   : chr "nopeak"
  .. .. .. .. .. .. .. .. .. ..$ spcrsd.s3   : chr "nopeak"
  .. .. .. .. .. .. .. .. .. ..$ spcrsd.s4   : chr "nopeak"
  .. .. .. .. .. .. .. .. .. ..$ spcrsd.s5   : chr "nopeak"
  .. .. .. .. .. .. .. .. .. ..$ spcrsd.s6   : chr "nopeak"
  .. .. .. .. .. .. .. .. .. ..$ spcrsd.s.dom: chr "s1"
  .. .. .. .. .. .. .. .. .. ..$ spcrsd.dom  : chr "s1"
  .. .. .. .. .. .. .. .. ..$ spcori       :List of 11
  .. .. .. .. .. .. .. .. .. ..$ spcori.t1   : chr "nopeak"
  .. .. .. .. .. .. .. .. .. ..$ spcori.t2   : chr "nopeak"
  .. .. .. .. .. .. .. .. .. ..$ spcori.t.dom: chr "no"
  .. .. .. .. .. .. .. .. .. ..$ spcori.s1   :List of 2
  .. .. .. .. .. .. .. .. .. .. ..$ : num 20.7
  .. .. .. .. .. .. .. .. .. .. ..$ : chr "+"
  .. .. .. .. .. .. .. .. .. ..$ spcori.s2   :List of 2
  .. .. .. .. .. .. .. .. .. .. ..$ : num 17.7
  .. .. .. .. .. .. .. .. .. .. ..$ : chr "+"
  .. .. .. .. .. .. .. .. .. ..$ spcori.s3   :List of 2
  .. .. .. .. .. .. .. .. .. .. ..$ : num 13.8
  .. .. .. .. .. .. .. .. .. .. ..$ : chr "+"
  .. .. .. .. .. .. .. .. .. ..$ spcori.s4   :List of 2
  .. .. .. .. .. .. .. .. .. .. ..$ : num 16.5
  .. .. .. .. .. .. .. .. .. .. ..$ : chr "+"
  .. .. .. .. .. .. .. .. .. ..$ spcori.s5   :List of 2
  .. .. .. .. .. .. .. .. .. .. ..$ : num 8.5
  .. .. .. .. .. .. .. .. .. .. ..$ : chr "+"
  .. .. .. .. .. .. .. .. .. ..$ spcori.s6   : chr "nopeak"
  .. .. .. .. .. .. .. .. .. ..$ spcori.s.dom: chr "s1"
  .. .. .. .. .. .. .. .. .. ..$ spcori.dom  : chr "s1"
  .. .. .. .. .. .. .. .. ..$ spcsa        :List of 11
  .. .. .. .. .. .. .. .. .. ..$ spcsa.t1   : chr "nopeak"
  .. .. .. .. .. .. .. .. .. ..$ spcsa.t2   :List of 2
  .. .. .. .. .. .. .. .. .. .. ..$ : num 13.8
  .. .. .. .. .. .. .. .. .. .. ..$ : chr "+"
  .. .. .. .. .. .. .. .. .. ..$ spcsa.t.dom: chr "t2"
  .. .. .. .. .. .. .. .. .. ..$ spcsa.s1   :List of 2
  .. .. .. .. .. .. .. .. .. .. ..$ : num 5.9
  .. .. .. .. .. .. .. .. .. .. ..$ : chr "+"
  .. .. .. .. .. .. .. .. .. ..$ spcsa.s2   : chr "nopeak"
  .. .. .. .. .. .. .. .. .. ..$ spcsa.s3   : chr "nopeak"
  .. .. .. .. .. .. .. .. .. ..$ spcsa.s4   : chr "nopeak"
  .. .. .. .. .. .. .. .. .. ..$ spcsa.s5   : chr "nopeak"
  .. .. .. .. .. .. .. .. .. ..$ spcsa.s6   : chr "nopeak"
  .. .. .. .. .. .. .. .. .. ..$ spcsa.s.dom: chr "s1"
  .. .. .. .. .. .. .. .. .. ..$ spcsa.dom  : chr "no"
  .. .. .. .. .. .. .. .. ..$ spcirr       :List of 11
  .. .. .. .. .. .. .. .. .. ..$ spcirr.t1   : chr "nopeak"
  .. .. .. .. .. .. .. .. .. ..$ spcirr.t2   :List of 2
  .. .. .. .. .. .. .. .. .. .. ..$ : num 11.2
  .. .. .. .. .. .. .. .. .. .. ..$ : chr "+"
  .. .. .. .. .. .. .. .. .. ..$ spcirr.t.dom: chr "t2"
  .. .. .. .. .. .. .. .. .. ..$ spcirr.s1   : chr "nopeak"
  .. .. .. .. .. .. .. .. .. ..$ spcirr.s2   : chr "nopeak"
  .. .. .. .. .. .. .. .. .. ..$ spcirr.s3   : chr "nopeak"
  .. .. .. .. .. .. .. .. .. ..$ spcirr.s4   : chr "nopeak"
  .. .. .. .. .. .. .. .. .. ..$ spcirr.s5   : chr "nopeak"
  .. .. .. .. .. .. .. .. .. ..$ spcirr.s6   : chr "nopeak"
  .. .. .. .. .. .. .. .. .. ..$ spcirr.s.dom: chr "no"
  .. .. .. .. .. .. .. .. .. ..$ spcirr.dom  : chr "no"
  .. .. .. .. .. .. .. .. ..$ q            :List of 1
  .. .. .. .. .. .. .. .. .. ..$ q: num 0.26
  .. .. .. .. .. .. .. .. ..$ q2           :List of 1
  .. .. .. .. .. .. .. .. .. ..$ q2: num 0.29
  .. .. .. .. .. .. .. .. ..$ nmfail       :List of 1
  .. .. .. .. .. .. .. .. .. ..$ nmfail: num 0
  .. .. .. .. .. .. .. .. ..$ loglikelihood:List of 1
  .. .. .. .. .. .. .. .. .. ..$ loglikelihood: num 245
  .. .. .. .. .. .. .. .. ..$ aic          :List of 1
  .. .. .. .. .. .. .. .. .. ..$ aic: num 990
  .. .. .. .. .. .. .. .. ..$ aicc         :List of 1
  .. .. .. .. .. .. .. .. .. ..$ aicc: num 991
  .. .. .. .. .. .. .. .. ..$ bic          :List of 1
  .. .. .. .. .. .. .. .. .. ..$ bic: num 1005
  .. .. .. .. .. .. .. .. ..$ hq           :List of 1
  .. .. .. .. .. .. .. .. .. ..$ hq: num 996
  .. .. .. .. .. .. .. .. ..$ aape         :List of 5
  .. .. .. .. .. .. .. .. .. ..$ aape.mode: chr "withinsample"
  .. .. .. .. .. .. .. .. .. ..$ aape.0   : num 5.43
  .. .. .. .. .. .. .. .. .. ..$ aape.1   : num 2.51
  .. .. .. .. .. .. .. .. .. ..$ aape.2   : num 5.61
  .. .. .. .. .. .. .. .. .. ..$ aape.3   : num 8.18
  .. .. .. .. .. .. .. .. ..$ autotransform:List of 1
  .. .. .. .. .. .. .. .. .. ..$ autotransform: chr "Log(y)"
  .. .. .. .. .. .. .. .. ..$ ifout        : chr "No outlier detection performed"
  .. .. .. .. .. .. .. .. ..$ rsd.acf      :'data.frame':	24 obs. of  6 variables:
  .. .. .. .. .. .. .. .. .. ..$ lag        : num [1:24] 1 2 3 4 5 6 7 8 9 10 ...
  .. .. .. .. .. .. .. .. .. ..$ sample.acf : num [1:24] -0.0264 0.0702 -0.0808 -0.1625 0.0441 ...
  .. .. .. .. .. .. .. .. .. ..$ stderr.acf : num [1:24] 0.0874 0.0874 0.0879 0.0884 0.0907 ...
  .. .. .. .. .. .. .. .. .. ..$ Ljung-Box.q: num [1:24] 0.0937 0.7599 1.6488 5.2697 5.5386 ...
  .. .. .. .. .. .. .. .. .. ..$ df.q       : num [1:24] 0 0 0 0 1 2 3 4 5 6 ...
  .. .. .. .. .. .. .. .. .. ..$ pval       : num [1:24] 0 0 0 0 0.0186 ...
  .. .. .. .. .. .. .. .. ..$ rsd.pacf     :'data.frame':	24 obs. of  3 variables:
  .. .. .. .. .. .. .. .. .. ..$ lag        : num [1:24] 1 2 3 4 5 6 7 8 9 10 ...
  .. .. .. .. .. .. .. .. .. ..$ sample.pacf: num [1:24] -0.0264 0.0696 -0.0777 -0.1728 0.0483 ...
  .. .. .. .. .. .. .. .. .. ..$ stderr.pacf: num [1:24] 0.0874 0.0874 0.0874 0.0874 0.0874 ...
  .. .. .. .. .. .. .. .. ..$ rsd.acf2     :'data.frame':	12 obs. of  3 variables:
  .. .. .. .. .. .. .. .. .. ..$ lag        : num [1:12] 1 2 3 4 5 6 7 8 9 10 ...
  .. .. .. .. .. .. .. .. .. ..$ sample.acf2: num [1:12] 0.1353 0.0123 0.0307 -0.0834 0.0724 ...
  .. .. .. .. .. .. .. .. .. ..$ stderr.acf2: num [1:12] 0.0874 0.089 0.089 0.089 0.0896 ...
  .. .. .. .. .. .. .. ..@ seats    : logi FALSE
  .. .. .. .. .. .. .. ..@ file     : chr "Series_4"
  .. .. .. .. .. .. .. ..@ tblnames : chr [1:11] "a1" "d10" "d11" "d12" ...
  .. .. .. .. .. .. .. ..@ Rtblnames: chr [1:11] "Original series" "Final seasonal factors" "Final seasonally adjusted data" "Final trend cycle" ...
  .. .. .. .. .. ..@ x12OldParameter:List of 1
  .. .. .. .. .. .. ..$ :Formal class 'x12Parameter' [package "x12"] with 42 slots
  .. .. .. .. .. .. .. .. ..@ span            : NULL
  .. .. .. .. .. .. .. .. ..@ modelspan       : NULL
  .. .. .. .. .. .. .. .. ..@ decimals        : num 2
  .. .. .. .. .. .. .. .. ..@ transform       : chr "auto"
  .. .. .. .. .. .. .. .. ..@ arima           : num [1:3] 1 1 1
  .. .. .. .. .. .. .. .. ..@ sarima          : num [1:3] 1 1 1
  .. .. .. .. .. .. .. .. ..@ automdl         : logi FALSE
  .. .. .. .. .. .. .. .. ..@ acceptdefault   : logi FALSE
  .. .. .. .. .. .. .. .. ..@ balanced        : logi TRUE
  .. .. .. .. .. .. .. .. ..@ maxorder        : num [1:2] 3 2
  .. .. .. .. .. .. .. .. ..@ maxdiff         : num [1:2] 1 1
  .. .. .. .. .. .. .. .. ..@ regvariables    : NULL
  .. .. .. .. .. .. .. .. ..@ reguser         : NULL
  .. .. .. .. .. .. .. .. ..@ regfile         : NULL
  .. .. .. .. .. .. .. .. ..@ usertype        : NULL
  .. .. .. .. .. .. .. .. ..@ centeruser      : NULL
  .. .. .. .. .. .. .. .. ..@ regfilestart    : NULL
  .. .. .. .. .. .. .. .. ..@ seats           : logi FALSE
  .. .. .. .. .. .. .. .. ..@ seatsparameter  : NULL
  .. .. .. .. .. .. .. .. ..@ sigmalim        : NULL
  .. .. .. .. .. .. .. .. ..@ outlier         : NULL
  .. .. .. .. .. .. .. .. ..@ critical        : NULL
  .. .. .. .. .. .. .. .. ..@ outlier_span    : NULL
  .. .. .. .. .. .. .. .. ..@ outlier_method  : NULL
  .. .. .. .. .. .. .. .. ..@ forecast_years  : NULL
  .. .. .. .. .. .. .. .. ..@ backcast_years  : NULL
  .. .. .. .. .. .. .. .. ..@ forecast_conf   : num 0.95
  .. .. .. .. .. .. .. .. ..@ estimate        : logi FALSE
  .. .. .. .. .. .. .. .. ..@ estOutofsample  : logi TRUE
  .. .. .. .. .. .. .. .. ..@ slidingspans    : logi FALSE
  .. .. .. .. .. .. .. .. ..@ aictest         : NULL
  .. .. .. .. .. .. .. .. ..@ onlytd          : logi FALSE
  .. .. .. .. .. .. .. .. ..@ sfshort         : logi FALSE
  .. .. .. .. .. .. .. .. ..@ samode          : NULL
  .. .. .. .. .. .. .. .. ..@ seasonalma      : NULL
  .. .. .. .. .. .. .. .. ..@ trendma         : NULL
  .. .. .. .. .. .. .. .. ..@ x11appendfcst   : logi TRUE
  .. .. .. .. .. .. .. .. ..@ x11appendbcst   : logi FALSE
  .. .. .. .. .. .. .. .. ..@ x11calendarsigma: NULL
  .. .. .. .. .. .. .. .. ..@ x11excludefcst  : logi TRUE
  .. .. .. .. .. .. .. .. ..@ x11final        : chr "user"
  .. .. .. .. .. .. .. .. ..@ x11regress      : logi FALSE
  .. .. .. .. .. ..@ x12OldOutput   :List of 1
  .. .. .. .. .. .. ..$ :Formal class 'x12Output' [package "x12"] with 22 slots
  .. .. .. .. .. .. .. .. ..@ a1       : Time-Series [1:144] from 1949 to 1961: 112 118 132 129 121 135 148 148 136 119 ...
  .. .. .. .. .. .. .. .. ..@ d10      : Time-Series [1:156] from 1949 to 1962: 0.899 0.947 1.057 0.999 0.967 ...
  .. .. .. .. .. .. .. .. ..@ d11      : Time-Series [1:144] from 1949 to 1961: 125 125 125 129 125 ...
  .. .. .. .. .. .. .. .. ..@ d12      : Time-Series [1:144] from 1949 to 1961: 124 125 126 126 126 ...
  .. .. .. .. .. .. .. .. ..@ d13      : Time-Series [1:144] from 1949 to 1961: 1.001 0.997 0.993 1.022 0.99 ...
  .. .. .. .. .. .. .. .. ..@ d16      : Time-Series [1:156] from 1949 to 1962: 0.899 0.947 1.057 0.999 0.967 ...
  .. .. .. .. .. .. .. .. ..@ c17      : Time-Series [1:144] from 1949 to 1961: 1 1 1 0.833 1 ...
  .. .. .. .. .. .. .. .. ..@ d9       : Time-Series [1:144] from 1949 to 1961: NA NA NA 1.02 NA ...
  .. .. .. .. .. .. .. .. ..@ e2       : Time-Series [1:144] from 1949 to 1961: 125 125 125 129 125 ...
  .. .. .. .. .. .. .. .. ..@ d8       : Time-Series [1:144] from 1949 to 1961: 0.903 0.944 1.049 1.02 0.957 ...
  .. .. .. .. .. .. .. .. ..@ b1       : Time-Series [1:156] from 1949 to 1962: 112 118 132 129 121 135 148 148 136 119 ...
  .. .. .. .. .. .. .. .. ..@ sp0      :Formal class 'spectrum' [package "x12"] with 2 slots
  .. .. .. .. .. .. .. .. .. .. ..@ frequency: num [1:61] 0 0.00833 0.01667 0.025 0.03333 ...
  .. .. .. .. .. .. .. .. .. .. ..@ spectrum : num [1:61] -36.1 -35.4 -33.4 -31.3 -30.7 ...
  .. .. .. .. .. .. .. .. ..@ sp1      :Formal class 'spectrum' [package "x12"] with 2 slots
  .. .. .. .. .. .. .. .. .. .. ..@ frequency: num [1:61] 0 0.00833 0.01667 0.025 0.03333 ...
  .. .. .. .. .. .. .. .. .. .. ..@ spectrum : num [1:61] -42.2 -40.8 -36.4 -33.8 -37.1 ...
  .. .. .. .. .. .. .. .. ..@ sp2      :Formal class 'spectrum' [package "x12"] with 2 slots
  .. .. .. .. .. .. .. .. .. .. ..@ frequency: num [1:61] 0 0.00833 0.01667 0.025 0.03333 ...
  .. .. .. .. .. .. .. .. .. .. ..@ spectrum : num [1:61] -42.6 -46.2 -47.9 -45.6 -46.9 ...
  .. .. .. .. .. .. .. .. ..@ spr      :Formal class 'spectrum' [package "x12"] with 2 slots
  .. .. .. .. .. .. .. .. .. .. ..@ frequency: num [1:61] 0 0.00833 0.01667 0.025 0.03333 ...
  .. .. .. .. .. .. .. .. .. .. ..@ spectrum : num [1:61] -36 -34.5 -29.2 -25.2 -30.2 ...
  .. .. .. .. .. .. .. .. ..@ forecast :Formal class 'fbcast' [package "x12"] with 3 slots
  .. .. .. .. .. .. .. .. .. .. ..@ estimate: Time-Series [1:12] from 1961 to 1962: 451 427 482 493 511 ...
  .. .. .. .. .. .. .. .. .. .. ..@ lowerci : Time-Series [1:12] from 1961 to 1962: 420 392 440 446 459 ...
  .. .. .. .. .. .. .. .. .. .. ..@ upperci : Time-Series [1:12] from 1961 to 1962: 485 464 529 545 568 ...
  .. .. .. .. .. .. .. .. ..@ backcast :Formal class 'fbcast' [package "x12"] with 3 slots
  .. .. .. .. .. .. .. .. .. .. ..@ estimate:Formal class 'ts' [package "methods"] with 3 slots
  .. .. .. .. .. .. .. .. .. .. .. .. ..@ .Data   : logi NA
  .. .. .. .. .. .. .. .. .. .. .. .. ..@ tsp     : num [1:3] 1 1 1
  .. .. .. .. .. .. .. .. .. .. .. .. ..@ .S3Class: chr "ts"
  .. .. .. .. .. .. .. .. .. .. ..@ lowerci :Formal class 'ts' [package "methods"] with 3 slots
  .. .. .. .. .. .. .. .. .. .. .. .. ..@ .Data   : logi NA
  .. .. .. .. .. .. .. .. .. .. .. .. ..@ tsp     : num [1:3] 1 1 1
  .. .. .. .. .. .. .. .. .. .. .. .. ..@ .S3Class: chr "ts"
  .. .. .. .. .. .. .. .. .. .. ..@ upperci :Formal class 'ts' [package "methods"] with 3 slots
  .. .. .. .. .. .. .. .. .. .. .. .. ..@ .Data   : logi NA
  .. .. .. .. .. .. .. .. .. .. .. .. ..@ tsp     : num [1:3] 1 1 1
  .. .. .. .. .. .. .. .. .. .. .. .. ..@ .S3Class: chr "ts"
  .. .. .. .. .. .. .. .. ..@ dg       :List of 38
  .. .. .. .. .. .. .. .. .. ..$ x11regress   :List of 1
  .. .. .. .. .. .. .. .. .. .. ..$ x11regress: chr "no"
  .. .. .. .. .. .. .. .. .. ..$ transform    :List of 1
  .. .. .. .. .. .. .. .. .. .. ..$ transform: chr "Automatic selection"
  .. .. .. .. .. .. .. .. .. ..$ samode       :List of 2
  .. .. .. .. .. .. .. .. .. .. ..$ samode     : chr "auto-mode seasonal adjustment"
  .. .. .. .. .. .. .. .. .. .. ..$ finalsamode: chr "multiplicative"
  .. .. .. .. .. .. .. .. .. ..$ seasonalma   :List of 2
  .. .. .. .. .. .. .. .. .. .. ..$ seasonalma     : chr "M.S.R."
  .. .. .. .. .. .. .. .. .. .. ..$ finalseasonalma: chr "3x3"
  .. .. .. .. .. .. .. .. .. ..$ trendma      :List of 2
  .. .. .. .. .. .. .. .. .. .. ..$ trendma     : chr "default"
  .. .. .. .. .. .. .. .. .. .. ..$ finaltrendma: num 9
  .. .. .. .. .. .. .. .. .. ..$ arimamdl     :List of 1
  .. .. .. .. .. .. .. .. .. .. ..$ arimamdl: chr "(1,1,1)(1,1,1)"
  .. .. .. .. .. .. .. .. .. ..$ automdl      :List of 1
  .. .. .. .. .. .. .. .. .. .. ..$ automdl: chr "-"
  .. .. .. .. .. .. .. .. .. ..$ regmdl       :List of 1
  .. .. .. .. .. .. .. .. .. .. ..$ regmdl: chr "none"
  .. .. .. .. .. .. .. .. .. ..$ nout         :List of 1
  .. .. .. .. .. .. .. .. .. .. ..$ nout: num 0
  .. .. .. .. .. .. .. .. .. ..$ nautoout     :List of 1
  .. .. .. .. .. .. .. .. .. .. ..$ nautoout: chr "-"
  .. .. .. .. .. .. .. .. .. ..$ nalmostout   :List of 1
  .. .. .. .. .. .. .. .. .. .. ..$ nalmostout: chr "-"
  .. .. .. .. .. .. .. .. .. ..$ almostoutlier:List of 1
  .. .. .. .. .. .. .. .. .. .. ..$ almostoutlier: chr "-"
  .. .. .. .. .. .. .. .. .. ..$ crit         :List of 1
  .. .. .. .. .. .. .. .. .. .. ..$ crit: chr "-"
  .. .. .. .. .. .. .. .. .. ..$ outlier      :List of 1
  .. .. .. .. .. .. .. .. .. .. ..$ outlier: chr "-"
  .. .. .. .. .. .. .. .. .. ..$ userdefined  :List of 1
  .. .. .. .. .. .. .. .. .. .. ..$ userdefined: chr "-"
  .. .. .. .. .. .. .. .. .. ..$ autooutlier  :List of 1
  .. .. .. .. .. .. .. .. .. .. ..$ autooutlier: chr "-"
  .. .. .. .. .. .. .. .. .. ..$ peaks.seas   :List of 1
  .. .. .. .. .. .. .. .. .. .. ..$ peaks.seas: chr "rsd"
  .. .. .. .. .. .. .. .. .. ..$ peaks.td     :List of 1
  .. .. .. .. .. .. .. .. .. .. ..$ peaks.td: chr "sa irr"
  .. .. .. .. .. .. .. .. .. ..$ id.seas      :List of 1
  .. .. .. .. .. .. .. .. .. .. ..$ id.seas: chr "yes"
  .. .. .. .. .. .. .. .. .. ..$ id.rsdseas   :List of 1
  .. .. .. .. .. .. .. .. .. .. ..$ id.rsdseas: chr "none"
  .. .. .. .. .. .. .. .. .. ..$ spcrsd       :List of 11
  .. .. .. .. .. .. .. .. .. .. ..$ spcrsd.t1   :List of 2
  .. .. .. .. .. .. .. .. .. .. .. ..$ : num 2.6
  .. .. .. .. .. .. .. .. .. .. .. ..$ : chr "+"
  .. .. .. .. .. .. .. .. .. .. ..$ spcrsd.t2   :List of 2
  .. .. .. .. .. .. .. .. .. .. .. ..$ : num 5.7
  .. .. .. .. .. .. .. .. .. .. .. ..$ : chr "+"
  .. .. .. .. .. .. .. .. .. .. ..$ spcrsd.t.dom: chr "t1"
  .. .. .. .. .. .. .. .. .. .. ..$ spcrsd.s1   :List of 2
  .. .. .. .. .. .. .. .. .. .. .. ..$ : num 15.9
  .. .. .. .. .. .. .. .. .. .. .. ..$ : chr "+"
  .. .. .. .. .. .. .. .. .. .. ..$ spcrsd.s2   : chr "nopeak"
  .. .. .. .. .. .. .. .. .. .. ..$ spcrsd.s3   : chr "nopeak"
  .. .. .. .. .. .. .. .. .. .. ..$ spcrsd.s4   : chr "nopeak"
  .. .. .. .. .. .. .. .. .. .. ..$ spcrsd.s5   : chr "nopeak"
  .. .. .. .. .. .. .. .. .. .. ..$ spcrsd.s6   : chr "nopeak"
  .. .. .. .. .. .. .. .. .. .. ..$ spcrsd.s.dom: chr "s1"
  .. .. .. .. .. .. .. .. .. .. ..$ spcrsd.dom  : chr "s1"
  .. .. .. .. .. .. .. .. .. ..$ spcori       :List of 11
  .. .. .. .. .. .. .. .. .. .. ..$ spcori.t1   : chr "nopeak"
  .. .. .. .. .. .. .. .. .. .. ..$ spcori.t2   : chr "nopeak"
  .. .. .. .. .. .. .. .. .. .. ..$ spcori.t.dom: chr "no"
  .. .. .. .. .. .. .. .. .. .. ..$ spcori.s1   :List of 2
  .. .. .. .. .. .. .. .. .. .. .. ..$ : num 20.7
  .. .. .. .. .. .. .. .. .. .. .. ..$ : chr "+"
  .. .. .. .. .. .. .. .. .. .. ..$ spcori.s2   :List of 2
  .. .. .. .. .. .. .. .. .. .. .. ..$ : num 17.7
  .. .. .. .. .. .. .. .. .. .. .. ..$ : chr "+"
  .. .. .. .. .. .. .. .. .. .. ..$ spcori.s3   :List of 2
  .. .. .. .. .. .. .. .. .. .. .. ..$ : num 13.8
  .. .. .. .. .. .. .. .. .. .. .. ..$ : chr "+"
  .. .. .. .. .. .. .. .. .. .. ..$ spcori.s4   :List of 2
  .. .. .. .. .. .. .. .. .. .. .. ..$ : num 16.5
  .. .. .. .. .. .. .. .. .. .. .. ..$ : chr "+"
  .. .. .. .. .. .. .. .. .. .. ..$ spcori.s5   :List of 2
  .. .. .. .. .. .. .. .. .. .. .. ..$ : num 8.5
  .. .. .. .. .. .. .. .. .. .. .. ..$ : chr "+"
  .. .. .. .. .. .. .. .. .. .. ..$ spcori.s6   : chr "nopeak"
  .. .. .. .. .. .. .. .. .. .. ..$ spcori.s.dom: chr "s1"
  .. .. .. .. .. .. .. .. .. .. ..$ spcori.dom  : chr "s1"
  .. .. .. .. .. .. .. .. .. ..$ spcsa        :List of 11
  .. .. .. .. .. .. .. .. .. .. ..$ spcsa.t1   : chr "nopeak"
  .. .. .. .. .. .. .. .. .. .. ..$ spcsa.t2   :List of 2
  .. .. .. .. .. .. .. .. .. .. .. ..$ : num 13.8
  .. .. .. .. .. .. .. .. .. .. .. ..$ : chr "+"
  .. .. .. .. .. .. .. .. .. .. ..$ spcsa.t.dom: chr "t2"
  .. .. .. .. .. .. .. .. .. .. ..$ spcsa.s1   :List of 2
  .. .. .. .. .. .. .. .. .. .. .. ..$ : num 5.9
  .. .. .. .. .. .. .. .. .. .. .. ..$ : chr "+"
  .. .. .. .. .. .. .. .. .. .. ..$ spcsa.s2   : chr "nopeak"
  .. .. .. .. .. .. .. .. .. .. ..$ spcsa.s3   : chr "nopeak"
  .. .. .. .. .. .. .. .. .. .. ..$ spcsa.s4   : chr "nopeak"
  .. .. .. .. .. .. .. .. .. .. ..$ spcsa.s5   : chr "nopeak"
  .. .. .. .. .. .. .. .. .. .. ..$ spcsa.s6   : chr "nopeak"
  .. .. .. .. .. .. .. .. .. .. ..$ spcsa.s.dom: chr "s1"
  .. .. .. .. .. .. .. .. .. .. ..$ spcsa.dom  : chr "no"
  .. .. .. .. .. .. .. .. .. ..$ spcirr       :List of 11
  .. .. .. .. .. .. .. .. .. .. ..$ spcirr.t1   : chr "nopeak"
  .. .. .. .. .. .. .. .. .. .. ..$ spcirr.t2   :List of 2
  .. .. .. .. .. .. .. .. .. .. .. ..$ : num 11.2
  .. .. .. .. .. .. .. .. .. .. .. ..$ : chr "+"
  .. .. .. .. .. .. .. .. .. .. ..$ spcirr.t.dom: chr "t2"
  .. .. .. .. .. .. .. .. .. .. ..$ spcirr.s1   : chr "nopeak"
  .. .. .. .. .. .. .. .. .. .. ..$ spcirr.s2   : chr "nopeak"
  .. .. .. .. .. .. .. .. .. .. ..$ spcirr.s3   : chr "nopeak"
  .. .. .. .. .. .. .. .. .. .. ..$ spcirr.s4   : chr "nopeak"
  .. .. .. .. .. .. .. .. .. .. ..$ spcirr.s5   : chr "nopeak"
  .. .. .. .. .. .. .. .. .. .. ..$ spcirr.s6   : chr "nopeak"
  .. .. .. .. .. .. .. .. .. .. ..$ spcirr.s.dom: chr "no"
  .. .. .. .. .. .. .. .. .. .. ..$ spcirr.dom  : chr "no"
  .. .. .. .. .. .. .. .. .. ..$ q            :List of 1
  .. .. .. .. .. .. .. .. .. .. ..$ q: num 0.26
  .. .. .. .. .. .. .. .. .. ..$ q2           :List of 1
  .. .. .. .. .. .. .. .. .. .. ..$ q2: num 0.29
  .. .. .. .. .. .. .. .. .. ..$ nmfail       :List of 1
  .. .. .. .. .. .. .. .. .. .. ..$ nmfail: num 0
  .. .. .. .. .. .. .. .. .. ..$ loglikelihood:List of 1
  .. .. .. .. .. .. .. .. .. .. ..$ loglikelihood: num 245
  .. .. .. .. .. .. .. .. .. ..$ aic          :List of 1
  .. .. .. .. .. .. .. .. .. .. ..$ aic: num 990
  .. .. .. .. .. .. .. .. .. ..$ aicc         :List of 1
  .. .. .. .. .. .. .. .. .. .. ..$ aicc: num 991
  .. .. .. .. .. .. .. .. .. ..$ bic          :List of 1
  .. .. .. .. .. .. .. .. .. .. ..$ bic: num 1005
  .. .. .. .. .. .. .. .. .. ..$ hq           :List of 1
  .. .. .. .. .. .. .. .. .. .. ..$ hq: num 996
  .. .. .. .. .. .. .. .. .. ..$ aape         :List of 5
  .. .. .. .. .. .. .. .. .. .. ..$ aape.mode: chr "withinsample"
  .. .. .. .. .. .. .. .. .. .. ..$ aape.0   : num 5.43
  .. .. .. .. .. .. .. .. .. .. ..$ aape.1   : num 2.51
  .. .. .. .. .. .. .. .. .. .. ..$ aape.2   : num 5.61
  .. .. .. .. .. .. .. .. .. .. ..$ aape.3   : num 8.18
  .. .. .. .. .. .. .. .. .. ..$ autotransform:List of 1
  .. .. .. .. .. .. .. .. .. .. ..$ autotransform: chr "Log(y)"
  .. .. .. .. .. .. .. .. .. ..$ ifout        : chr "No outlier detection performed"
  .. .. .. .. .. .. .. .. .. ..$ rsd.acf      :'data.frame':	24 obs. of  6 variables:
  .. .. .. .. .. .. .. .. .. .. ..$ lag        : num [1:24] 1 2 3 4 5 6 7 8 9 10 ...
  .. .. .. .. .. .. .. .. .. .. ..$ sample.acf : num [1:24] -0.0264 0.0702 -0.0808 -0.1625 0.0441 ...
  .. .. .. .. .. .. .. .. .. .. ..$ stderr.acf : num [1:24] 0.0874 0.0874 0.0879 0.0884 0.0907 ...
  .. .. .. .. .. .. .. .. .. .. ..$ Ljung-Box.q: num [1:24] 0.0937 0.7599 1.6488 5.2697 5.5386 ...
  .. .. .. .. .. .. .. .. .. .. ..$ df.q       : num [1:24] 0 0 0 0 1 2 3 4 5 6 ...
  .. .. .. .. .. .. .. .. .. .. ..$ pval       : num [1:24] 0 0 0 0 0.0186 ...
  .. .. .. .. .. .. .. .. .. ..$ rsd.pacf     :'data.frame':	24 obs. of  3 variables:
  .. .. .. .. .. .. .. .. .. .. ..$ lag        : num [1:24] 1 2 3 4 5 6 7 8 9 10 ...
  .. .. .. .. .. .. .. .. .. .. ..$ sample.pacf: num [1:24] -0.0264 0.0696 -0.0777 -0.1728 0.0483 ...
  .. .. .. .. .. .. .. .. .. .. ..$ stderr.pacf: num [1:24] 0.0874 0.0874 0.0874 0.0874 0.0874 ...
  .. .. .. .. .. .. .. .. .. ..$ rsd.acf2     :'data.frame':	12 obs. of  3 variables:
  .. .. .. .. .. .. .. .. .. .. ..$ lag        : num [1:12] 1 2 3 4 5 6 7 8 9 10 ...
  .. .. .. .. .. .. .. .. .. .. ..$ sample.acf2: num [1:12] 0.1353 0.0123 0.0307 -0.0834 0.0724 ...
  .. .. .. .. .. .. .. .. .. .. ..$ stderr.acf2: num [1:12] 0.0874 0.089 0.089 0.089 0.0896 ...
  .. .. .. .. .. .. .. .. ..@ seats    : logi FALSE
  .. .. .. .. .. .. .. .. ..@ file     : chr "Series_4"
  .. .. .. .. .. .. .. .. ..@ tblnames : chr [1:11] "a1" "d10" "d11" "d12" ...
  .. .. .. .. .. .. .. .. ..@ Rtblnames: chr [1:11] "Original series" "Final seasonal factors" "Final seasonally adjusted data" "Final trend cycle" ...
  .. .. .. .. .. ..@ tsName         : chr "Series_4"
  .. .. .. .. .. ..@ firstRun       : logi TRUE
  ..@ x12BaseInfo:Formal class 'x12BaseInfo' [package "x12"] with 4 slots
  .. .. ..@ x12path     : chr "d:/x12/x12a.exe"
  .. .. ..@ x13path     : NULL
  .. .. ..@ use         : chr "x12"
  .. .. ..@ showWarnings: logi FALSE
}
\examples{
data(AirPassengersX12Batch)
summary(AirPassengersX12Batch)
}
\keyword{datasets}
