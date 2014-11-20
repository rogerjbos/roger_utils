#' A routine to install all CRAN packages
#'
#' @param none
#' @keywords packages
#' @export
#' @examples
#' installAllPakcages()

"installAllPakcages" <- function() {

    if (length(.libPaths() > 1)) .libPaths(.libPaths()[-1])

    x <- packageStatus(repositories="http://cran.r-project.org/src/contrib")
    st <- x$avai["Status"]
    install.packages(rownames(st)[which(st$Status=="not installed")])
    st <- x$inst["Status"]
    install.packages(rownames(st)[which(st$Status=="upgrade")])

    update.packages(ask=FALSE, checkBuilt=TRUE)

}

#' A routine to install essential packages
#'
#' @param none
#' @keywords packages
#' @export
#' @examples
#' installPakcages()

# Install TinnRcom from c:/Tinn-R/package
"installPakcages" <- function() {

    if (length(.libPaths() > 1)) .libPaths(.libPaths()[-1])

    install.packages('animation')
    install.packages('codetools')
    install.packages('xlsx')
    install.packages('yaml')
    install.packages('htmltools')
    install.packages('rmarkdown')
    install.packages('shinyapps')
    install.packages('installr')
    install.packages('plotGoogleMaps')
    install.packages('ggmap')
    install.packages('xtable')
    install.packages('svSocket')
    install.packages('formatR')
    install.packages('knitr')
    install.packages('Rook')
    install.packages('rpart')
    install.packages('survival')
    install.packages('rJava')
    install.packages('RGtk2')
    install.packages('Rsolnp')
    install.packages('Rcpp')
    install.packages('lubridate')
    install.packages('timeDate')
    install.packages('tseries')
    install.packages('ggplot2')
    install.packages('corpcor')
    install.packages('RODBC')
    install.packages('quadprog')
    install.packages('fortunes')
    install.packages('R2HTML')
    install.packages('prettyR')
    install.packages('rgenoud')
    install.packages('randomForest')
    install.packages('Hmisc')
    install.packages('Rpad')
    install.packages('Defaults')
    install.packages('PerformanceAnalytics')
    install.packages('chron')
    install.packages('lattice')
    install.packages('survival')
    install.packages('svIDE')
    install.packages('lattice')
    install.packages('multic')
    install.packages('pnmath')
    install.packages('dplyr')
    install.packages('financial')
    install.packages('XLConnect')
    install.packages('quantmod')
    install.packages('timeSeries')
    install.packages('XML')
    install.packages('Hmisc')
    install.packages('sqldf')
    install.packages('RCurl')
    install.packages('rbenchmark')
    install.packages('Cairo')
    install.packages('scrapeR')
    install.packages('SciViews')
    install.packages('foreach')
    install.packages('snow')
    install.packages('zoo')
    install.packages('survival')
    install.packages('arules')
    install.packages('party')
    install.packages('plotrix')
    install.packages('devtools')
    install.packages('Quandl')
    install.packages('softImpute')
    install.packages('WDI')
    install.packages('countrycode')
    install.packages('DEoptim')
    install.packages('DEoptimR')
    install.packages('pa')
    install.packages("TTR")
    install.packages("lattice")
    install.packages("digest")
    install.packages("Rcpp")
    install.packages("plyr")
    install.packages("e1071")
    install.packages("sn")
    install.packages("slam")
    install.packages("kernlab")
    install.packages("rneos")
    install.packages("pander")
    install.packages("rmarkdown")
    install.packages("roxygen2")

#    tmp <- c('FRACTION','animation','codetools','xlsx','yaml','htmltools','rmarkdown',
#    'shinyapps','installr','plotGoogleMaps','ggmap','xtable','svSocket','formatR',
#    'knitr','Rook','rpart','survival','rJava','RGtk2','Rsolnp','Rcpp','lubridate',
#    'timeDate','tseries','ggplot2','corpcor','RODBC','quadprog','fortunes','R2HTML',
#    'prettyR','rgenoud','randomForest','Hmisc','Rpad','Defaults','PerformanceAnalytics',
#    'chron','lattice','survival','svIDE','lattice','multic','pnmath','dplyr','financial',
#    'XLConnect','quantmod','timeSeries','XML','Hmisc','sqldf','RCurl','rbenchmark',
#    'Cairo','scrapeR','SciViews','foreach','snow','zoo','survival','arules','party',
#    'plotrix','devtools','Quandl','softImpute','WDI','countrycode','DEoptim','DEoptimR',
#    'pa',"TTR","lattice","digest","Rcpp","plyr","e1071","sn","slam","kernlab","rneos",
#    "pander","rmarkdown","roxygen2")
#    for (pac in seq_along(tmp)) eval(substitute(try(install.packages(target)), list(target = as.name(tmp[pac]))))

    install.packages("xts", repos="http://R-Forge.R-project.org", dependencies = TRUE)
    install.packages("PortfolioAnalytics", repos="http://R-Forge.R-project.org", dependencies = TRUE)
    install.packages("FactorAnalytics", repos="http://R-Forge.R-project.org", dependencies = TRUE)
    install.packages("LSPM",repos="http://R-Forge.R-project.org", dependencies = TRUE)
    install.packages("blotter",repos="http://R-Forge.R-project.org", dependencies = TRUE)
    install.packages("quantstrat",repos="http://R-Forge.R-project.org", dependencies = TRUE)
    install.packages("RTAQ",repos="http://R-Forge.R-project.org", dependencies = TRUE)
    install.packages("FinancialInstrument",repos="http://R-Forge.R-project.org", dependencies = TRUE)
    install.packages("NMOF",repos="http://R-Forge.R-project.org", dependencies = TRUE)
    install.packages("fPortfolio",repos="http://R-Forge.R-project.org", dependencies = TRUE)
    install.packages("fAssets",repos="http://R-Forge.R-project.org", dependencies = TRUE)
    install.packages("knitr",repos="http://R-Forge.R-project.org", dependencies = TRUE)

    install_github("knitcitations", "cboettig")
    install_github("slidify", "ramnathv")
    install_github("slidifyLibraries", "ramnathv")
    install_github("runr", "yihui")

}
