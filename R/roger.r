if (length(.libPaths() > 1)) .libPaths(.libPaths()[-1])
perlPath <- ifelse(version$arch=="x86_64", "c:/perl64/bin/perl.exe", "c:/perl/bin/perl.exe")

options(stringsAsFactors=FALSE)

#' Like SQL isnull(), replace first value with second value if first one is NA
#'
#' @param x first value (may be a vector)
#' @param y second value (may be a vector)
#' @keywords null replace
#' @export
#' @examples
#' isnull(c(1,NA,1), c(0,0,0))

isnull <- function(x,y) {
    x[is.na(x)] <- y[is.na(x)]
    x
}

#' alias for glimpse() function in dplyr
#'
#' @param x dplyr table
#' @keywords glimpse
#' @export
#' @examples
#' gg(x)

gg <- function(x) glimpse(x)

# Convert selected columns to numeric
toN <- function(x, cols) {
    for (jj in cols) x[, jj] <- as.numeric(as.character(x[, jj]))
    return(x)
}

# Make first letter of each word capitalized
properCase <- function(x) {
  s <- strsplit(tolower(x), " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
      sep="", collapse=" ")
}

#setClass('mycharnum', representation=list('character'))
#setAs('character', 'mycharnum' , function(from) as.numeric(from))

gitwd <- function(folder=NULL) {
    if (is.null(folder)) {
        setwd("//rinnycs0051/research/R_HOME")
        print(getwd())
    } else {
        setwd("//rinnycs0051/research/R_HOME_Research/" %+% folder)
        print(getwd())
    }
}

git <- function(cmd="s", arg1="", arg2="") {
    
    if (cmd=="c") { # page 22
  
      system(command='git add *.r', intern = TRUE)
      system(command='git commit -m "' %+% arg1 %+% '"', intern = TRUE)
  
    } else if (cmd=="t") { # page 36
      
      system(command='git tag -a ' %+% arg1 %+% ' -m "' %+% arg2 %+% '"', intern = TRUE)
      system(command='git tag', intern = TRUE)
      
    } else if (cmd=="b") { # page 49
      
      system(command='git branch ' %+% arg1, intern = TRUE)
      system(command='git checkout ' %+% arg1, intern = TRUE)
      
    } else if (cmd=="l") { # page 26
      
      system(command='git log --pretty=format:"%h - %cn, %cr : %s"', intern = TRUE)
      
    } else if (cmd=="co") { # page 49
      
      system(command='git checkout ' %+% arg1, intern = TRUE)
      
    } else if (cmd=="m") { # page 52
      
      system(command='git checkout ' %+% arg1, intern = TRUE)
      system(command='git merge ' %+% arg2, intern = TRUE)
      
    } else if (cmd=="d") { # page 53
      
      system(command='git brach -d ' %+% arg1, intern = TRUE)
      
    } else if (cmd=="s") { # page 53
      
      system(command='git status', intern = TRUE)
    
    } else if (cmd=="a") { # page 53
      
      system(command='git add *.r', intern = TRUE)

    } else if (cmd=="r") { # page 53
      
      system(command='git remote', intern = TRUE)

    } 
    
}


"trim" <- function(x) gsub("^[[:space:]]+|[[:space:]]+$", "", x)

"mysize" <- function(n=15) {
      z <- sapply(ls(env=globalenv()), function(w) object.size(get(w)))
      as.matrix(rev(sort(z))[1:n])
}


"my.ls" <- function (pos = 1, sorted = FALSE, envir = as.environment(pos)) {
    .result <- sapply(ls(envir = envir, all.names = TRUE), function(..x) object.size(eval(as.symbol(..x),envir = envir)))
    if (sorted) {
        .result <- rev(sort(.result))
    }
    .ls <- as.data.frame(rbind(as.matrix(.result), `**Total` = sum(.result)))
    names(.ls) <- "Size"
    .ls$Size <- formatC(.ls$Size, big.mark = ",", digits = 0,
        format = "f")
    .ls$Mode <- c(unlist(lapply(rownames(.ls)[-nrow(.ls)], function(x) mode(eval(as.symbol(x),
        envir = envir)))), "-------")
    .ls
}

"cleaning" <- function() {
   	task <- as.character(sqlQuery(xf, "select top 1 task from roger_cleaning where datediff(day, datadate, getdate()) > 5 order by newid()")$task)
   	upd <- sqlQuery(xf, "UPDATE roger_cleaning SET datadate=getdate() FROM roger_cleaning WHERE task = '" %+% as.character(task) %+% "'")
    email("16466482002@tmomail.net", task, task, "TEXT", "NORMAL")
}


"capwords" <- function(s, strict = FALSE) {
    cap <- function(s) paste(toupper(substring(s,1,1)),{s <- substring(s,2);
    if(strict) tolower(s) else s}, sep = "", collapse = " " )
    sapply(strsplit(s, split = " "), cap, USE.NAMES = !is.null(names(s)))
}
# Use capwords and substitution to make reassonable names for companies
"capNames" <- function(s) {
   s <- trim(s)
   s <- capwords(tolower(as.character(s)))
   s <- gsub("  -cl A", "", s)
   s <- gsub("  -cl B", "", s)
   s <- gsub("Co$", "Co.", s)
   s <- gsub("Corp$", "Corp.", s)
   s <- gsub("Inc$", "Inc.", s)
   s <- gsub("Ltd$", "Ltd.", s)
   return(s)
}


"cls" <- function() {
       require(rcom)
       wsh <- comCreateObject("Wscript.Shell")
       comInvoke(wsh, "SendKeys", "\014")
       invisible(wsh)
}

#maxrun( c(TRUE, TRUE, TRUE, FALSE, FALSE, TRUE, TRUE, FALSE, TRUE))
"maxrun" <- function(x) {
    xx <- (cumsum(!x) + 1) * x
    y <- (seq_along(x) - match(xx, xx) + 1) * x
    max(y)
}

# To read from excel clipboard
"fromExcel" <- function() {read.table("clipboard-128", header=TRUE, sep="\t")}
"toExcel" <- function(x, tag=FALSE) {write.table(x, "clipboard-128", sep="\t", row.names=tag)}

options(scipen=10000)
"%+%" <- function(x,y) {paste(x,y,sep="")}

# Transform data into the unit cube
"unitCube" <- function(x) {
 #x <- temp$bvToPr
    x[x>100] <- 100
				x[x< -100] <- -100
				a <- abs(min(x, na.rm=TRUE))
				b <- max(x, na.rm=TRUE)
			#	if (!is.na(a) && !is.na(b) && a+b==0) b <- b + 1e6
				return((x+a)/(a+b))
}

# Use inverse normal to produce a normal distribution
"tScale" <- function (x)
{
    t1 <- x
    t2 <- x[!is.na(x)]
    r <- qnorm(rank(t2)/(length(rank(t2)) + 1))
    t1[!is.na(t1)] <- r
    t1
}

"aScale" <- function(x, scaled=TRUE) {
		return(wScaleHelper(atan(wScaleHelper(x, scaled)), scaled))
}

# Winsorize and standardize a vector
"wScale" <- function(x, win=FALSE, par1=.02, par2=.02) {
    if (win) {
        return(wScaleWinsorize(wScaleWinsorize(wScaleWinsorize(wScaleWinsorize(wScaleWinsorize(x, par1, par2), par1, par2), par1, par2), par1, par2), par1, par2))
		} else {
        return(wScaleHelper(wScaleHelper(wScaleHelper(wScaleHelper(wScaleHelper(x))))))
    }
}

"wScaleHelper" <- function(x) {

    # Pass 1
    x[is.infinite(x)] <- as.numeric('NA')
    x[x > 100] <- 100
    x[x < -100] <- -100
    if (sum(!is.na(x))==0) {
        x[]  <- 0
    } else {
        m <- mean(x, na.rm=TRUE)
        v <- sd(x, na.rm=TRUE)
        hi <- m+3*v
        lo <- m-3*v
        x[x > hi] <- hi
        x[x < lo] <- lo
        x <- scale(x)
    }
    return(x)
}

"wScaleWinsorize" <- function(x, par1=.02, par2=.02) {

    x[is.infinite(x)] <- as.numeric('NA')
    extreme <- quantile(x, c(par1, 1-par1), na.rm=TRUE)
    x[x > extreme[2]] <- extreme[2]
    x[x < extreme[1]] <- extreme[1]
    if (sum(!is.na(x))==0) {
        x[]  <- 0
    } else {
        m <- mean(x, trim=par2, na.rm=TRUE)
        v <- sd(x, na.rm=TRUE)
        hi <- m+3*v
        lo <- m-3*v
        x[x > hi] <- hi
        x[x < lo] <- lo
        x <- scale(x)
    }
    return(x)
}

#untested code
droplevels <- function(x) {
    if (is.null(levels(x))) stop("no 'levels' attribute")
    nlev <- length(levels(x))
    y <- unclass(x)
    used <- logical(nlev)
    used[] <- FALSE
    used[y] <- TRUE
    tb <- used
    tb[used] <- seq(length=sum(used))
    y[] <- tb[y]
    levels(y) <- levels(x)[used]
    attr(y, "class") <- attr(x, "class")
    y
}

# Round data
"g" <- function(x, digits=4) {
    x[!is.finite(unlist(x))] <- as.numeric('NA')
				x <- round(x, digits)
}

# Replace extreme values wit cutoff
"f" <- function(x, cutoff = 99) {
    x[x < -cutoff] <- -cutoff
    x[x > cutoff] <- cutoff
    return(x)
}


# Turns a numeric gvkey into a 6-digit string like that used in XF database
"gvkeyStr" <- function(g) {
  	len <- nchar(g)
  	fixed <- g
  	# test to not add zeros if its not all numberic
    idx <- !is.na(as.numeric(g))
    fixed[len==1] <- paste("00000",g[len==1], sep="")
		fixed[len==2] <- paste("0000",g[len==2], sep="")
		fixed[len==3] <- paste("000",g[len==3], sep="")
		fixed[len==4] <- paste("00",g[len==4], sep="")
		fixed[len==5] <- paste("0",g[len==5], sep="")
    fixed[!idx] <- g # return original string
   	return(fixed)
}

"iidStr" <- function(g) {
				fixed<-as.character(g)
				len <- nchar(fixed)
				fixed[len==1] <- paste("0",g[len==1], sep="")
				return(fixed)
}

# Returns 1 if leap year, 0 otherwise
"leapyear" <- function(yr) {
				a <- yr%%400
				b <- yr%%100
				c <- yr%%4
				return(ifelse(a==0,1,ifelse(b==0,0,ifelse(c==0,1,0))))
}

# Returns mix, mean, max and % missing for a matrix
"mysummary" <- function(mat) {
				n <- nrow(mat)
				for (j in 1:ncol(mat)) {
								a <- as.numeric(summary(mat[,j]))
								b <- cbind(names(mat)[j], round(a[1],3), round(a[4],3), round(a[6],3), round(a[7]/n,2))
								ifelse(j==1, out <- b, out <- rbind(out,b))
				}
				dimnames(out)[[2]] <- c("Variable", "Min", "Mean", "Max", "Percent NA")
				#dimnames(out)[[2]] <- c("Variable", "Min", "Q1", "Median", "Mean", "Q3", "Max", "Percent NA")
				return(out)
}

# Used by Rpad applications
.HTML.file <- ""
#"HTML.list" <- function (x, file = .HTML.file, first = TRUE, append = TRUE, collapsed = FALSE, align="left", digits=4, ...) {
"HTML.list" <- function (x, file = "", first = TRUE, append = TRUE, collapsed = FALSE, align="left", digits=4, ...) {
   cat("\n", file = file, append = append, ...)
   if (first) { # IE needs contenteditable off
     cat("<div contenteditable='false'><span class='tree'><ul>",
         file = file, append = TRUE, sep = "\n")
   }
   for (i in seq(along=x)) {
     cat('<li style="list-style-image: url(img/', ifelse(collapsed,"plus","minus"),'.gif);',
         ' color: rgb(0, 0, 51); cursor: pointer;">',
         file = file, append = TRUE, sep = "")
     cat(names(x)[i])
     cat("<ul ",ifelse(collapsed,"style='display:none'",""),">",
         file = file, append = TRUE, sep = "\n")
     #print(HTML(x[[i]], file = file, first = FALSE, collapsed=collapsed, align=align, digits=digits, ...))
     R2HTML::HTML(x[[i]], file = file, first = FALSE, collapsed=collapsed, align=align, digits=digits, ...)
     cat("</ul></li>", file = file, append = TRUE, sep = "\n")
   }
   if (first) {
     cat("</ul></span></div>", file = file, append = TRUE, sep = "\n")
   }
}

"firmValue1" <- function(sales, initial.growth, initial.growth.periods, perpetual.growth, discount.rate, op.exp, curr.assets, curr.liab, fixed.assets, depreciation.periods, tax.rate) {
#sales=6250;initial.growth=.49;initial.growth.periods=5;perpetual.growth=.07;discount.rate=.2;op.exp=.56;curr.assets=.15;curr.liab=.10;fixed.assets=1.35;depreciation.periods=33;tax.rate=.525
#should be 6351.12
    dat <- data.frame(sales, initial.growth, initial.growth.periods, perpetual.growth, discount.rate,
           op.exp, curr.assets, curr.liab, fixed.assets, depreciation.periods, tax.rate)
    fv <- mutate(dat, perpetual.growth = pmin((discount.rate-.005), perpetual.growth),
                          adjusted.discount.annuity = (1+discount.rate)/(1+initial.growth)-1,
                          adjusted.discount.perpetuity = (1+discount.rate)/(1+perpetual.growth)-1,
                          average.annuity.factor = (1+1/(1+initial.growth))/2,
                          average.perpetuity.factor = (1+1/(1+perpetual.growth))/2,
                          difference.annuity.factor = 1 - 1/(1+initial.growth),
                          difference.perpetuity.factor = 1 - 1/(1+perpetual.growth),
                          sales.annuity = sales * (((1+adjusted.discount.annuity)^initial.growth.periods - 1) / adjusted.discount.annuity) / ((1+adjusted.discount.annuity)^initial.growth.periods),
                          sales.perpetuity = (sales/adjusted.discount.perpetuity) * ((((1+adjusted.discount.annuity) - 1) / adjusted.discount.annuity)) / ((1+adjusted.discount.annuity)^initial.growth.periods),
                          after.tax.sales = (sales.annuity + sales.perpetuity)*(1-op.exp)*(1-tax.rate),
                          after.tax.dep = fixed.assets * tax.rate * (sales.annuity*average.annuity.factor + sales.perpetuity*average.perpetuity.factor)/depreciation.periods,
                          net.working.capital = (sales.annuity*difference.annuity.factor + sales.perpetuity*difference.perpetuity.factor) * (curr.assets - curr.liab),
                          chg.fixed.assets = (sales.annuity*difference.annuity.factor + sales.perpetuity*difference.perpetuity.factor) * fixed.assets,
                          firm.value = after.tax.sales + after.tax.dep - chg.fixed.assets - net.working.capital)
    return(fv$firm.value)
}
"firmValue2" <- function(sales, initial.growth, initial.growth.periods, perpetual.growth, discount.rate, op.exp, curr.assets, curr.liab, fixed.assets, depreciation.periods, tax.rate) {
#sales=1000;initial.growth=.07;initial.growth.periods=5;perpetual.growth=.04;discount.rate=.15;op.exp=.7;curr.assets=.15;curr.liab=.07;fixed.assets=.9;depreciation.periods=20;tax.rate=.53
#should be 1063.67
    dat <- data.frame(sales, initial.growth, initial.growth.periods, perpetual.growth, discount.rate,
           op.exp, curr.assets, curr.liab, fixed.assets, depreciation.periods, tax.rate)
    fv <- mutate(dat, perpetual.growth = pmin((discount.rate-.005), perpetual.growth),
                           kstar = (((1+discount.rate)/(1+initial.growth))-1),
                           pvsales = (sales/kstar)*(1-(1/(1+kstar)^initial.growth.periods)) + (sales/(1+kstar)^initial.growth.periods)*((1+perpetual.growth)/(discount.rate-perpetual.growth)),
                           pvopexp = pvsales * op.exp,
                           pvdep = pvsales * fixed.assets * (1/depreciation.periods) * (1+(1/(1+curr.liab)))/2,
                           EBIT = pvsales - pvopexp - pvdep,
                           aftax = EBIT * (1-tax.rate),
                           wcap = pvsales *(curr.assets-curr.liab) * (1 - 1/(1+curr.liab)),
                           cfops = aftax + pvdep - wcap,
                           fixed = pvsales * fixed.assets * (1 - 1/(1+curr.liab)),
                           firm.value = cfops - fixed)
    return(fv$firm.value)
}

# Total Least Squares (Orthogonal regression) as described in Better Hedge Ratios
tlsHedgeRatio <- function(p, q) {
  r <- princomp( ~ p + q)
  r$loadings[1,1] / r$loadings[2,1]
}

# Modified version of portfolio.optim
#er is the expected return for each stock in the portfolio
"myport" <- function (x, pm = mean(x), riskless = FALSE, shorts = FALSE,
							rf = 0, reslow = NULL, reshigh = NULL, covmat = cov.shrink(x, verbose=FALSE),
							industry = NULL, industrylow = NULL, industryhigh = NULL, shrink=FALSE, er = NULL, ...)
{

				#if (!require("quadprog", quietly = TRUE))
    #    stop("Package", sQuote("quadprog"), "is required.  Please download from www.cran.r-project.org. Stopping!")
    if (NCOL(x) == 1)
        stop("x is not a matrix. Stopping!")
    if (any(is.na(x)))
        stop("NAs in x. Stopping!")
    k <- dim(x)[2]
    if (!is.matrix(covmat)) {
        stop("covmat is not a matrix. Stopping!")
    }
    if ((dim(covmat)[1] != k) | (dim(covmat)[2] != k)) {
        stop("covmat doesn't have the right dimensions. Stopping!")
    }
    Dmat <- covmat
    dvec <- rep(0, k)
    big <- 1e+100
    if (!is.null(reslow) & is.null(reshigh)) {
        reshigh <- rep(big, k)
    }
    if (is.null(reslow) & !is.null(reshigh)) {
        reslow <- -rep(big, k)
    }
    if (!is.null(reslow)) {
        if (!is.vector(reslow)) {
            stop("reslow is not a vector. Stopping!")
        }
        if (length(reslow) != k) {
            stop("reslow doesn't have the right dimensions. Stopping!")
        }
    }
    if (!is.null(reshigh)) {
        if (!is.vector(reshigh)) {
            stop("reshigh is not a vector. Stopping!")
        }
        if (length(reshigh) != k) {
            stop("reshigh doesn't have the right dimensions. Stopping!")
        }
    }
    if (riskless) {
        a1 <- apply(x, 2, mean) - rf
        if (shorts) {
            a2 <- NULL
            b2 <- NULL
        } else {
            a2 <- matrix(0, k, k)
            diag(a2) <- 1
            b2 <- rep(0, k)
        }
        if (!is.null(reslow) & !is.null(reshigh)) {
            a3 <- matrix(0, k, k)
            diag(a3) <- 1
            Amat <- t(rbind(a1, a2, a3, -a3))
            b0 <- c(pm - rf, b2, reslow, -reshigh)
        } else {
            Amat <- t(rbind(a1, a2))
            b0 <- c(pm - rf, b2)
        }
        res <- solve.QP(Dmat, dvec, Amat, bvec = b0, meq = 1)
    } else {
        a1 <- rep(1, k)
        if (length(er)) {
            a2 <-  er
        } else {
            a2 <- apply(x, 2, mean)
        }
        if (shorts) {
            if (!is.null(reslow) & !is.null(reshigh)) {
                a3 <- matrix(0, k, k)
                diag(a3) <- 1
                Amat <- t(rbind(a1, a2, a3, -a3))
                b0 <- c(1, pm, reslow, -reshigh)
            } else {
                Amat <- t(rbind(a1, a2))
                b0 <- c(1, pm)
            }
        }
        else {
            a3 <- matrix(0, k, k)
            diag(a3) <- 1
            b3 <- rep(0, k)
            if (!is.null(reslow) & !is.null(reshigh)) {
                Amat <- t(rbind(a1, a2, a3, a3, -a3))
                b0 <- c(1, pm, b3, reslow, -reshigh)
            } else {
                Amat <- t(rbind(a1, a2, a3))
                b0 <- c(1, pm, b3)
            }
        }
								if (length(industry)) {
								   ind <- sort(unique(industry))
											indn <- matrix(0,length(ind), length(industry))
											for (j in 1:length(ind)) {
															indn[j, match(industry, ind[j])==1] <- 1
											}
											Amat <- cbind(Amat, t(indn), -t(indn))
											b0 <- c(b0, industrylow, -industryhigh)
								}

								res <- solve.QP(Dmat, dvec, Amat, bvec = b0, meq = 2)

								# Use James-Stein shrinkage estimator iteratively so the weights settle down somewhat
								#if (shrink) {
								#			for (sim in 1:10) {
								#				   wgt <- zapsmall(res$solution)
								#					 	wgt[wgt==0] <- 0.001
								#					 	sumwgt <- sum(wgt)
								#					 	kap <- .001
								#					 	a2.adj <- a2 - sqrt((kap * kap)/t(wgt)*sumwgt)*sumwgt
								#					 	Amat <- t(rbind(a1, a2.adj, a3))
								#			    if (length(industry)) Amat <- cbind(Amat, t(indn), -t(indn))
								#			    res <- solve.QP(Dmat, dvec, Amat, bvec = b0, meq = 2)
								#			}
					   #}
			 }
    y <- t(res$solution %*% t(x))
    ans <- list(pw = zapsmall(res$solution), px = y, pm = mean(y), ps = sd(y), Dmat=Dmat, dvec=dvec, Amat=Amat, bvec=b0)
    return(ans)
}


### THIS CODE SHOULD BE AT THE END ###

# load necessary packages
tmp <- c("corpcor","fortunes","MASS","prettyR","quadprog","RODBC","R2HTML","timeDate","tools",
         "XLConnect","xts","ggplot2","reshape2","dplyr","formatR","Rpad","data.table","devtools",
         "rmarkdown")
for (pac in seq_along(tmp)) eval(substitute(try(library(target), silent=TRUE), list(target = as.name(tmp[pac]))))

options(java.parameters = "-Xmx512m")
if (version$arch=="x86_64") options(java.parameters = "-Xmx2048m")

# Define month-ends taking into account leap years
months <- c("1/31/","2/28/","3/31/","4/30/","5/31/","6/30/","7/31/","8/31/","9/30/","10/31/","11/30/","12/31/")
years <- 1962:2020
leaps <- leapyear(years)
yearVec <- rep(years, each=12)
monthVec <- rep(1:12,length(years))
datec <- paste(months, yearVec, sep="")
leapdate <- rep(leaps, each=12)
test <- (leapdate==1 & substring(datec,1,5)=="2/28/")
datec[test] <- paste("2/29/",yearVec[test],sep="")



sum_indicator <- function(df) sum(df$indicator>0, na.rm=TRUE)
sum_diff <- function(df) sum(df$diff, na.rm=TRUE)
mean_diff <- function(df) mean(df$diff, na.rm=TRUE)
mean_diffalt <- function(df) mean(df$diffalt, na.rm=TRUE)

# NPV of cash flows (usually excluding the PV)
"npv1" <- function(irate, cf, tt = seq(along = cf)) sum(cf / (1+irate)^tt)
# NPV of PV & Cash Flows, then take the absolute value (used by otimize function)
"absnpv1" <- function(irate, pmt, pv) abs(npv1(irate, cf=c(pv, pmt)))
# Numerically solve for the IRR; better than solving for roots
"myIRR" <- function(n, pv, fv, pmt) {

    partial <- (n-floor(n))*pmt
    if (partial > 0) {
        cf <- c(rep(pmt, n), fv+partial)
    } else {
        cf <- c(rep(pmt, n), fv)
    }
 
    # Starting Values
    A <- sum(cf)
    r1 <- (A / abs(pv))^(2/(n+1))-1
    p <- log(A/abs(pv)) / log(A/npv1(r1, cf))
    r2 <- (1+r1)^p -1
    
    # NPV of inflows only (not PV)
    NPVin1 <- npv1(r1, cf)
    NPVin2 <- npv1(r2, cf)
    
    p <- log(NPVin2/abs(pv)) / log(NPVin2/NPVin1)
    # r3 is new estimated value
    r3 <- (1+r2)*((1+r1)/(1+r2))^p - 1

    if (!is.nan(r3) & !is.nan(r2)) {
        # Iterate through estimates until tolerance is met
        for (jj in 1:100) {
            r1 <- r2
            r2 <- r3
        
            NPVin1 <- npv1(r1, cf)
            NPVin2 <- npv1(r2, cf)
            
            p <- log(NPVin2/abs(pv)) / log(NPVin2/NPVin1)
            r3 <- (1+r2)*((1+r1)/(1+r2))^p - 1
            if (is.na(r3) | abs(r3 - r2) <.00001) break
        }
    }
    
    # Return final estimate
    r3
}

"npv" <- function(i, cf, t=seq(along=cf)) sum(cf/(1+i)^t) 
"irr" <- function(cf) {uniroot(npv, c(-2.5, 2.5), cf=cf)$root} 
"urIRR" <- function(n, pv, fv, pmt) {
    partial <- (n-floor(n))*pmt
    cf <- c(pv, rep(pmt, floor(n)), fv+partial)
    irr(cf)  
}     

# Numerically solve for the IRR; better than solving for roots
"myIRRoptimize" <- function(n, pv, fv, pmt) {

    partial <- (n-floor(n))*pmt
    if (partial > 0) {
        pmt <- c(rep(pmt, n), fv+partial)
    } else {
        pmt <- c(rep(pmt, n), fv)
    }
 
    # Return final estimate
    r3 <- optimize(absnpv1, c(-2, 2), pmt, pv)$minimum
    r3
}


#########################################
                       
"tvm" <- function (i = 0, n = 1, pv = 0, fv = 0, pmt = 0, days = 360/pyr, adv = 0, pyr = 12, cyr = pyr) {
    options(warn = -1)
    ii = i
    nn = n
    res = c()
    l = max(c(length(i), length(n), length(pv), length(fv), length(pmt), 
        length(days), length(adv), length(pyr), length(cyr)))
    ii = ii + rep(0, l)
    nn = nn + rep(0, l)
    pv = pv + rep(0, l)
    fv = fv + rep(0, l)
    pmt = pmt + rep(0, l)
    days = days + rep(0, l)
    adv = adv + rep(0, l)
    pyr = pyr + rep(0, l)
    cyr = cyr + rep(0, l)
    for (k in 1:l) {
        b = pv[k]
        p = pmt[k]
        f = fv[k]
        d = days[k]
        a = adv[k]
        i = ii[k]
        n = nn[k]
        py = pyr[k]
        cy = cyr[k]
        if (py != cy) 
            i = irnom(ireff(i, cy), py)
        i = i/(100 * py)
        s = 1
        q = d/(360/py)
        if (sum(is.na(c(b, p, f, i, n, a))) != 1) {
            stop("Incorrect number of Not Available values")
        }
        if (is.na(b)) 
            b = (i + 1)^(-n) * (p * (i + 1)^a * (i * s + 1) - 
                p * (i + 1)^n * (a * i + 1) * (i * s + 1) - f * 
                i)/(i * (i * q + 1))
        if (is.na(f)) 
            f = (p * (i + 1)^(2 * a) * (i * s + 1) - (i + 1)^(a + 
                n) * (2 * a * i * p * (i * s + 1) + b * i * (i * 
                q + 1) + 2 * p * (i * s + 1)) + (i + 1)^(2 * 
                n) * (a * i + 1) * (a * i * p * (i * s + 1) + 
                b * i * (i * q + 1) + p * (i * s + 1)))/(i * 
                ((i + 1)^a - (i + 1)^n * (a * i + 1)))
        if (is.na(p)) 
            p = i * (b * (i + 1)^n * (i * q + 1) + f)/(((i + 
                1)^a - (i + 1)^n * (a * i + 1)) * (i * s + 1))
        if (is.na(n)) 
            n = log((p * (i + 1)^a * (i * s + 1) - f * i)/(a * 
                i * p * (i * s + 1) + b * i * (i * q + 1) + p * 
                (i * s + 1)))/log(i + 1)
        if (is.na(i)) {
            fi = function(i) {
                (-b * (1 + i * q) - f * (1 + i)^(-n))/((1 - (1 + 
                  i)^(-(n - a)))/i + a) - p * (1 + i * s)
            }
            r = try(uniroot(fi, c(0.0000000001, 1/py), tol = 0.0000000001), 
                silent = T)
            if (inherits(r, "try-error")) 
                i = NA
            else i = r$root
        }
        if (is.na(a)) {
            fa = function(a) {
                (-b * (1 + i * q) - f * (1 + i)^(-n))/((1 - (1 + 
                  i)^(-(n - a)))/i + a) - p * (1 + i * s)
            }
            r = try(uniroot(fa, c(0, n), tol = 0.0000000001), 
                silent = T)
            if (inherits(r, "try-error")) 
                a = NA
            else a = r$root
        }
        i = i * py * 100
        res = rbind(res, c(i, n, b, f, p, d, a, py, cy))
    }
    class(res) = "tvm"
    rownames(res) = 1:k
    colnames(res) = c("I%", "#N", "PV", "FV", "PMT", "Days", "#Adv", "P/YR", "C/YR")
    return(res)
}

#########################################

"getSymbols.ALFRED" <- function (Symbols, observationStart='1776-07-04', observationEnd='9999-12-31', vintageDate='2010-12-31', API_KEY='b795bc491f224505db87f9e03b2ded09') {

    require(RCurl)
    require(XML)
    require(xts)
    fred.url <- "http://api.stlouisfed.org/fred/series/observations"
    for (i in 1:length(Symbols)) {
        #Download Data
        doc <- getForm(fred.url, .params = list(
                           series_id=Symbols[[i]],
                           file_type="xml",
                           output_type=2,
                           observation_start=observationStart,
                           observation_end=observationEnd,
                           vintage_date=vintageDate,
                           api_key=API_KEY)
        )
    
        #Parse Data
        out <- xmlToList(xmlParse(doc))
        attrs <- out$.attrs
        out$.attrs <- NULL
        out <- data.frame(do.call(rbind, out))
        as.data.frame(out)

        fr <- xts(as.matrix(as.numeric(as.character(out[, -1]))), as.Date(out[, 1], origin = "1970-01-01"), src = "ALFRED", updated = Sys.time())
        dim(fr) <- c(NROW(fr), 1)
        colnames(fr) <- as.character(toupper(Symbols[[i]]))
        fr <- quantmod:::convert.time.series(fr = fr, return.class = "xts")
        assign(toupper(gsub("\\^", "", Symbols[[i]])), fr, envir = .GlobalEnv)
        rm(fr,out,doc)
        gc()
    }
}

#######################

# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  require(grid)

  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)

  numPlots = length(plots)

  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                    ncol = cols, nrow = ceiling(numPlots/cols))
  }

 if (numPlots==1) {
    print(plots[[1]])

  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

