#' Build the doc entirely from within R
#'
#' @param knit.only Only run knitr, not latex
#' @param make.pdf TRUE to make the pdf, if FALSE it will only go as far as
#'   postscript.
#' @param doc.name Name of the document without extension
#'
#' @details Build the doc entirely from within R.
#'   Make sure you have created the .RData files by sourcing all.r
#'   with the create.rdata.file variables set to TRUE.
#'   Once you have done that and run this function once within an R session,
#'   you can go into the first knitr code chunk in hake-assessment.rnw and
#'   set the call to load.models.into.parent.env() to FALSE,
#'   which will save time for doing the build.
#'
#' @return Nothing
#' @export
#'
#' @examples
#' \donttest
build.doc <- function(knit.only = FALSE,
                      make.pdf  = TRUE,
                      doc.name  = "hake-assessment"){
  ##
  ## knit.only - Only run knitr, not latex
  ## make.pdf - TRUE to make the pdf, if FALSE it will only go as far as
  ##  postscript.

  knit(paste0(doc.name,".rnw"))
  if(!knit.only){
    system(paste0("latex -synctex=1 ", doc.name, ".tex"),
           invisible = FALSE,
           show.output.on.console = FALSE)
    system(paste0("bibtex ", doc.name),
           invisible = FALSE,
           show.output.on.console = TRUE)
    system(paste0("latex ", doc.name, ".tex"),
           invisible = FALSE,
           show.output.on.console = FALSE)
    system(paste0("latex ", doc.name, ".tex"),
           invisible = FALSE,
           show.output.on.console = FALSE)
    system(paste0("dvips ", doc.name,".dvi"),
           invisible = FALSE,
           show.output.on.console = TRUE)
    if(make.pdf){
      shell(paste0("ps2pdf ", doc.name, ".ps"))
    }
  }
}

#' Format x to have supplied number of decimal points
#'
#' @param x The value to format
#' @param dec.points Number of digits after decimal point
#'
#' @details Format x to have supplied number of decimal points.
#'   Make thousands seperated by commas and the number of decimal points given by
#'   dec.points. Strip away any asterisks.
#'
#' @return A string representing the formatted value
#' @export
#'
#' @examples
#' \donttest
f <- function(x, dec.points = 0){
  format(round(x, dec.points),
         big.mark = ",",
         nsmall = dec.points)
}

#' Remove any asterisks found in the given data frame x
#'
#' @param x A data frame
#'
#' @return List of length 2, first is data frame without
#'   asterisks, second is matrix of TRUE for for position
#'   so that the asterisks can be reapplied after formatting.
#' @export
#'
#' @examples
#' \donttest
remove.asterisks <- function(x){

  list(apply(x,
             c(1,2),
             function(y){
               as.numeric(sub("\\*+", "", y))
             }),
       apply(x, c(1,2), function(y)grep("\\*+", y)))
}

#' Add two asterisks to the data frame in the positions
#' given by matrix w
#' @param x A data frame
#' @param w A data frame where 1's represent positions to add asterisks to
#'
#' @details The data frame x and matrix w must be the same dimensions
#'
#' @return
#' @export
#'
#' @examples
#' \donttest
add.asterisks <- function(x, w){
  ## Add two asterisks to the data frame in the positions
  ##  given by matrix 'where' (w)

  p <- x[w == 1]
  p[!is.na(p)] <- paste0("**", p[!is.na(p)])
  x[!is.na(p)] <- p[!is.na(p)]
  x
}

## -----------------------------------------------------------------------------
## Functions to make table generation easier

## Latex newline
latex.nline <- " \\\\ "

## Horizontal line
latex.hline <- " \\hline "

#' Escape percent signs properly with \\ for input to latex
#'
#' @param vec A vector of strings
#'
#' @return The input vector with all instances of % escaped properly for
#'   xtable code - \\\\
#' @export
#'
#' @examples
#' \donttest
latex.perc <- function(vec){

  gsub("%", "\\\\%", vec)
}

#' Make a string of whitespace-padded ampersands
#'
#' @param n Number of ampersands in the string
#'
#' @return A string with n ampersands seperated by spaces. The string will
#'   have one leading and one trailing space.
#' @export
#'
#' @examples
#' \donttest
latex.amp <- function(n = 1){

  paste0(rep(" &", n), " ", collapse = "")
}

#' Make an ampersand-seperated string
#'
#' @param vec A vector of the strings to seperate with ampersands
#'
#' @return A string comprised of each element in the vector vec with an
#'   ampersand in between. The string will have one leading and one
#'   trailing space.
#' @export
#'
#' @examples
#' \donttest
latex.paste <- function(vec){

  paste(" ", vec, " ", collapse = " & ")
}

#' Make a latex bold text string
#'
#' @param txt The text to make bold
#'
#' @return The given text with the latex \\textbf{} macro around it
#' @export
#'
#' @examples
#' \donttest
latex.bold <- function(txt){

  paste0("\\textbf{", txt, "}")
}

#' Make a latex bold math string
#'
#' @param txt A string of the math to make bold
#'
#' @return The given text with the latex \\mathbf{} macro and the
#'   dollar signs around it
#' @export
#'
#' @examples
#' \donttest
latex.math.bold <- function(txt){

  paste0("$\\mathbf{", txt, "}$")
}

#' Make a latex italics math string
#'
#' @param txt The text to italicize
#'
#' @return The given text with the latex \\emph{} macro around it
#' @export
#'
#' @examples
#' \donttest
latex.italics <- function(txt){

  paste0("\\emph{", txt, "}")
}

#' Make an underlining latex string
#'
#' @param txt The text to underline
#'
#' @return The given text with the latex \\underline{} macro around it
#' @export
#'
#' @examples
#' \donttest
latex.under <- function(txt){

  paste0("\\underline{", txt, "}")
}

#' Create a latex string for a multi-line-cell
#'
#' @param latex.vec A vector of strings to put into a multi-line-cell. One for each
#'   line.
#' @param make.bold Make all the text bold
#' @param math.bold If it's math, make the math bold
#'
#' @details If make.bold is TRUE, the \textbf macro will be
#'  inserted unless math.bold is TRUE, then \\mathbf macro will be used
#'
#' @return A string which has been glued together using multi-line-cell
#'   macro for latex.
#' @export
#'
#' @examples
#' \donttest
latex.mlc <- function(latex.vec, make.bold = TRUE, math.bold = FALSE){

  if(make.bold){
    if(math.bold){
      latex.vec <- sapply(latex.vec, latex.math.bold)
    }else{
      latex.vec <- sapply(latex.vec, latex.bold)
    }
  }
  latex.str <- paste(latex.vec, collapse = latex.nline)
  paste0("\\mlc{", latex.str, "}")
}

#' Make a latex string for mulicolumns
#'
#' @param ncol The number of columns
#' @param just Justification, e.g. "l", "c", or "r" for left, center, right
#' @param txt The string to span the columns
#'
#' @return The given text with the latex \\multicolumn{} macro around it
#' @export
#'
#' @examples
#' \donttest
latex.mcol <- function(ncol, just, txt){

  paste0("\\multicolumn{", ncol, "}{", just, "}{", txt, "}")
}

#' Make a latex string for multirows
#'
#' @param nrow The number of rows the text will span
#' @param just Justification, e.g. "l", "c", or "r" for left, center, right
#' @param txt The string to span the rows
#' @param option Optional argument for multirow. See latex documentation.
#'
#' @return The given text with the latex \\multirow{} macro around it
#' @export
#'
#' @examples
#' \donttest
latex.mrow <- function(nrow, just, txt, option = NULL){

  if(is.null(option)){
    paste0("\\multirow{", nrow, "}{", just, "}{", txt, "}")
  }else{
    paste0("\\multirow{", nrow, "}{", just, "}[", option, "]{", txt, "}")
  }
}

#' Make a latex string which for font size and space size
#'
#' @param fnt.size Size of font
#' @param spc.size Amount of vertical space, in same units as font size
#'
#' @return A string which has the given font size and space size applied
#' @export
#'
#' @examples
#' \donttest
latex.size.str <- function(fnt.size, spc.size){

  paste0("\\fontsize{", fnt.size, "}{", spc.size, "}\\selectfont")
}

#' Make the latex string to draw a horizontal line across the columns specified
#' using cline latex command
#'
#' @param cols A string in this format: "1-3" which means
#'   the line should go across columns 1 to 3
#'
#' @return The latex string to draw a horizontal line across the columns specified
#' @export
#'
#' @examples
#' \donttest
latex.cline <- function(cols){

  paste0("\\cline{", cols, "}")
}

#' Make the latex string to draw a horizontal line across the columns specified
#' using cmidrule latex command
#'
#' @param cols A string in this format: "1-3" which means
#'   the line should go across columns 1 to 3
#' @param trim Can be l, r, or lr and tells it to trim the
#'   line a bit so that if there are two lines they don't
#'   touch in the middle. See latex booktabs package
#'
#' @return the latex string to draw a horizontal line across the columns specified
#' @export
#'
#' @examples
#' \donttest
latex.cmidr <- function(cols, trim = "r"){

  paste0("\\cmidrule(", trim, "){", cols, "}")
}

#' Make a latex string with main.txt subscripted by subscr.txt
#'
#' @param main.txt The normal sized text
#' @param subscr.txt The subscript to apply to main.txt
#'
#' @return A latex string with main.txt subscripted by subscr.txt
#' @export
#'
#' @examples
#' \donttest
latex.subscr <- function(main.txt, subscr.txt){

  paste0(main.txt, "\\subscr{", subscr.txt, "}")
}

#' Make a latex string with main.txt subscripted by subscr.txt
#' where only main.txt is italicised
#'
#' @param main.txt The normal sized text
#' @param subscr.txt The subscript to apply to main.txt
#'
#' @return A latex string with main.txt subscripted by subscr.txt
#'   where only main.txt is italicised
#' @export
#'
#' @examples
#' \donttest
latex.subscr.ital <- function(main.txt, subscr.txt){

  paste0("\\emph{", main.txt, "}\\subscr{", subscr.txt, "}")
}

#' Make a latex string with main.txt superscripted by supscr.txt
#'
#' @param main.txt The normal sized text
#' @param supscr.txt The superscript to apply to main.txt
#'
#' @return
#' @export
#'
#' @examples
latex.supscr <- function(main.txt, supscr.txt){

  paste0(main.txt, "\\supscr{", supscr.txt, "}")
}

## -----------------------------------------------------------------------------

#' Calculate the column quantiles for the data frame
#'
#' @param data A data frame
#' @param probs Probabilities to use. See quantile() function
#'
#' @return A vector of the column quantiles for the data frame
#' @export
#'
#' @details The median along with the confidence interval 'ci'
#'   will be calculated and the quantiles returned.
#'
#' @examples
#' \donttest
get.quants <- function(data,
                       probs){

  if(is.null(dim(data))){
    ## It is a single posterior, e.g. sbo
    quants <- quantile(data, probs)
  }else{
    ## It is a timeseries posterior, e.g. sbt
    quants <- apply(data, 2, quantile, probs)
  }
  quants
}

#' Install a package if it isn't already
#'
#' @param package.name Name of the package as a string
#'
#' @return Nothing
#' @export
#'
#' @examples
#' \donttest
install.packages.if.needed <- function(package.name){

  if(!(package.name %in% rownames(installed.packages()))){
    install.packages(package.name)
  }
}

#' Get priors information from prior.str
#'
#' @param prior.str A string like "Lognormal(2.0,1.01)"
#' @param dec.points Number of decimal points to use
#' @param first.to.lower If TRUE, makes the first letter of the name of the prior lower case
#'
#' @return A vector of length 3. e.g. "Lognormal", 2.0, 1.01
#' @export
#'
#' @examples
#' \donttest
split.prior.info <- function(prior.str,
                             dec.points = 1,
                             first.to.lower = FALSE){

  p <- strsplit(prior.str, "\\(")[[1]]
  if(first.to.lower){
    ## Make the name of the prior lower case
    p[1] <- paste0(tolower(substr(p[1], 1, 1)), substr(p[1], 2, nchar(p[1])))
  }
  p.type <- p[1]
  p <- strsplit(p[2], ",")[[1]]
  p.mean <- f(as.numeric(p[1]), dec.points)
  p.sd <- f(as.numeric(gsub(")", "", p[2])), dec.points)
  return(c(p.type, p.mean, p.sd))
}

#' Calculate the catch weight of a given cohort by year
#'
#' @param cohort The year of the cohort
#' @param catage A catch-at-age data frame in the format of iscam
#'   model object, i.e. model$catage
#' @param ages A vector of the ages for the cohort
#'
#' @return A vector of the catch weight by year for the given cohort
#' @export
#'
#' @details Uses the weight-at-age for the cohort by year as a multiplier
#'   with the catch-at-age as found in the iscam model
#'   object. i.e. model$wtatage
#'
#' @examples
#' \donttest
cohortCatch <- function(cohort, catage, ages = 0:20) {

  cohort.yrs <- cohort + ages
  caa <- as.matrix(catage[catage$Yr %in% cohort.yrs, as.character(ages)])
  w <- base.model$wtatage
  w$yr <- w$yr * -1
  waa <- w[w$fleet == 1 & w$yr %in% cohort.yrs, ]
  waa <- waa[, names(waa) %in% ages]
  catch.waa <- as.matrix(caa * waa)

  ind <- 1:(nrow(caa) + 1)
  if(length(ind) > length(ages)){
    ind <- 1:nrow(caa)
  }
  cohort.catch <- diag(catch.waa[,ind])
  names(cohort.catch) <- cohort.yrs[1:(nrow(caa))]
  cohort.catch
}

#' Calculate the age proportions and the age itself for the
#' cohort with the highest, second highest, etc values.
#' Used to find the highest proportion cohort typically
#'
#' @param vec A vector of the age proportions for a given year
#' @param place Which value to return, i.e. 1 = highest value,
#'   2 = second highest, etc.
#'
#' @return
#' @export
#'
#' @examples
get.age.prop <- function(vec, place = 1){

  prop <- rev(sort(vec))
  prop <- prop[place]
  age <- as.numeric(names(vec[vec == prop]))
  return(c(age, prop))
}

#' Make a vector of RGB strings of the specified color and opacity
#'
#' @param color The R color to use
#' @param opacity The opaqueness of the resulting color from 0 to 99
#'
#' @details If color is a single R color string or single number,
#'   returns an rgb string of the specified color and opacity
#'   If color is a vector of cR color strings or numbers,
#'   returns a vector of rgb strings of the specified color and opacity.
#'   If the opacity argument is non-integer or not between 0 and 99, NULL will be returned.
#'   - opacity - 2-decimal-digit string (00-99), i.e. "20" means 20%
#'   Notes: format of returned string is #RRGGBBAA
#'          where RR=red, a 2-hexadecimal-digit string
#'          GG=green, a 2-hexadecimal-digit string
#'          BB=blue, a 2-hexadecimal-digit string
#'          AA=alpha or opacity
#'
#'   The opacity agrument is scalar and will be applied to all colors.

#'
#'
#' @return A vector of rgb strings of the specified color and opacity
#' @export
#'
#' @examples
#' \donttest
get.shade <- function(color, opacity){

  if(!(opacity %% 1 == 0) || opacity<0 || opacity>99){
    warning("opacity argument must be an integer between 0 and 99.")
    return(NULL)
  }
  colorDEC <- col2rgb(color)
  if(is.matrix(colorDEC)){
    colorHEX <- matrix(nrow=3,ncol=ncol(colorDEC))
    shade <- NULL
    for(col in 1:ncol(colorDEC)){
      for(row in 1:nrow(colorDEC)){
        colorHEX[row,col] <- sprintf("%X", colorDEC[row,col])
        if(nchar(colorHEX[row,col])==1){
          colorHEX[row,col] <- paste0("0",colorHEX[row,col])
        }
      }
      shade[col] <- paste0("#",colorHEX[1,col],colorHEX[2,col],colorHEX[3,col],opacity)
    }
  }else{
    colorHEX <- sprintf("%X", colorDEC)
    for(i in 1:length(colorHEX)){
      if(nchar(colorHEX[i])==1){
        colorHEX[i] <- paste0("0",colorHEX[i])
      }
    }
    shade <- paste0("#",colorHEX[1],colorHEX[2],colorHEX[3],opacity)
  }
  shade
}

#' Remove all objects in the workspace except the ones in the vars list
#'
#' @param vars The objects to keep
#'
#' @details Upon finishing, the workspace will contain whatever is in the vars list,
#'   plus the object 'remove.all.objects.except' (this function)
#' @return Nothing
#' @export
#'
#' @examples
#' \donttest
remove.all.objects.except <- function(vars){

  vars <- c(vars, "remove.all.objects.except")
  keep <- match(x = vars, table = ls(all = TRUE, envir = .GlobalEnv))
  if(!any(is.na(keep))){
    rm(list=ls(all = TRUE, envir = .GlobalEnv)[-keep], envir = .GlobalEnv)
  }
}

#' Prepend the given number with zeros
#'
#' @param num The number as a numeric
#' @param digits The number of digits that the resulting string will have
#'
#' @details If the string is less than digits long, it will
#' be prepended with zeroes
#'
#' @return A string with the number prepended with zeros
#' @export
#'
#' @examples
#' cat("pad.num(0, 1) = ", pad.num(0, 1), "\n")
#' cat("pad.num(1, 2) = ", pad.num(1, 2), "\n")
#' cat("pad.num(10, 2) = ", pad.num(10, 2), "\n")
#' cat("pad.num(10, 3) = ", pad.num(10, 3), "\n")
#' cat("pad.num(10, 0) = ", pad.num(10, 0), "\n")
pad.num <- function(num, digits = 0){

  if(digits < 1) stop("Error in pad.num - digits must be positive\n")
  sapply(num, function(x){paste0(rep("0", digits - nchar(as.character(x))), as.character(x))})
}

#' Print out a message stating the model directory names and pretty names
#'
#' @param model.dir.names The names of the models' directories
#' @param model.names The pretty name of the models
#' @param group The group number to use as defined in iscam
#' @param model.type "Bridge" or "Sensitivity" or any string you want to use
#'   to describe the type
#'
#' @return Nothing
#' @export
#'
#' @examples
#' \donttest
print.model.message <- function(model.dir.names, model.names, group, model.type){

  cat0("***")
  cat0(model.type, " model group ", group, " directories: ")
  cat(paste0("  ", model.dir.names), sep = "\n")
  cat0(model.type, " model group ", group, " pretty names: ")
  cat(paste0("  ", model.names), sep = "\n")
  cat0("***")
}

#' Get the current function name from within the function itself
#'
#' @param skipframes Number of call stack frames to skip. Leave at 0 to get current
#'   function name. If you make a wrapper function, it should call this with
#'   skipframes = 1 to avoid the wrapper function name being returned
#' @param skipnames Used to ensure a function name is retrieved
#' @param ret.if.none What to write if called from outside a function
#' @param ret.stack If TRUE, return the call stack as a string
#' @param extra.perf.per.level What string to prepend to the return val n times where
#'   n is the number of functions in the call stack
#'
#' @return The current function's name
#' @export
#'
#' @examples
#' \donttest
curr.fn.finder <- function(skipframes = 0,
                           skipnames = "(FUN)|(.+apply)|(replicate)",
                           ret.if.none = "Not in function",
                           ret.stack = FALSE,
                           extra.perf.per.level = "\t"){

  prefix <- sapply(3 + skipframes + 1:sys.nframe(), function(i){
    currv <- sys.call(sys.parent(n = i))[[1]]
    return(currv)
  })
  prefix[grep(skipnames, prefix)] <- NULL
  prefix <- gsub("function \\(.*", "do.call", prefix)
  if(length(prefix)==0){
    return(ret.if.none)
  }else if(ret.stack){
    return(paste(rev(prefix), collapse = "|"))
  }else{
    retval <- as.character(unlist(prefix[1]))
    if(length(prefix) > 1){
      retval <- paste0(paste(rep(extra.perf.per.level, length(prefix) - 1), collapse = ""), retval)
    }
    return(retval)
  }
}

#' Get the calling function's name followed by ": "
#'
#' @return The calling function's name followed by ": "
#' @export
#'
#' @examples
#' \donttest
get.curr.func.name <- function(){

  func.name <- curr.fn.finder(skipframes = 1) # skipframes=1 is there to avoid returning getCurrFunc itself
  ## Strip extraneous whitespace
  func.name <- gsub("\t+", "", func.name)
  func.name <- gsub("\ +", "", func.name)
  func.name <- paste0(func.name,": ")
  return(func.name)
}

#' Wrapper function to make cat have no space and insert a newline at the end
#'
#' @param ...
#'
#' @return A string with no space and a newline inserted at the end. Like paste0()
#' @export
#'
#' @examples
#' \donttest
cat0 <- function(...){

  cat(..., "\n", sep = "")
}

#' Make a string representing the English spelling of a given number
#'
#' @param x The number to use
#' @param th Include a 'th' after the end. e.g. tenth, twelvth, ninetieth
#' @param cap.first Capitalize the first letter
#'
#' @details All special cases work, e.g. returns ninetieth not ninetyeth
#'
#' @return a string representing the English spelling of a given number
#' @export
#'
#' @examples
#' number.to.word(909)
#' number.to.word(90, TRUE)
#' number.to.word(1000000, TRUE)
#' number.to.word(100000, TRUE)
number.to.word <- function(x, th = FALSE, cap.first = FALSE){

  helper <- function(x){
    digits <- rev(strsplit(as.character(x), "")[[1]])
    nDigits <- length(digits)
    if(nDigits == 1) as.vector(ones[digits])
    else if(nDigits == 2)
      if(x <= 19) as.vector(teens[digits[1]])
      else trim(paste(tens[digits[2]],
                      Recall(as.numeric(digits[1]))))
    else if (nDigits == 3) trim(paste(ones[digits[3]], "hundred and",
                                      Recall(makeNumber(digits[2:1]))))
    else {
      nSuffix <- ((nDigits + 2) %/% 3) - 1
      if (nSuffix > length(suffixes)) stop(paste(x, "is too large!"))
      trim(paste(Recall(makeNumber(digits[
        nDigits:(3*nSuffix + 1)])),
        suffixes[nSuffix],"," ,
        Recall(makeNumber(digits[(3*nSuffix):1]))))
    }
  }
  trim <- function(text){
    ## Tidy leading/trailing whitespace, space before comma
    text=gsub("^\ ", "", gsub("\ *$", "", gsub("\ ,",",",text)))
    ## Clear any trailing " and"
    text=gsub(" and$","",text)
    ##Clear any trailing comma
    gsub("\ *,$","",text)
  }
  makeNumber <- function(...) as.numeric(paste(..., collapse=""))
  ## Disable scientific notation
  opts <- options(scipen=100)
  on.exit(options(opts))
  ones <- c("", "one", "two", "three", "four", "five", "six", "seven",
            "eight", "nine")
  names(ones) <- 0:9
  teens <- c("ten", "eleven", "twelve", "thirteen", "fourteen", "fifteen",
             "sixteen", " seventeen", "eighteen", "nineteen")
  names(teens) <- 0:9
  tens <- c("twenty", "thirty", "forty", "fifty", "sixty", "seventy", "eighty",
            "ninety")
  names(tens) <- 2:9
  x <- round(x)
  suffixes <- c("thousand", "million", "billion", "trillion")
  if (length(x) > 1) return(trim(sapply(x, helper)))
  j <- helper(x)
  ## Cgrandin added the 'th' bit
  if(th){
    j <- strsplit(j, " ")[[1]]
    first <- j[-length(j)]
    last <- j[length(j)]
    if(last == "one"){
      last <- "first"
    }else if(last == "two"){
      last <- "second"
    }else if(last == "three"){
      last <- "third"
    }else if(last == "five"){
      last <- "fifth"
    }else if(last == "eight"){
      last <- "eighth"
    }else if(last == "nine"){
      last <- "ninth"
    }else if(last == "twelve"){
      last <- "twelfth"
    }else if(last == "twenty"){
      last <- "twentieth"
    }else if(last == "thirty"){
      last <- "thirtieth"
    }else if(last == "forty"){
      last <- "fortieth"
    }else if(last == "fifty"){
      last <- "fiftieth"
    }else if(last == "sixty"){
      last <- "sixtieth"
    }else if(last == "seventy"){
      last <- "seventieth"
    }else if(last == "eighty"){
      last <- "eightieth"
    }else if(last == "ninety"){
      last <- "ninetieth"
    }else{
      last <- paste0(last, "th")
    }
    j <- paste(c(first, last), collapse = " ")
  }
  if(cap.first){
    j <- paste0(toupper(substr(j, 1, 1)), substr(j, 2, nchar(j)))
  }
  return(j)
}

## *****************************************************************************
## The following three functions give the ability to assign more than one variable at once.
## Example Call;  Note the use of set.elems()  AND  `%=%`
## Right-hand side can be a list or vector
## set.elems(a, b, c)  %=%  list("hello", 123, list("apples, oranges"))
## set.elems(d, e, f) %=%  101:103
## # Results:
## > a
## [1] "hello"
## > b
## [1] 123
## > c
## [[1]]
## [1] "apples, oranges"
## > d
## [1] 101
## > e
## [1] 102
## > f
## [1] 103

## Generic form
"%=%" <- function(l, r, ...) UseMethod("%=%")

## Binary Operator
"%=%.lhs" <- function(l, r, ...) {
  env <- as.environment(-1)
  if (length(r) > length(l))
    warning("RHS has more args than LHS. Only first", length(l), "used.")
  if (length(l) > length(r))  {
    warning("LHS has more args than RHS. RHS will be repeated.")
    r <- extend.to.match(r, l)
  }
  for(II in 1:length(l)) {
    do.call('<-', list(l[[II]], r[[II]]), envir = env)
  }
}

## Used if LHS is larger than RHS
extend.to.match <- function(src, destin) {
  s <- length(src)
  d <- length(destin)
  # Assume that destin is a length when it is a single number and src is not
  if(d==1 && s>1 && !is.null(as.numeric(destin)))
    d <- destin
  dif <- d - s
  if (dif > 0) {
    src <- rep(src, ceiling(d/s))[1:d]
  }
  return (src)
}

set.elems <- function(...) {
  list.tmp <-  as.list(substitute(list(...)))[-1L]
  class(list.tmp) <-  "lhs"
  return(list.tmp)
}

#' Equivalent of cbind(df, xx) for case where df is an empty data frame
#'
#' @param ...
#'
#' @return A data frame with column xx bound to it
#' @export
#'
#' @examples
#' \donttest
cbind.fill <- function(...){

  nm <- list(...)
  nm <- lapply(nm, as.matrix)
  n <- max(sapply(nm, nrow))
  do.call(cbind, lapply(nm, function (x)
    rbind(x, matrix(, n-nrow(x), ncol(x)))))
}

#' Remove specific columns from a vector
#'
#' @param vec A vector with names set
#' @param names A vector of names to remove from vec
#'
#' @return A vector with some elements removed
#' @export
#'
#' @examples
#' \donttest
strip.columns <- function(vec, names){

  vec[!names(vec) %in% names]
}

#' Make a character vector for use as the align argument of the xtable command
#'
#' @param num The number of columns in the table
#' @param first.left TRUE/FALSE to justify the first column left
#' @param just Justification of the table, including the first column if
#'   first.left is FALSE. Can be any of  "r", "l", or "c"
#'
#' @return A character vector of the justifications for the latex xtable
#' @export
#'
#' @examples
#' \donttest
get.align <- function(num,
                      first.left = TRUE,
                      just = "r"){

  if(first.left){
    align <- c("l", "l")
  }else{
    align <- c(just, just)
  }
  for(i in 1:(num-1)){
    align <- c(align, just)
  }
  align
}

#' Make a vector of rich color strings based on several distributions
#'
#' @param n The number of color strings to return
#' @param alpha The transparency of the colors
#'
#' @return A vector of rich color strings based on several distributions
#' @export
#'
#' @examples
#' \donttest
rc <- rich.colors.short <- function(n, alpha = 1){

  x <- seq(0, 1, length = n)
  r <- 1/(1 + exp(20 - 35 * x))
  g <- pmin(pmax(0, -0.8 + 6 * x - 5 * x^2), 1)
  b <- dnorm(x, 0.25, 0.15)/max(dnorm(x, 0.25, 0.15))
  rgb.m <- matrix(c(r, g, b), ncol = 3)
  rich.vector <- apply(rgb.m, 1, function(v) rgb(v[1],v[2],v[3],alpha = alpha))
}

#' Plot bars (confidence intervals)
#'
#' @param x The x-axis values
#' @param y A data frame with columns:
#'  value: estimate (point) to plot
#'  lo: lower CI
#'  hi: higher CI
#'
#' @param gap To leave a small gap around the point set this to be non-zero
#' @param add TRUE/FALSE Add points also
#' @param ciCol Color of the bars
#' @param ciLty Line type of the bars
#' @param ciLwd Line width of the bars
#' @param ...
#'
#' @return Nothing
#' @export
#'
#' @examples
#' \donttest
plotBars.fn <- function(x,
                        y,
                        gap = 0,
                        scalar = 1e6,
                        add = FALSE,
                        ciCol = "black",
                        ciLty = 1,
                        ciLwd = 1,
                        ...) {

  if(!add) plot(x, y$value / scalar, ...)
  if(add) points(x, y$value / scalar, ...)
  segments(x, y$lo / scalar, x, y$value / scalar - gap, col = ciCol, lty = ciLty, lwd = ciLwd)
  segments(x, y$hi / scalar, x, y$value / scalar + gap, col = ciCol, lty = ciLty, lwd = ciLwd)
}

#' Add a polygon to the current timeseries plot
#'
#' @param yrvec A vector of years on the x-axis
#' @param lower A vector of the y-values for the lower part of the polygon envelope
#' @param upper A vector of the y-values for the upper part of the polygon envelope
#' @param color Color of the lines around the polygon
#' @param shade.col Color of the shade for the envelope. If NULL, the color argument
#'   will be used
#'
#' @return Nothing
#' @export
#'
#' @examples
#' \donttest
addpoly <- function(yrvec, lower, upper, color = 1, shade.col = NULL){

  lower[lower<0] <- 0 ## max of value or 0
  shade.col <- rgb(t(col2rgb(color)), alpha = 0.2 * 255, maxColorValue = 255)

  polygon(x = c(yrvec, rev(yrvec)),
          y = c(lower, rev(upper)),
          border = NA,
          col = shade.col)
  lines(yrvec, lower, lty = 3, col = color)
  lines(yrvec, upper, lty = 3, col = color)
}

#' Writes out some information about the calling function to screen
#'
#' @param ...
#' @param file See cat() function
#' @param sep See cat() function
#' @param fill See cat() function
#' @param labels See cat() function
#' @param append See cat() function
#' @param prefix Numeric value
#'
#' @return Nothing
#' @export
#'
#' @examples
#' \donttest
catw <- function(..., file = "", sep = " ", fill = FALSE, labels = NULL,
                 append = FALSE, prefix = 0){

    if(is.numeric(prefix)){
      ## the +1 is there to avoid returning catw itself
      prefix <- cur.fn.finder(skipframes = prefix + 1)
      prefix <- paste0(prefix, ": ")
    }
    cat(prefix, ..., format(Sys.time(), "(%Y-%m-%d %H:%M:%S)"), "\n",
        file = file, sep = sep, fill = fill, labels = labels, append = append)
}

#' Adds letters to plot panels
#'
#' @param letter The letter to add. If numeric, use letter at that place
#'   in the alphabet.
#'
#' @return Nothing
#' @export
#'
#' @examples
#' \donttest
panel.letter <- function(letter){

  usr <- par("usr")
  inset.x <- 0.05 * (usr[2] - usr[1])
  inset.y <- 0.05 * (usr[4] - usr[3])
  if(is.character(letter)){
    let <- letter
  }else{
    let <- letters[letter]
  }
  text(usr[1] + inset.x,
       usr[4] - inset.y,
       paste0("(", let, ")"),
       cex = 1.,
       font = 1)
}

#' Generate a pretty latex version of the parameter name
#'
#' @param name Parameter name
#' @param addToQ An integer to the parameter name for the q's. This is necessary
#'   because iscam sets the q parameter names to 1, 2, 3... regardless of the
#'   gear number. i.e. if gear 1 is a trawl fishery and gear 2 is a survey,
#'   iscam will call q1 the survey gear. We must add 1 to it to get q2 to
#'   accurately portray the survey gear number
#'
#' @return A pretty version of the parameter name found in variable 'name'
#'   or NULL if the name was not found
#' @export
#'
#' @examples
get.latex.name <- function(name, addToQ = 0){

  if(name == "ro") return(expression("R"[0]))
  if(name == "rbar") return(expression(bar("R")))
  if(name == "rinit") return(expression(bar("R")[init]))
  if(name == "m") return(expression("M"))
  if(name == "bo") return(expression("B"[0]))
  if(name == "vartheta") return(expression(vartheta))
  if(name == "rho") return(expression(rho))
  if(name == "bmsy") return(expression("B"[MSY]))
  if(name == "msy") return(expression("MSY"))
  if(name == "fmsy") return(expression("F"[MSY]))
  if(name == "umsy") return(expression("U"[MSY]))
  if(name == "ssb") return(expression("SSB"))
  if(name == "sel1") return(expression(hat(a)[1]))
  if(name == "selsd1") return(expression(hat(gamma)[1]))
  if(name == "sel2") return(expression(hat(a)[2]))
  if(name == "selsd2") return(expression(hat(gamma)[2]))
  if(name == "sel3") return(expression(hat(a)[3]))
  if(name == "selsd3") return(expression(hat(gamma)[3]))
  if(name == "sel4") return(expression(hat(a)[4]))
  if(name == "selsd4") return(expression(hat(gamma)[4]))
  if(name == "sel5") return(expression(hat(a)[5]))
  if(name == "selsd5") return(expression(hat(gamma)[5]))

  if(name == "log_ro") return(expression("ln(R"[0]*")"))
  if(name == "h") return(expression("h"))
  if(name == "m1") return(expression("M"[1]))
  if(name == "m2") return(expression("M"[2]))
  if(name == "log_m") return(expression("ln(M)"))
  if(name == "log_rbar") return(expression("ln("*bar("R")*")"))
  if(name == "log_rinit") return(expression("ln("*bar("R")[init]*")"))

  if(length(grep("^q[1-9]+$", name))){
    digit <- as.numeric(sub("^q([1-9]+)$", "\\1", name))
    return(substitute("q"[digit], list(digit = digit)))
  }

  if(length(grep("^log_q[1-9]+$", name))){
    digit <- as.numeric(sub("^log_q([1-9]+)$", "\\1", name))
    return(substitute("ln(q"[digit]*")", list(digit = digit)))
  }

  NULL
}

#' Draw a time series confidence envelope on a device on which plot.new
#' has already been called.
#'
#' @param yrs A vector of the years (x-axis values)
#' @param quants A 3-row matrix, where the middle row is the
#'   median and the other two are the lower and upper values for some
#'   confidence interval.
#' @param col Color of the lines for the envelope
#' @param first if TRUE, plot() will be called. If FALSE, lines() will be
#'   called.
#' @param opacity How opaque is the envelope. Values from 0 - 99
#' @param ...
#'
#' @return Nothing
#' @export
#'
#' @examples
#' \donttest
draw.envelope <- function(yrs,
                          quants,
                          col = "black",
                          first,
                          opacity = 75,
                          ...){

  lower  <- quants[1,]
  median <- quants[2,]
  upper  <- quants[3,]

  if(first){
    plot(yrs,
         median,
         type = "l",
         col = col,
         lty = 1,
         lwd = 2,
         ...)
    shade <- get.shade(col, opacity)
    poly.yrs <- c(yrs, rev(yrs))
    poly.ci    <- c(lower, rev(upper))
    polygon(poly.yrs, poly.ci, col = shade)
  }else{
    lines(yrs,
          median,
          type = "l",
          col = col,
          lty = 1,
          lwd = 2,
          ...)
    ## Upper and lower part of CI
    lines(yrs,
          lower,
          col = col,
          lty = 5,
          lwd = 1)
    lines(yrs,
          upper,
          col = col,
          lty = 5,
          lwd = 1)
  }
}

#' Extract the iscam model class objects from the list of model lists,
#'  and merge them into a single model list containing all the iscam model
#'  class objects.
#'
#' @param ...
#'
#' @return A list of singular iscam model objects
#' @export
#'
#' @examples
#' c(model.list.1, model.list.2)
c.model.list <- function(...){

  lst <- list(...)
  ret.lst <- NULL
  ind <- 1
  for(i in 1:length(lst)){
    if(class(lst[[i]]) != model.lst.class){
      stop("List element ", i, " is not of the class '", model.lst.class, "'.")
    }
    for(j in 1:length(lst[[i]])){
      if(class(lst[[i]][[j]]) != model.class){
        stop("Sublist element ", j, " of list element ", i,
             " is not of the class '", model.class, "'.")
      }
      ret.lst[[ind]] <- lst[[i]][[j]]
      ind <- ind + 1
    }
  }
  class(ret.lst) <- model.lst.class
  ret.lst
}

#' Calculation of sigma and tau from rho and vartheta
#'
#' @param rho The rho parameter value
#' @param vartheta The vartheta parameter value
#'
#' @return A list of length 2, first tau, second sigma
#' @export
#'
#' @examples
#' \donttest
calc.sig.tau <- function(rho, vartheta){

  tau <- sqrt((1 - rho) / vartheta)
  sigma <- sqrt(rho / vartheta)
  list(tau, sigma)
}

#' Make a vector of length 2 representing the number of rows and columns
#'   to use to pack a plot in a grid
#'
#' @param num The number of plots
#'
#' @return A vector of length 2 representing the number of rows and
#'   columns necessary to fit the plots in a panel properly
#' @export
#'
#' @examples
#' \donttest
get.rows.cols <- function(num){

  if(num <= 64 && num > 49){
    if(num <= 56){
      nside <- c(8,7)
    }else{
      nside <- c(8,8)
    }
  }else if(num <= 49 && num > 36){
    if(num <= 42){
      nside <- c(7,6)
    }else{
      nside <- c(7,7)
    }
  }else if(num <= 36 && num > 25){
    if(num <= 30){
      nside <- c(6,5)
    }else{
      nside <- c(6,6)
    }
  }else if(num <= 25 && num > 16){
    if(num <= 20){
      nside <- c(5,4)
    }else{
      nside <- c(5,5)
    }
  }else if(num <= 16 && num > 9){
    if(num <= 12){
      nside <- c(4,3)
    }else{
      nside <- c(4,4)
    }
  }else if(num <=  9 && num > 4){
    if(num <= 6){
      nside <- c(3,2)
    }else{
      nside <- c(3,3)
    }
  }else if(num <=  4 && num > 1){
    if(num == 2){
      nside <- c(2,1)
    }else{
      nside <- c(2,2)
    }
  }else{
    nside <- c(1,1)
  }
  return(nside)
}
