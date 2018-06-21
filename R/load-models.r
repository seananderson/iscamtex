#' Construct an iscam model object from its input and output files
#'
#' @param model.dir The directory the model is in
#' @param burnin The number of samples to burn away from the beginning of the MCMC
#' @param thin The thinning to apply to the MCMC posterior samples
#' @param low Lower quantile value to apply to MCMC samples
#' @param high Higher quantile value to apply to MCMC samples
#' @param load.proj Load the projections from the MCMC and do the calculations
#'   to construct the decision tables
#' @param which.stock 1-5 for the five herring stocks: 1=HG, 2=PRD, 3=CC,
#'   4=SOG, 5=WCVI
#' @param which.model 1 = AM1 or 2 = AM2 for herring
#' @param fixed.cutoffs A vector of catch cutoffs to use in decision tables
#' @param verbose Say more
#'
#' @details Load all the iscam files for output and input, and return the model object
#'   If MCMC directory is present, load that and perform calculations for mcmc
#'  parameters.
#'
#' @return An iscam model object
#' @export
#'
#' @examples
#' \donttest{}
load.iscam.files <- function(model.dir,
                             burnin = 1000,
                             thin = 1,
                             low = 0.025,
                             high = 0.975,
                             load.proj = TRUE,
                             which.stock = NULL,
                             which.model = NULL,
                             fixed.cutoffs,
                             verbose = FALSE){

  curr.func.name <- get.curr.func.name()
  model <- list()
  model$path <- model.dir
  ## Get the names of the input files
  inp.files <- fetch.file.names(model.dir, starter.file.name)
  model$dat.file <- inp.files[[1]]
  model$ctl.file <- inp.files[[2]]
  model$proj.file <- inp.files[[3]]

  ## Load the input files
  model$dat <- read.data.file(model$dat.file)
  model$ctl <- read.control.file(model$ctl.file,
                                 model$dat$num.gears,
                                 model$dat$num.age.gears)
  model$proj <- read.projection.file(model$proj.file)
  model$par <- read.par.file(file.path(model.dir, par.file))
  ## Load MPD results
  model$mpd <- read.report.file(file.path(model.dir, rep.file))
  ## Unflatten A_hat so there are nice dataframes of estimated
  ##  numbers-at-age for each gear
  model$mpd$ahat <- calc.ahat(model)
  ## Add sigma and tau
  sigtau <- calc.sig.tau(model$mpd$rho, model$mpd$vartheta)
  model$mpd$tau <- sigtau[[1]]
  model$mpd$sigma <- sigtau[[2]]

  ## Some of the parameters need to be logged
  model$mpd <- calc.mpd.logs(model$mpd)
  model.dir.listing <- dir(model.dir)
  ## Set default mcmc members to NA. Later code depends on this.
  model$mcmc <- NA
  ## Set the mcmc path. This doesn't mean it exists.
  model$mcmcpath <- file.path(model.dir, "mcmc")

  ## If it has an 'mcmc' sub-directory, load it
  if(dir.exists(model$mcmcpath)){
    model$mcmc <- read.mcmc(model$mcmcpath)
    ## Do the mcmc quantile calculations
    model$mcmccalcs <- calc.mcmc(model,
                                 burnin,
                                 thin,
                                 lower = low,
                                 upper = high,
                                 load.proj = load.proj,
                                 which.stock = which.stock,
                                 which.model = which.model,
                                 fixed.cutoffs = fixed.cutoffs)
    model$mcmc$params <- strip.areas.groups(model$mcmc$params)
    model$mcmc$params <- fix.m(model$mcmc$params)
    model$mcmc$params.est <- get.estimated.params(model$mcmc$params)
    model$mcmc$params.est.log <- calc.logs(model$mcmc$params.est)
  }
  class(model) <- model.class
  model
}

#' Delete .Rdata files from all subdirectories
#'
#' @param del.dir The directory which has subdirectories
#'   from which the files will be deleted
#'
#' @return Nothing
#' @export
#'
#' @examples
#' \donttest{}
delete.rdata.files <- function(del.dir = model.dir){

  dirs <- list.dirs(del.dir, recursive = FALSE)
  subdirs <- list.dirs(dirs, recursive = FALSE)

  ## Extract the last two directory names from the full paths to create
  ##  the filenames
  rdata.files <- lapply(subdirs,
                        function(x){
                          d <- strsplit(x, "/")[[1]]
                          fn <- paste0(d[length(d) - 1],
                                       "-",
                                       d[length(d)],
                                       ".Rdata")
                          dd <- file.path(x, fn)})

  nil <- lapply(rdata.files,
                function(x){
                  if(file.exists(x)){
                    unlink(x, force = TRUE)
                    cat(paste0("Deleted ", x, "\n"))
                  }
                })
}

#' Delete all directories and files in a directory
#'
#' @param models.dir Directory name for all models location
#' @param sub.dir The subdirectory to delete recursively from
#'
#' @return Nothing
#' @export
#'
#' @examples
#' \donttest{}
delete.dirs <- function(models.dir = model.dir,
                        sub.dir = NULL){

  dirs <- dir(models.dir)
  files <- file.path(models.dir, dirs, sub.dir)
  unlink(files, recursive = TRUE, force = TRUE)
  cat("All files and directories were deleted from the",
      sub.dir, "directory in each model directory.\n")
}

#' Create a .RData file to hold the model's data and outputs
#'
#' @param models.dir Directory name for all models location
#' @param model.name Directory name of model to be loaded
#' @param ovwrt.rdata overwrite the RData file if it exists? TRUE/FALSE
#' @param load.proj Load the projections from the mcmc and do the calculations
#'   to construct the decision tables
#' @param low Lower quantile value to apply to MCMC samples
#' @param high Higher quantile value to apply to MCMC samples
#' @param burnin The number of samples to burn away from the beginning of the MCMC
#' @param which.stock 1-5 for the five herring stocks: 1=HG, 2=PRD, 3=CC,
#'   4=SOG, 5=WCVI
#' @param which.model 1 = AM1 or 2 = AM2 for herring
#' @param fixed.cutoffs A vector of catch cutoffs to use in decision tables
#' @param verbose Say more
#'
#' @details If an RData file exists, and overwrite is FALSE, return immediately.
#'   If no RData file exists, the model will be loaded from outputs into
#'   an R list and saved as a .RData file in the correct directory.
#'   When this function exits, a .RData file will be located in the
#'   directory given by model.name.
#'   Assumes the files model-setup.r and utilities.r has been sourced.
#'
#' @return Nothing
#' @export
#'
#' @examples
#' \donttest{}
create.rdata.file <- function(models.dir = model.dir,
                              model.name,
                              ovwrt.rdata = FALSE,
                              load.proj = TRUE,
                              low = 0.025,
                              high = 0.975,
                              burnin = 1000,
                              which.stock = NULL,
                              which.model = NULL,
                              fixed.cutoffs,
                              verbose = FALSE){
  ##
  ## If an RData file exists, and overwrite is FALSE, return immediately.
  ## If no RData file exists, the model will be loaded from outputs into
  ##  an R list and saved as an RData file in the correct directory.
  ## When this function exits, an RData file will be located in the
  ##  directory given by model.name.
  ## Assumes the files model-setup.r and utilities.r has been sourced.
  ##
  ## models.dir - directory name for all models location
  ## model.name - directory name of model to be loaded
  ## ovwrt.rdata - overwrite the RData file if it exists?
  ## load.proj - load the projections from the mcmc and do the calculations
  ##  to construct the decision tables
  ## low - lower quantile value to apply to mcmc samples
  ## high - higher quantile value to apply to mcmc samples
  ## which.stock and which.model are passed to load.iscam.files

  curr.func.name <- get.curr.func.name()
  model.dir <- file.path(models.dir, model.name)
  if(!dir.exists(model.dir)){
    stop(curr.func.name,"Error - the directory ", model.dir,
         " does not exist. Fix the problem and try again.\n")
  }
  ## The RData file will have the same name as the directory it is in
  ## If the model.name has a slash in it, remove the slash and
  ##  everything before it. This allows a model to have a name which
  ##  is a path.
  rdata.file <- paste0(strsplit(model.name, "/")[[1]], collapse = "-")
  rdata.file <- file.path(model.dir, paste0(rdata.file, ".RData"))
  if(file.exists(rdata.file)){
    if(ovwrt.rdata){
      ## Delete the RData file
      cat0(curr.func.name, "RData file found in ", model.dir,
           ". Deleting...\n")
      unlink(rdata.file, force = TRUE)
    }else{
      cat0(curr.func.name, "RData file found in ", model.dir, "\n")
      return(invisible())
    }
  }else{
    cat0(curr.func.name, "No RData file found in ", model.dir,
         ". Creating one now.\n")
  }

  ## If this point is reached, no RData file exists so it
  ##  has to be built from scratch
  model <- load.iscam.files(model.dir,
                            low = low,
                            high = high,
                            load.proj = load.proj,
                            burnin = burnin,
                            which.stock = which.stock,
                            which.model = which.model,
                            fixed.cutoffs = fixed.cutoffs)


  ## Save the model as an RData file
  save(model, file = rdata.file)
  invisible()
}

#' Create a .RData file to hold the restrospective model's data and outputs
#'
#' @param model.dir Directory name for all models location
#' @param ovwrt.rdata overwrite the RData file if it exists? TRUE/FALSE
#' @param load.proj Load the projections from the mcmc and do the calculations
#'   to construct the decision tables
#' @param low Lower quantile value to apply to MCMC samples
#' @param high Higher quantile value to apply to MCMC samples
#' @param burnin The number of samples to burn away from the beginning of the MCMC
#' @param which.stock 1-5 for the five herring stocks: 1=HG, 2=PRD, 3=CC,
#'   4=SOG, 5=WCVI
#' @param which.model 1 = AM1 or 2 = AM2 for herring
#' @param verbose Say more
#'
#' @details Create a .RData file to hold the restrospective model's data and outputs.
#'   If an RData file exists, and overwrite is FALSE, return immediately.
#'   If no RData file exists, the model will be loaded from outputs into
#'   an R list and saved as an RData file in the correct directory.
#'   When this function exits, an RData file will be located in the
#'   directory given by model.name.
#'   Assumes the files model-setup.r and utilities.r has been sourced.
#'
#' @return Nothing
#' @export
#'
#' @examples
#' \donttest{}
create.rdata.file.retro <- function(model.dir,
                                    ovwrt.rdata = FALSE,
                                    load.proj = TRUE,
                                    low = 0.025,
                                    high = 0.975,
                                    burnin = 1000,
                                    which.stock = NULL,
                                    which.model = NULL,
                                    verbose = FALSE){
  ##
  ## models.dir - directory name for all models location
  ## ovwrt.rdata - overwrite the RData file if it exists?
  ## load.proj - load the projections from the mcmc and do the calculations
  ##  to construct the decision tables
  ## low - lower quantile value to apply to mcmc samples
  ## high - higher quantile value to apply to mcmc samples
  ## which.stock and which.model are passed to load.iscam.files

  if(!dir.exists(model.dir)){
    warning("Warning - the directory ", model.dir,
            " does not exist. Skipping...\n")
    return(NULL)
  }
  ## The RData file will have the same name as the directory it is in
  ## If the model.dir has a slash in it, remove the slash and
  ##  everything before it. This allows a model to have a name which
  ##  is a path.
  tmp <- strsplit(model.dir, "/")[[1]]
  ## Remove all double-dots
  tmp <- tmp[tmp != ".."]
  ## Keep only the last two strings
  tmp <- tmp[c(length(tmp) - 1, length(tmp))]
  rdata.file <- paste0(tmp, collapse = "-")
  rdata.file <- file.path(model.dir, paste0(rdata.file, ".RData"))
  if(file.exists(rdata.file)){
    if(ovwrt.rdata){
      ## Delete the RData file
      cat0("RData file found in ", model.dir,
           ". Deleting...\n")
      unlink(rdata.file, force = TRUE)
    }else{
      cat0("RData file found in ", model.dir, "\n")
      return(invisible())
    }
  }else{
    cat0("No RData file found in ", model.dir,
         ". Creating one now.\n")
  }

  ## If this point is reached, no RData file exists so it
  ##  has to be built from scratch
  model <- load.iscam.files(model.dir,
                            low = low,
                            high = high,
                            load.proj = load.proj,
                            burnin = burnin,
                            which.stock = which.stock,
                            which.model = which.model)


  ## Save the model as an RData file
  save(model, file = rdata.file)
  invisible()
}

#' Load the iscam models and return as a list of iscam model objects
#'
#' @param models.dir Directory name for all models location
#' @param model.dir.names Vector of directory names of models to be loaded
#'
#' @return A list of iscam model objects
#' @export
#'
#' @examples
#' \donttest{}
load.models <- function(models.dir,
                        model.dir.names){

  rdata.files <- lapply(model.dir.names,
                        function(x){
                          i <- file.path(models.dir, x)
                          j <- sub("/.*", "", x)
                          k <- sub(".*/", "", x)
                          file.path(i, paste0(j, "-", k, ".Rdata"))})

  out <- lapply(1:length(rdata.files),
                function(x){
                  if(!file.exists(rdata.files[[x]])){
                    return(invisible())
                  }
                  load(rdata.files[[x]])
                  if(class(model) != model.class){
                    model <- list(model)
                  }
                  model
                })

  class(out) <- model.lst.class
  out
}

#' Read the iscam starter file to get the iscam input file names
#'
#' @param path Full path to the file
#' @param filename Filename
#'
#' @return A list with three names:
#'   1. Data file name
#'   2. Control file name
#'   3. Projection file name
#'
#' @export
#'
#' @examples
#' \donttest{}
fetch.file.names <- function(path,
                             filename
                             ){

  ## Get the path the file is in
  d <- readLines(file.path(path, filename), warn = FALSE)
  ## Remove comments
  d <- gsub("#.*", "", d)
  ## Remove trailing whitespace
  d <- gsub(" +$", "", d)
  list(file.path(path, d[1]),
       file.path(path, d[2]),
       file.path(path, d[3]))
}

#' Read in the iscam report (.rep) file
#'
#' @param fn Filename (full path)
#'
#' @return A list representing everything in the report file
#' @export
#'
#' @details Read in the iscam report (.rep) file:
#'   File structure:
#'   It is assumed that each text label will be on its own line,
#'   followed by one or more lines of data.
#'   If the label is followed by a single value or line of data,
#'   a vector will be created to hold the data.
#'   If the label is followed by multiple lines of data,
#'   a matrix will be created to hold the data. The matrix might be
#'   ragged so a check is done ahead of time to ensure correct
#'   matrix dimensions.
#'
#'   If a label has another label following it but no data,
#'   that label is thrown away and not included in the returned list.
#'
#'   A label must start with an alphabetic character followed by
#'   any number of alphanumeric characters (includes underscore and .)
#'
#' @examples
#' \donttest{}
read.report.file <- function(fn){

  dat <- readLines(fn, warn = FALSE)
  # Remove preceeding and trailing whitespace on all elements,
  #  but not 'between' whitespace.
  dat <- gsub("^[[:blank:]]+", "", dat)
  dat <- gsub("[[:blank:]]+$", "", dat)

  # Find the line indices of the labels
  # Labels start with an alphabetic character followed by
  # zero or more alphanumeric characters
  idx  <- grep("^[[:alpha:]]+[[:alnum:]]*", dat)
  objs <- dat[idx]     # A vector of the object names
  nobj <- length(objs) # Number of objects
  ret  <- list()
  indname <- 0

  for(obj in 1:nobj){
    indname <- match(objs[obj], dat)
    if(obj != nobj){ # If this is the last object
      inddata <- match(objs[obj + 1], dat)
    }else{
      inddata <- length(dat) + 1 # Next row
    }
    # 'inddiff' is the difference between the end of data
    # and the start of data for this object. If it is zero,
    # throw away the label as there is no data associated with it.
    inddiff <- inddata - indname
    tmp <- NA
    if(inddiff > 1){
      if(inddiff == 2){
        # Create and populate a vector
        vecdat <- dat[(indname + 1) : (inddata - 1)]
        vecdat <- strsplit(vecdat,"[[:blank:]]+")[[1]]
        vecnum <- as.numeric(vecdat)
        ret[[objs[obj]]] <- vecnum
      }else if(inddiff > 2){
        # Create and populate a (possible ragged) matrix
        matdat <- dat[(indname + 1) : (inddata - 1)]
        matdat <- strsplit(c(matdat), "[[:blank:]]+")
        # Now we have a vector of strings, each representing a row
        # of the matrix, and not all may be the same length
        rowlengths <- unlist(lapply(matdat, "length"))
        nrow <- max(rowlengths)
        ncol <- length(rowlengths)
        # Create a new list with elements padded out by NAs
        matdat <- lapply(matdat, function(.ele){c(.ele, rep(NA, nrow))[1:nrow]})
        matnum <- do.call(rbind, matdat)
        mode(matnum) <- "numeric"
        ret[[objs[obj]]] <- matnum
      }
    }else{
      # Throw away this label since it has no associated data.
    }
  }
  return(ret)
}

#' Read in the iscam data file
#'
#' @param file Filename (full path)
#' @param verbose Say more
#'
#' @return A list representing the contents of the iscam data file
#' @export
#'
#' @examples
#' \donttest{}
read.data.file <- function(file = NULL,
                           verbose = FALSE){

  data <- readLines(file, warn=FALSE)
  tmp <- list()
  ind <- 0

  # Remove any empty lines
  data <- data[data != ""]

  # remove preceeding whitespace if it exists
  data <- gsub("^[[:blank:]]+", "", data)

  # Get the element number for the "Gears" names if present
  dat <- grep("^#.*Gears:.+", data)
  tmp$has.gear.names <- FALSE
  if(length(dat > 0)){
    gear.names.str <- gsub("^#.*Gears:(.+)", "\\1", data[dat])
    gear.names <- strsplit(gear.names.str, ",")[[1]]
    tmp$gear.names <- gsub("^[[:blank:]]+", "", gear.names)
    tmp$has.gear.names <- TRUE
  }

  ## Get the element number for the "IndexGears" names if present
  ## dat <- grep("^#.*IndexGears:.+",data)
  ## tmp$hasIndexGearNames <- FALSE
  ## if(length(dat >0)){
  ##   # The gear names were in the file
  ##   indexGearNamesStr <- gsub("^#.*IndexGears:(.+)","\\1",data[dat])
  ##   indexGearNames <- strsplit(indexGearNamesStr,",")[[1]]
  ##   tmp$indexGearNames <- gsub("^[[:blank:]]+","",indexGearNames)
  ##   tmp$hasIndexGearNames <- TRUE
  ## }

  ## # Get the element number for the "AgeGears" names if present (gears with age comp data)
  ## dat <- grep("^#.*AgeGears:.+",data)
  ## tmp$hasAgeGearNames <- FALSE
  ## if(length(dat >0)){
  ##   # The gear names were in the file
  ##   ageGearNamesStr <- gsub("^#.*AgeGears:(.+)","\\1",data[dat])
  ##   ageGearNames <- strsplit(ageGearNamesStr,",")[[1]]
  ##   tmp$ageGearNames <- gsub("^[[:blank:]]+","",ageGearNames)
  ##   tmp$hasAgeGearNames <- TRUE
  ## }

  ## Get the element number for the "CatchUnits" if present
  dat <- grep("^#.*CatchUnits:.+", data)
  if(length(dat > 0)){
    catch.units.str <- gsub("^#.*CatchUnits:(.+)", "\\1", data[dat])
    tmp$catch.units <- gsub("^[[:blank:]]+", "", catch.units.str)
  }

  ## Get the element number for the "IndexUnits" if present
  dat <- grep("^#.*IndexUnits:.+", data)
  if(length(dat > 0)){
    index.units.str <- gsub("^#.*IndexUnits:(.+)", "\\1", data[dat])
    tmp$index.units <- gsub("^[[:blank:]]+", "", index.units.str)
  }

  ## Save the number of specimens per year (comment at end of each age comp
  ##  line), eg. #135 means 135 specimens contributed to the age proportions for
  ##  that year
  age.n <- vector()
  ## Match age comp lines which have N's as comments
  tmp$has.age.comp.n <- FALSE
  pattern <- "^[[:digit:]]{4}[[:space:]]+[[:digit:]][[:space:]]+[[:digit:]][[:space:]]+[[:digit:]][[:space:]]+[[:digit:]].*#([[:digit:]]+).*"
  dat <- data[grep(pattern, data)]
  if(length(dat) > 0){
    for(n in 1:length(dat)){
      age.n[n] <- sub(pattern, "\\1", dat[n])
    }
  }
  ## age.n is now a vector of values of N for the age comp data.
  ## The individual gears have not yet been parsed out, this will
  ##  happen later when the age comps are read in.

  ## Get the element numbers which start with #.
  dat <- grep("^#.*", data)
  ## Remove the lines that start with #.
  dat <- data[-dat]

  ## Remove comments which come at the end of a line
  dat <- gsub("#.*", "", dat)

  ## Remove preceeding and trailing whitespace
  dat <- gsub("^[[:blank:]]+", "", dat)
  dat <- gsub("[[:blank:]]+$", "", dat)

  ## Now we have a nice bunch of string elements which are the inputs for iscam.
  ## Here we parse them into a list structure
  ## This is dependent on the current format of the DAT file and needs to
  ##  be updated whenever the DAT file changes format
  tmp$num.areas  <- as.numeric(dat[ind <- ind + 1])
  tmp$num.groups <- as.numeric(dat[ind <- ind + 1])
  tmp$num.sex    <- as.numeric(dat[ind <- ind + 1])
  tmp$start.yr   <- as.numeric(dat[ind <- ind + 1])
  tmp$end.yr     <- as.numeric(dat[ind <- ind + 1])
  tmp$start.age  <- as.numeric(dat[ind <- ind + 1])
  tmp$end.age    <- as.numeric(dat[ind <- ind + 1])
  tmp$num.gears  <- as.numeric(dat[ind <- ind + 1])

  ## Gear allocation
  tmp$gear.alloc  <- as.numeric(strsplit(dat[ind <- ind + 1],"[[:blank:]]+")[[1]])
  if(!tmp$has.gear.names){
    tmp$gear.names <- 1:length(tmp$gear.alloc)
  }

  ## Age-schedule and population parameters
  tmp$linf      <- as.numeric(strsplit(dat[ind <- ind + 1],"[[:blank:]]+")[[1]])
  tmp$k         <- as.numeric(strsplit(dat[ind <- ind + 1],"[[:blank:]]+")[[1]])
  tmp$to        <- as.numeric(strsplit(dat[ind <- ind + 1],"[[:blank:]]+")[[1]])
  tmp$lw.alpha  <- as.numeric(strsplit(dat[ind <- ind + 1],"[[:blank:]]+")[[1]])
  tmp$lw.beta   <- as.numeric(strsplit(dat[ind <- ind + 1],"[[:blank:]]+")[[1]])
  tmp$age.at.50.mat <- as.numeric(strsplit(dat[ind <- ind + 1],"[[:blank:]]+")[[1]])
  tmp$sd.at.50.mat  <- as.numeric(strsplit(dat[ind <- ind + 1],"[[:blank:]]+")[[1]])
  tmp$use.mat   <- as.numeric(dat[ind <- ind + 1])
  tmp$mat.vec   <- as.numeric(strsplit(dat[ind <- ind + 1],"[[:blank:]]+|,")[[1]])

  ## Delay-difference options
  tmp$dd.k.age   <- as.numeric(dat[ind <- ind + 1])
  tmp$dd.alpha.g <- as.numeric(dat[ind <- ind + 1])
  tmp$dd.rho.g   <- as.numeric(dat[ind <- ind + 1])
  tmp$dd.wk      <- as.numeric(dat[ind <- ind + 1])

  ## Catch data
  tmp$num.catch.obs <- as.numeric(dat[ind <- ind + 1])
  tmp$catch         <- matrix(NA, nrow = tmp$num.catch.obs, ncol = 7)

  for(row in 1:tmp$num.catch.obs){
    tmp$catch[row,] <- as.numeric(strsplit(dat[ind <- ind + 1], "[[:blank:]]+")[[1]])
  }
  colnames(tmp$catch) <- c("year", "gear", "area", "group", "sex", "type", "value")
  ## Abundance indices are a ragged object and are stored as a list of matrices
  tmp$num.indices     <- as.numeric(dat[ind <- ind + 1])
  tmp$num.index.obs   <- as.numeric(strsplit(dat[ind <- ind + 1],"[[:blank:]]+")[[1]])
  tmp$survey.type <- as.numeric(strsplit(dat[ind <- ind + 1], "[[:blank:]]+")[[1]])
  ##nrows <- sum(tmp$nitnobs)
  tmp$indices <- list()
  for(index in 1:tmp$num.indices){
    nrows <- tmp$num.index.obs[index]
    ncols <- 8
    tmp$indices[[index]] <- matrix(NA, nrow = nrows, ncol = ncols)
    for(row in 1:nrows){
      tmp$indices[[index]][row,] <- as.numeric(strsplit(dat[ind <- ind + 1],"[[:blank:]]+")[[1]])
    }
    colnames(tmp$indices[[index]]) <- c("iyr","it","gear","area","group","sex","wt","timing")
  }
  ## Age composition data are a ragged object and are stored as a list of matrices
  tmp$num.age.gears <- as.numeric(dat[ind <- ind + 1])
  ##if(!tmp$hasAgeGearNames){
  ##  tmp$ageGearNames <- 1:length(tmp$nagears)
  ##}

  tmp$num.age.gears.vec       <- as.numeric(strsplit(dat[ind <- ind + 1],"[[:blank:]]+")[[1]])
  tmp$num.age.gears.start.age <- as.numeric(strsplit(dat[ind <- ind + 1],"[[:blank:]]+")[[1]])
  tmp$num.age.gears.end.age   <- as.numeric(strsplit(dat[ind <- ind + 1],"[[:blank:]]+")[[1]])
  tmp$eff                     <- as.numeric(strsplit(dat[ind <- ind + 1],"[[:blank:]]+")[[1]])
  tmp$age.comp.flag           <- as.numeric(strsplit(dat[ind <- ind + 1],"[[:blank:]]+")[[1]])
  tmp$age.comps <- NULL
  ## One list element for each gear (tmp$nagears)
  ## Check to see if there are age comp data
  if(tmp$num.age.gears.vec[1] > 0){
   tmp$age.comps <- list()
   for(gear in 1:tmp$num.age.gears){
     nrows <- tmp$num.age.gears.vec[gear]
     ## 5 of the 6 here is for the header columns
     ncols <- tmp$num.age.gears.end.age[gear] - tmp$num.age.gears.start.age[gear] + 6
     tmp$age.comps[[gear]] <- matrix(NA, nrow = nrows, ncol = ncols)
     for(row in 1:nrows){
       tmp$age.comps[[gear]][row,] <- as.numeric(strsplit(dat[ind <- ind + 1], "[[:blank:]]+")[[1]])
     }
     colnames(tmp$age.comps[[gear]]) <- c("year",
                                          "gear",
                                          "area",
                                          "group",
                                          "sex",
                                          tmp$num.age.gears.start.age[gear]:tmp$num.age.gears.end.age[gear])
   }
  }
  ## Build a list of age comp gear N's
  tmp$age.gears.n <- list()
  start <- 1
  for(ng in 1:length(tmp$num.age.gears.vec)){
    end <- start + tmp$num.age.gears.vec[ng] - 1
    tmp$age.gears.n[[ng]] <- age.n[start:end]
    start <- end + 1
  }
  ## Empirical weight-at-age data
  tmp$num.weight.tab <- as.numeric(dat[ind <- ind + 1])
  tmp$num.weight.obs <- as.numeric(dat[ind <- ind + 1])
  tmp$waa <- NULL

  if(tmp$num.weight.obs > 0){
    ## Parse the weight-at-age data
    nrows       <- tmp$num.weight.obs
    ncols       <- tmp$end.age - tmp$start.age + 6
    tmp$weight.at.age <- matrix(NA, nrow = nrows, ncol = ncols)
    for(row in 1:nrows){
      tmp$weight.at.age[row,] <-
        as.numeric(strsplit(dat[ind <- ind + 1], "[[:blank:]]+")[[1]])
    }
    colnames(tmp$weight.at.age) <- c("year",
                                     "gear",
                                     "area",
                                     "group",
                                     "sex",
                                     tmp$start.age:tmp$end.age)
  }

  ## Annual Mean Weight data
  ## Catch data
  tmp$num.mean.weight <- as.numeric(dat[ind <- ind + 1])
  tmp$num.mean.weight.obs <- as.numeric(dat[ind <- ind + 1])
  if(tmp$num.mean.weight.obs >0){
    tmp$mean.weight.data  <- matrix(NA, nrow = sum(tmp$num.mean.weight.obs), ncol = 7)
    for(row in 1:sum(tmp$num.mean.weight.obs)){
      tmp$mean.weight.data[row,] <- as.numeric(strsplit(dat[ind <- ind + 1],"[[:blank:]]+")[[1]])
    }
    colnames(tmp$mean.weight.data) <- c("year",
                                        "meanwt",
                                        "gear",
                                        "area",
                                        "group",
                                        "sex",
                                        "timing")
  }
  tmp$eof <- as.numeric(dat[ind <- ind + 1])
  tmp
}

#' Read in the iscam control file
#'
#' @param file Filename
#' @param num.gears The total number of gears in the datafile
#' @param num.age.gears The number of gears with age composition information
#'   in the datafile
#' @param verbose Say more
#'
#' @return A list representing the contents on the iscam control file
#' @export
#'
#' @examples
#' \donttest{}
read.control.file <- function(file = NULL,
                              num.gears = NULL,
                              num.age.gears = NULL,
                              verbose = FALSE){
  ## Read in the iscam control file given by 'file'
  ## Parses the file into its constituent parts and returns a list of the
  ##  contents.
  ## num.gears is the total number of gears in the datafile
  ## num.age.gears in the number of gears with age composition information in the
  ##  datafile

  curr.func <- get.curr.func.name()
  if(is.null(num.gears)){
    cat0(curr.func,
         "You must supply the total number of gears (num.gears). ",
         "Returning NULL.")
    return(NULL)
  }
  if(is.null(num.age.gears)){
    cat0(curr.func,
         "You must supply the number of gears with age composition ",
         "(num.age.gears). Returning NULL.")
    return(NULL)
  }

  data <- readLines(file, warn = FALSE)

  ## Remove any empty lines
  data <- data[data != ""]

  ## Remove preceeding whitespace if it exists
  data <- gsub("^[[:blank:]]+", "", data)

  ## Get the element numbers which start with #.
  dat <- grep("^#.*", data)
  ## Remove the lines that start with #.
  dat <- data[-dat]

  ## Save the parameter names, since they are comments and will be deleted in
  ##  subsequent steps.
  ## To get the npar, remove any comments and preceeding and trailing
  ##  whitespace for it.
  dat1 <- gsub("#.*", "", dat[1])
  dat1 <- gsub("^[[:blank:]]+", "", dat1)
  dat1 <- gsub("[[:blank:]]+$", "", dat1)
  n.par <- as.numeric(dat1)
  param.names <- vector()
  ## Lazy matching with # so that the first instance matches, not any other
  pattern <- "^.*?#([[:alnum:]]+_*[[:alnum:]]*).*"
  for(param.name in 1:n.par){
    ## Each parameter line in dat which starts at index 2,
    ##  retrieve the parameter name for that line
    param.names[param.name] <- sub(pattern, "\\1", dat[param.name + 1])
  }
  ## Now that parameter names are stored, parse the file.
  ##  remove comments which come at the end of a line
  dat <- gsub("#.*", "", dat)

  ## Remove preceeding and trailing whitespace
  dat <- gsub("^[[:blank:]]+", "", dat)
  dat <- gsub("[[:blank:]]+$", "", dat)

  ## Now we have a nice bunch of string elements which are the inputs for iscam.
  ## Here we parse them into a list structure.
  ## This is dependent on the current format of the CTL file and needs to
  ## be updated whenever the CTL file changes format.
  tmp <- list()
  ind <- 0
  tmp$num.params <- as.numeric(dat[ind <- ind + 1])
  tmp$params <- matrix(NA, nrow = tmp$num.params, ncol = 7)
  for(param in 1:tmp$num.params){
    tmp$params[param,] <-
      as.numeric(strsplit(dat[ind <- ind + 1],"[[:blank:]]+")[[1]])
  }
  colnames(tmp$params) <- c("ival","lb","ub","phz","prior","p1","p2")
  ## param.names is retreived at the beginning of this function
  rownames(tmp$params) <- param.names

  ## Age and size composition control parameters and likelihood types
  nrows <- 8
  ncols <- num.age.gears
  tmp$age.size <- matrix(NA, nrow = nrows, ncol = ncols)
  for(row in 1:nrows){
    tmp$age.size[row,] <-
      as.numeric(strsplit(dat[ind <- ind + 1],"[[:blank:]]+")[[1]])
  }
  ## Rownames here are hardwired, so if you add a new row you must add a name
  ##  for it here
  rownames(tmp$age.size) <- c("gearind",
                              "likelihoodtype",
                              "minprop",
                              "comprenorm",
                              "logagetau2phase",
                              "phi1phase",
                              "phi2phase",
                              "degfreephase")
  ## Ignore the int check value
  ind <- ind + 1

  ## Selectivity parameters for all gears
  nrows <- 10
  ncols <- num.gears
  tmp$sel <- matrix(NA, nrow = nrows, ncol = ncols)
  for(row in 1:nrows){
    tmp$sel[row,] <-
      as.numeric(strsplit(dat[ind <- ind + 1],"[[:blank:]]+")[[1]])
  }
  ## Rownames here are hardwired, so if you add a new row you must add a name
  ##  for it here
  rownames(tmp$sel) <- c("iseltype",
                         "agelen50log",
                         "std50log",
                         "nagenodes",
                         "nyearnodes",
                         "estphase",
                         "penwt2nddiff",
                         "penwtdome",
                         "penwttvs",
                         "nselblocks")

  ## Start year for time blocks, one for each gear
  max.block <- max(tmp$sel[10,])
  tmp$start.yr.time.block <- matrix(nrow = num.gears, ncol = max.block)
  for(ng in 1:num.gears){
    ## Pad the vector with NA's to make it the right size if it isn't
    ##  maxblocks size.
    tmp.vec <- as.numeric(strsplit(dat[ind <- ind + 1],"[[:blank:]]+")[[1]])
    if(length(tmp.vec) < max.block){
      for(i in (length(tmp.vec) + 1):max.block){
        tmp.vec[i] <- NA
      }
    }
    tmp$start.yr.time.block[ng,] <- tmp.vec
  }

  ## Priors for survey Q, one column for each survey
  tmp$num.indices <- as.numeric(dat[ind <- ind + 1])
  nrows <- 3
  ncols <- tmp$num.indices
  tmp$surv.q <- matrix(NA, nrow = nrows, ncol = ncols)
  for(row in 1:nrows){
    tmp$surv.q[row,] <-
      as.numeric(strsplit(dat[ind <- ind + 1],"[[:blank:]]+")[[1]])
  }
  ## Rownames here are hardwired, so if you add a new row you must add a name
  ##  for it here.
  rownames(tmp$surv.q) <- c("priortype",
                            "priormeanlog",
                            "priorsd")

  ## Controls for fitting to mean weight data
  tmp$fit.mean.weight <- as.numeric(dat[ind <- ind + 1])
  tmp$num.mean.weight.cv <- as.numeric(dat[ind <- ind + 1])
  n.vals <- tmp$num.mean.weight.cv
  tmp$weight.sig <-  vector(length = n.vals)
  for(val in 1:n.vals){
    tmp$weight.sig[val] <- as.numeric(dat[ind <- ind + 1])
  }

  ## Miscellaneous controls
  n.rows <- 20
  tmp$misc <- matrix(NA, nrow = n.rows, ncol = 1)
  for(row in 1:n.rows){
    tmp$misc[row, 1] <- as.numeric(dat[ind <- ind + 1])
  }
  ## Rownames here are hardwired, so if you add a new row you must add a name
  ##  for it here.
  rownames(tmp$misc) <- c("verbose",
                          "rectype",
                          "sdobscatchfirstphase",
                          "sdobscatchlastphase",
                          "unfishedfirstyear",
                          "maternaleffects",
                          "meanF",
                          "sdmeanFfirstphase",
                          "sdmeanFlastphase",
                          "mdevphase",
                          "sdmdev",
                          "mnumestnodes",
                          "fracZpriorspawn",
                          "agecompliketype",
                          "IFDdist",
                          "fitToMeanWeight",
                          "calculateMSY",
                          "runSlowMSY",
                          "slowMSYPrecision",
                          "slowMSYMaxF")
  tmp$eof <- as.numeric(dat[ind <- ind + 1])
  tmp
}

#' Read in the contents of the iscam projection file
#'
#' @param file Filename
#' @param verbose Say more
#'
#' @return A list representing the contents of the iscam projection file
#' @export
#'
#' @examples
#' \donttest{}
read.projection.file <- function(file = NULL,
                                 verbose = FALSE){

  data <- readLines(file, warn = FALSE)

  ## Remove any empty lines
  data <- data[data != ""]

  ## remove preceeding whitespace if it exists
  data <- gsub("^[[:blank:]]+", "", data)

  ## Get the element numbers which start with #.
  dat <- grep("^#.*", data)
  ## remove the lines that start with #.
  dat <- data[-dat]

  ## remove comments which come at the end of a line
  dat <- gsub("#.*", "", dat)

  ## remove preceeding and trailing whitespace
  dat <- gsub("^[[:blank:]]+", "", dat)
  dat <- gsub("[[:blank:]]+$", "", dat)

  ## Now we have a nice bunch of string elements which are the inputs for iscam.
  ## Here we parse them into a list structure.
  ## This is dependent on the current format of the DAT file and needs to
  ##  be updated whenever the proj file changes format.
  tmp <- list()
  ind <- 0

  ## Get the TAC values
  tmp$num.tac  <- as.numeric(dat[ind <- ind + 1])
  for(tac in 1:tmp$num.tac){
    ## Read in the tacs, one, per line
    tmp$tac.vec[tac] <- as.numeric(dat[ind <- ind + 1])
  }

  ## If the tac vector is on one line
  ##tmp$tac.vec <- as.numeric(strsplit(dat[ind <- ind + 1],"[[:blank:]]+")[[1]])

  ## Get the control options vector
  tmp$num.ctl.options <- as.numeric(dat[ind <- ind + 1])
  n.rows <- tmp$num.ctl.options
  n.cols <- 1
  tmp$ctl.options  <- matrix (NA, nrow = n.rows, ncol = n.cols)
  for(row in 1:n.rows){
    tmp$ctl.options[row, 1] <- as.numeric(dat[ind <- ind + 1])
  }
  ## Rownames here are hardwired, so if you add a new row you must add a name
  ##  or it here.
  option.names <- c("syrmeanm",
                    "nyrmeanm",
                    "syrmeanfecwtageproj",
                    "nyrmeanfecwtageproj",
                    "syrmeanrecproj",
                    "nyrmeanrecproj",
                    "shortcntrlpts",
                    "longcntrlpts",
                    "bmin")
  rownames(tmp$ctl.options) <- option.names[1:tmp$num.ctl.options]
  tmp$eof <- as.numeric(dat[ind <- ind + 1])
  tmp
}

#' Read in the contents of the iscam par file
#'
#' @param file Filename (full path)
#' @param verbose Say more
#'
#' @return A list representing the contents of the iscam par file
#' @export
#'
#' @examples
#' \donttest{}
read.par.file <- function(file = NULL,
                          verbose = FALSE){

  data <- readLines(file, warn = FALSE)
  tmp <- list()
  ind <- 0

  ## Remove preceeding #
  conv.check <- gsub("^#[[:blank:]]*", "", data[1])
  ## Remove all letters, except 'e'
  ##convCheck <- gsub("[[:alpha:]]+","",convCheck)
  convCheck <- gsub("[abcdfghijklmnopqrstuvwxyz]",
                    "",
                    conv.check,
                    ignore.case = TRUE)
  ## Remove the equals signs
  conv.check <- gsub("=", "", conv.check)
  ## Remove all preceeding and trailing whitespace
  conv.check <- gsub("^[[:blank:]]+", "", conv.check)
  conv.check <- gsub("[[:blank:]]+$", "", conv.check)
  ## Remove the non-numeric parts
  conv.check <- strsplit(conv.check, " +")[[1]]
  conv.check <- conv.check[grep("^-?[[:digit:]]", conv.check)]
  ## The following values are saved for appending to the tmp list later

  num.params   <- conv.check[1]
  obj.fun.val <-  format(conv.check[2], digits = 6, scientific = FALSE)
  max.gradient <-  format(conv.check[3], digits = 8, scientific = FALSE)

  ##Remove the first line from the par data since we already parsed it and saved the values
  data <- data[-1]

  ## At this point, every odd line is a comment and every even line is the value.
  ## Parse the names from the odd lines (oddData) and parse the
  ## values from the even lines (evenData)
  odd.elem <- seq(1, length(data), 2)
  even.elem <- seq(2, length(data), 2)
  odd.data <- data[odd.elem]
  even.data <- data[even.elem]

  ## Remove preceeding and trailing whitespace if it exists from both
  ##  names and values.
  names <- gsub("^[[:blank:]]+", "", odd.data)
  names <- gsub("[[:blank:]]+$", "", names)
  values <- gsub("^[[:blank:]]+", "", even.data)
  values <- gsub("[[:blank:]]+$", "", values)

  ## Remove the preceeding # and whitespace and the trailing : from the names
  pattern <- "^#[[:blank:]]*(.*)[[:blank:]]*:"
  names <- sub(pattern, "\\1", names)

  ## Remove any square brackets from the names
  names <- gsub("\\[|\\]", "", names)

  data.length <- length(names)
  for(item in 1:(data.length)){
    tmp[[item]] <-
      as.numeric(strsplit(values[ind <- ind + 1], "[[:blank:]]+")[[1]])
  }

  names(tmp) <- names
  tmp$num.params <- num.params
  tmp$obj.fun.val <- as.numeric(obj.fun.val)
  tmp$max.gradient <- as.numeric(max.gradient)
  tmp
}

#' Read in the MCMC results from an iscam model
#'
#' @param model.dir Directory in which the model output resides
#' @param verbose Say more
#'
#' @return A list representing the MCMC output of the iscam model,
#'   or NULL if there was a problem or there werer no MCMC output files
#' @export
#'
#' @examples
#' \donttest{}
read.mcmc <- function(model.dir = NULL,
                      verbose = TRUE){

  curr.func <- get.curr.func.name()
  if(is.null(model.dir)){
    cat0(curr.func,
         "You must supply a directory name (model.dir). Returning NULL.")
    return(NULL)
  }
  mcmcfn     <- file.path(model.dir, mcmc.file)
  mcmcsbtfn  <- file.path(model.dir, mcmc.biomass.file)
  mcmcrtfn   <- file.path(model.dir, mcmc.recr.file)
  mcmcrdevfn <- file.path(model.dir, mcmc.recr.devs.file)
  mcmcftfn   <- file.path(model.dir, mcmc.fishing.mort.file)
  mcmcmfn    <- file.path(model.dir, mcmc.natural.mort.file)
  mcmcutfn   <- file.path(model.dir, mcmc.fishing.mort.u.file)
  mcmcvbtfn  <- file.path(model.dir, mcmc.vuln.biomass.file)
  mcmcprojfn <- file.path(model.dir, mcmc.proj.file)

  tmp        <- list()
  if(file.exists(mcmcfn)){
    tmp$params <- read.csv(mcmcfn)
  }
  if(file.exists(mcmcsbtfn)){
    sbt        <- read.csv(mcmcsbtfn)
    tmp$sbt    <- extract.group.matrices(sbt, prefix = "sbt")
  }
  if(file.exists(mcmcrtfn)){
    rt         <- read.csv(mcmcrtfn)
    tmp$rt     <- extract.group.matrices(rt, prefix = "rt")
  }
  if(file.exists(mcmcftfn)){
    ft         <- read.csv(mcmcftfn)
    tmp$ft     <- extract.area.sex.matrices(ft, prefix = "ft")
  }
  if(file.exists(mcmcmfn)){
    tmp$m         <- read.csv(mcmcmfn)
  }
  if(file.exists(mcmcutfn)){
    ut         <- read.csv(mcmcutfn)
    tmp$ut     <- extract.area.sex.matrices(ut, prefix = "ut")
  }
  if(file.exists(mcmcrdevfn)){
    rdev       <- read.csv(mcmcrdevfn)
    tmp$rdev   <- extract.group.matrices(rdev, prefix = "rdev")
  }
  if(file.exists(mcmcvbtfn)){
    vbt        <- read.csv(mcmcvbtfn)
    tmp$vbt    <- extract.area.sex.matrices(vbt, prefix = "vbt")
  }
  tmp$proj <- NULL
  if(file.exists(mcmcprojfn)){
    tmp$proj   <- read.csv(mcmcprojfn)
  }
  tmp
}

#' Extract the given data frame into a list of matrices by iscam 'group'
#'
#' @param data A data frame read in from one of the MCMC csv output files
#' @param prefix See details
#'
#' @details Extract the data frame given (data) by unflattening into a list of matrices
#'   by group. The group number is located in the names of the columns of the
#'   data frame in this format: "prefix[groupnum]_year" where [groupnum] is one
#'   or more digits representing the group number and prefix is the string
#'   given as an argument to the function.
#'
#' @return A list of matrices, one element per group
#' @export
#'
#' @examples
#' \donttest{}
extract.group.matrices <- function(data = NULL,
                                   prefix = NULL){

  curr.func.name <- get.curr.func.name()
  if(is.null(data) || is.null(prefix)){
    cat0(curr.func.name,
         "You must give two arguments (data & prefix). Returning NULL.")
    return(NULL)
  }
  tmp <- list()

  names <- names(data)
  pattern <- paste0(prefix, "([[:digit:]]+)_[[:digit:]]+")
  groups  <- sub(pattern, "\\1", names)
  unique.groups <- unique(as.numeric(groups))
  tmp <- vector("list", length = length(unique.groups))
  ## This code assumes that the groups are numbered sequentially from 1,2,3...N
  for(group in 1:length(unique.groups)){
    ## Get all the column names (group.names) for this group by making a specific
    ##  pattern for it
    group.pattern <- paste0(prefix, group, "_[[:digit:]]+")
    group.names   <- names[grep(group.pattern, names)]
    ## Remove the group number in the name, as it is not needed anymore
    pattern      <- paste0(prefix, "[[:digit:]]+_([[:digit:]]+)")
    group.names   <- sub(pattern, "\\1", group.names)

    # Now, the data must be extracted
    # Get the column numbers that this group are included in
    dat <- data[,grep(group.pattern, names)]
    colnames(dat) <- group.names
    tmp[[group]]  <- dat
  }
  tmp
}

#' Extract the given data frame into a list of matrices by iscam 'area'
#' and 'sex'
#'
#' @param data A data frame read in from one of the MCMC csv output files
#' @param prefix See details
#'
#' @details Extract the data frame given (data) by unflattening into a list of matrices
#'   by area-sex and gear. The area-sex number is located in the names of the
#'   columns of the data frame in this format:
#'   "prefix[areasexnum]_gear[gearnum]_year" where [areasexnum] and [gearnum]
#'   are one or more digits and prefix is the string given as an argument
#'   to the function.
#'
#' @return a list (area-sex) of lists (gears) of matrices, one element
#'  per group
#' @export
#'
#' @examples
#' \donttest{}
extract.area.sex.matrices <- function(data = NULL,
                                      prefix = NULL){

  curr.func.name <- get.curr.func.name()
  if(is.null(data) || is.null(prefix)){
    cat0(curr.func.name,
         "You must give two arguments (data & prefix). Returning NULL.")
    return(NULL)
  }

  names <- names(data)
  pattern <- paste0(prefix, "([[:digit:]]+)_gear[[:digit:]]+_[[:digit:]]+")
  groups  <- sub(pattern, "\\1", names)
  unique.groups <- unique(as.numeric(groups))
  tmp <- vector("list", length = length(unique.groups))
  ## This code assumes that the groups are numbered sequentially from 1,2,3...N
  for(group in 1:length(unique.groups)){
    ## Get all the column names (group.names) for this group by making a
    ##  specific pattern for it
    group.pattern <- paste0(prefix, group, "_gear[[:digit:]]+_[[:digit:]]+")
    group.names <- names[grep(group.pattern, names)]
    ## Remove the group number in the name, as it is not needed anymore
    pattern <- paste0(prefix, "[[:digit:]]+_gear([[:digit:]]+_[[:digit:]]+)")
    group.names <- sub(pattern, "\\1", group.names)
    ## At this point, group.names' elements look like this: 1_1963
    ## The first value is the gear, and the second, the year.
    ## Get the unique gears for this area-sex group
    pattern <- "([[:digit:]]+)_[[:digit:]]+"
    gears <- sub(pattern, "\\1", group.names)
    unique.gears <- unique(as.numeric(gears))
    tmp2 <- vector("list", length = length(unique.gears))
    for(gear in 1:length(unique.gears)){
      gear.pattern <- paste0(prefix, group,"_gear", gear, "_[[:digit:]]+")
      ## Now, the data must be extracted
      ## Get the column numbers that this group are included in
      dat <- data[,grep(gear.pattern, names)]
      ##colnames(dat) <- groupNames
      tmp2[[gear]] <- dat
    }
    tmp[[group]] <- tmp2
  }
  tmp
}

#' Apply burnin and thinning to the MCMC posteriors
#'
#' @param mcmc.dat A data frame of the MCMC posteriors
#' @param burnin The number of samples to burn away from the beginning of the MCMC
#' @param thin The thinning to apply to the MCMC posterior samples
#'
#' @return an mcmc.window object (CODA package)
#' @export
#'
#' @examples
#' \donttest{}
mcmc.thin <- function(mcmc.dat,
                      burnin,
                      thin){

  if(is.vector(mcmc.dat)){
    mcmc.obj <- mcmc(mcmc.dat)
    mcmc.window <- window(mcmc.obj,
                          start = burnin + 1,
                          thin = thin)
    return(mcmc.window)
  }
  nm <- names(mcmc.dat)
  mcmc.obj <- apply(mcmc.dat, 2, mcmc)
  mcmc.window <- NULL
  for(col in 1:ncol(mcmc.obj)){
    tmp <- window(mcmc.obj[,col],
                  start = burnin + 1,
                  thin = thin)
    mcmc.window <- cbind(mcmc.window, tmp)
  }
  mcmc.window <- as.data.frame(mcmc.window)
  names(mcmc.window) <- nm
  mcmc.window
}

#' Perform some quantile calculations on the MCMC posteriors
#'
#' @param model An iscam model object
#' @param burnin The number of samples to burn away from the beginning of the MCMC
#' @param thin The thinning to apply to the MCMC posterior samples
#' @param lower Lower quantile value to apply to MCMC samples
#' @param upper Upper quantile value to apply to MCMC samples
#' @param load.proj Load the projections from the MCMC and do the calculations
#'   to construct the decision tables
#' @param which.stock 1-5 for the five herring stocks: 1=HG, 2=PRD, 3=CC,
#'   4=SOG, 5=WCVI
#' @param which.model 1 = AM1 or 2 = AM2 for herring
#' @param fixed.cutoffs A vector of catch cutoffs to use in decision tables
#'
#' @return A list of each parameter for which quantiles were calculated
#' @export
#'
#' @examples
#' \donttest{}
calc.mcmc <- function(model,
                      burnin = 1000,
                      thin = 1,
                      lower = 0.025,
                      upper = 0.975,
                      load.proj = TRUE,
                      which.stock = NULL,
                      which.model = NULL,
                      fixed.cutoffs){

  ## burnin - the number of posteriors to remove from the data
  ## thin - the thinning to apply to the posterior samples
  ## lower - lower quantile for confidence interval calcs
  ## upper - upper quantile for confidence interval calcs
  ## load.proj - load the projections from the mcmc and do the calculations
  ##  to construct the decision tables
  ## which.stock and which.model are passed to calc.probabilities

  curr.func.name <- get.curr.func.name()
  if(is.null(mcmc)){
    cat0(curr.func.name,
         "The mcmc list was null. Check read.mcmc function. Returning NULL.")
    return(NULL)
  }

  probs <- c(lower, 0.5, upper)

  ## Parameters
  mc <- model$mcmc
  mpd <- model$mpd
  params.dat <- mc$params
  params.dat <- strip.areas.groups(params.dat)
  params.dat <- strip.static.params(model, params.dat)
  nm <- names(params.dat)

  p.dat <- params.dat[ , -which(nm %in% c("msy",
                                          "fmsy",
                                          "bmsy",
                                          "umsy",
                                          "ssb"))]
  p.dat <- fix.m(p.dat)
  p.dat <- mcmc.thin(p.dat, burnin, thin)
  ## Calculate sigma and tau and add to p.dat
  sigtau <- calc.sig.tau(p.dat$rho, p.dat$vartheta)
  p.dat$tau <- sigtau[[1]]
  p.dat$sigma <- sigtau[[2]]

  p.dat.log <- calc.logs(p.dat)
  p.quants <- apply(p.dat, 2, quantile, prob = probs)
  p.quants.log <- apply(p.dat.log, 2, quantile, prob = probs)

  ## Reference points
  r.dat <- NULL
  tryCatch({
    r.dat <- as.data.frame(params.dat[ , which(nm %in% c("sbo"))])
    r.dat <- mcmc.thin(r.dat, burnin, thin)
    colnames(r.dat) <- "sbo"
  }, warning = function(war){
  }, error = function(err){
    warning("MCMC calculations for SB0 failed.\n")
  })

  ## Spawning biomass
  sbt.dat <- mcmc.thin(mc$sbt[[1]], burnin, thin)
  sbt.quants <- apply(sbt.dat,
                      2,
                      quantile,
                      prob = probs)
  sbt.quants <- rbind(sbt.quants, mpd$sbt)
  rownames(sbt.quants)[4] <- "MPD"

  ## Depletion
  depl.dat <- NULL
  depl.quants <- NULL

  tryCatch({
    depl.dat <- apply(sbt.dat,
                      2,
                      function(x){x / r.dat$sbo})
    depl.quants <- apply(sbt.dat / r.dat$sbo,
                         2,
                         quantile,
                         prob = probs)
    depl.quants <- rbind(depl.quants, mpd$sbt / mpd$sbo)
    rownames(depl.quants)[4] <- "MPD"
  }, warning = function(war){
  }, error = function(err){
  })

  ## Natural mortality
  nat.mort.dat <- mcmc.thin(mc$m, burnin, thin)
  colnames(nat.mort.dat) <- gsub("m_age2_", "", colnames(nat.mort.dat))
  nat.mort.quants <- apply(nat.mort.dat,
                           2,
                           quantile,
                           prob = probs)

  ## Recruitment
  recr.dat <- mcmc.thin(mc$rt[[1]], burnin, thin)
  recr.mean <- apply(recr.dat,
                     2,
                     mean)
  recr.quants <- apply(recr.dat,
                       2,
                       quantile,
                       prob = probs)
  recr.quants <- rbind(recr.quants, mpd$rt)
  rownames(recr.quants)[4] <- "MPD"

  ## Recruitment deviations
  recr.devs.dat <- mcmc.thin(mc$rdev[[1]], burnin, thin)
  recr.devs.quants <- apply(recr.devs.dat,
                            2,
                            quantile,
                            prob = probs)

  ## Q for the survey indices
  q.dat <- p.dat[, grep("^q[[:digit:]]+$", colnames(p.dat))]
  num.indices <- ncol(q.dat)
  g.nms <- model$dat$gear.names
  colnames(q.dat) <- g.nms[(length(g.nms) - num.indices + 1):
                           length(g.nms)]
  q.quants <- apply(q.dat,
                    2,
                    quantile,
                    prob = probs)

  build.quant.list <- function(mc.dat, mpd.dat){
    ## Run quantiles on each dataframe in a list of dataframes and append
    ##  the MPD values as well. Returns a list of dataframes
    quants <- lapply(mc.dat,
                     function(x){
                       apply(x,
                             2,
                             quantile,
                             prob = probs,
                             na.rm = TRUE)})
    lapply(1:length(quants),
           function(x){
             quants[[x]] <- rbind(quants[[x]], mpd.dat[x,])
             rownames(quants[[x]])[4] <- "MPD"
             c.names <- colnames(quants[[x]])
             colnames(quants[[x]]) <-
               gsub("^.*_([[:digit:]]+$)", "\\1", c.names)
             quants[[x]]
           })
  }
  ## Vulnerable biomass by gear (list of data frames)
  vuln.dat <- lapply(mc$vbt[[1]], mcmc.thin, burnin, thin)
  ## Reshape the vulnerable biomass output from the MPD
  vbt <- as.data.frame(mpd$vbt)
  vbt <- split(vbt, vbt[,1])
  vbt <- lapply(1:length(vbt),
              function(x){
                vbt[[x]][,4]})
  vbt <- do.call(rbind, vbt)
  vuln.quants <- build.quant.list(vuln.dat, vbt)

  ## Fishing mortalities by gear (list of data frames)
  f.mort.dat <- lapply(mc$ft[[1]], mcmc.thin, burnin, thin)
  f.mort.quants <- build.quant.list(f.mort.dat, mpd$ft)

  u.mort.dat <- lapply(mc$ut[[1]], mcmc.thin, burnin, thin)
  u.mort.quants <- build.quant.list(u.mort.dat, mpd$ut)

  ## Add calculated reference points - these have already been thinned
  ##  and burned in
  sbt.yrs <- names(sbt.dat)
  sbt.init <- sbt.dat[,1]
  sbt.end <- sbt.dat[,ncol(sbt.dat)]
  sbt.end.1 <- sbt.dat[,ncol(sbt.dat) - 1]
  yr.sbt.init <- sbt.yrs[1]
  yr.sbt.end <- as.numeric(sbt.yrs[length(sbt.yrs)])
  yr.sbt.end.1 <- yr.sbt.end - 1

  f.yrs <- names(f.mort.dat[[1]])
  f.yrs <- gsub(".*_([[:digit:]]+)",
                 "\\1",
                 f.yrs)
  f.end <- f.mort.dat[[1]][,ncol(f.mort.dat[[1]])]
  yr.f.end <- f.yrs[length(f.yrs)]

  ## Proportion of age 3 and Proportion of age 4-10
  ## The values are the same for all TAC values so
  ##  just use TAC = 0
  proj <- mc$proj
  proj <- proj[proj$TAC == 0,]
  prop3.dat <- proj$PropAge3
  prop4.dat <- proj$PropAge4to10
  prop3.dat <- mcmc.thin(prop3.dat, burnin, thin)
  prop4.dat <- mcmc.thin(prop4.dat, burnin, thin)

  r.quants <- NULL
  tryCatch({
    r.dat <- cbind(r.dat,
                   0.3 * r.dat$sbo,
                   sbt.end.1,
                   sbt.end.1 / r.dat$sbo,
                   sbt.end,
                   prop3.dat,
                   prop4.dat)

    names(r.dat) <- c("sbo",
                      paste0("0.3sbo"),
                      paste0("sb", yr.sbt.end.1),
                      paste0("sb", yr.sbt.end.1, "/sbo"),
                      paste0("sb", yr.sbt.end),
                      "PropAge3",
                      "PropAge4to10")
  r.quants <- apply(r.dat, 2, quantile, prob = probs)
  }, warning = function(war){
  }, error = function(err){
    ## If this is the case, a message will have been printed in the previous
    ##  tryCatch above so none is needed here.
  })

  desc.col <- c("$SB_0$",
                "$0.3SB_0$",
                paste0("$SB_{", yr.sbt.end.1, "}$"),
                paste0("$SB_{", yr.sbt.end.1,
                       "}/",
                       "SB_0$"),
                paste0("$SB_{", yr.sbt.end, "}$"),
                "$\\text{Proportion aged 3}$",
                "$\\text{Proportion aged 4-10}$")

  r.quants <- t(r.quants)
  r.quants <- cbind.data.frame(desc.col, r.quants)
  col.names <- colnames(r.quants)
  col.names <- latex.bold(latex.perc(col.names))
  col.names[1] <- latex.bold("Reference point")
  colnames(r.quants) <- col.names

  proj.dat <- NULL
  if(load.proj){
    proj.dat <- calc.probabilities(model,
                                   burnin,
                                   thin,
                                   which.stock = which.stock,
                                   which.model = which.model,
                                   fixed.cutoffs = fixed.cutoffs)

    proj.quants <- apply(model$mcmc$proj[model$mcmc$proj$TAC == 0,],
                         2,
                         quantile,
                         prob = probs,
                         na.rm = TRUE)
  }

  sapply(c("p.dat",
           "p.quants",
           "p.dat.log",
           "p.quants.log",
           "r.dat",
           "r.quants",
           "sbt.dat",
           "sbt.quants",
           "depl.dat",
           "depl.quants",
           "nat.mort.dat",
           "nat.mort.quants",
           "recr.dat",
           "recr.quants",
           "recr.devs.dat",
           "recr.devs.quants",
           "q.dat",
           "q.quants",
           "vuln.dat",
           "vuln.quants",
           "f.mort.dat",
           "f.mort.quants",
           "u.mort.dat",
           "u.mort.quants",
           "proj.dat",
           "proj.quants"),
           function(x){get(x)})
}

#' Calculate the estimated numbers-at-age for an iscam MCMC model
#'
#' @param model An iscam model object
#'
#' @return A list of data frames, one for each gear
#' @export
#'
#' @examples
#' \donttest{}
calc.ahat <- function(model){
  if(class(model) == model.lst.class){
    model <- model[[1]]
    if(class(model) != model.class){
      stop("The structure of the model list is incorrect.")
    }
  }

  mpd <- model$mpd
  ahat <- mpd$A_hat
  sage <- mpd$n_A_sage[1]
  nage <- mpd$n_A_nage[1]
  num.ages <- nage - sage + 1
  nagv <- model$dat$num.age.gears.vec
  age.comps <- model$dat$age.comps

  ## Break up the ahat vector into its correct dimensions
  gears <- list()
  ind <- 1
  for(i in 1:length(nagv)){
    ext <- ahat[ind:(ind + nagv[i] * num.ages - 1)]
    ind <- ind + nagv[i] * num.ages
    ind.byage <- 1
    gears[[i]] <- list()
    for(j in 1:(length(ext) / num.ages)){
      ext.byage <- ext[ind.byage:(ind.byage + num.ages - 1)]
      ind.byage <- ind.byage + num.ages
      gears[[i]][[j]] <- ext.byage
    }
    gears[[i]] <- do.call(rbind, gears[[i]])
  }

  ## Now gears is a list of dataframes, 1 for each gear
  ## Add years to rows and ages to columns
  gears <- lapply(1:length(nagv),
                  function(x){
                    gears[[x]] <- as.data.frame(gears[[x]])
                    rownames(gears[[x]]) <- as.data.frame(age.comps[[x]])$year
                    colnames(gears[[x]]) <- sage:nage
                    gears[[x]]
                  })
  gears
}

#' Extract and calculate probabilities from the iscam projection model
#'
#' @param model An iscam model object
#' @param burnin The number of samples to burn away from the beginning of the MCMC
#' @param thin The thinning to apply to the MCMC posterior samples
#' @param which.stock 1-5 for the five herring stocks: 1=HG, 2=PRD, 3=CC,
#'   4=SOG, 5=WCVI
#' @param which.model 1 = AM1 or 2 = AM2 for herring
#' @param fixed.cutoffs A vector of catch cutoffs to use in decision tables
#'
#' @details Extract and calculate probabilities from the projection model.
#'   Used for decision tables in the document (see make.decision.table())
#'   in tables-decisions.r
#'
#' @return A data frame which has its names formatted for latex
#' @export
#'
#' @examples
#' \donttest{}
calc.probabilities <- function(model,
                               burnin,
                               thin,
                               which.stock = NULL,
                               which.model = NULL,
                               fixed.cutoffs){

  if(is.null(which.stock)){
    warning("which.stock must be between 1 and 5.")
    return(NULL)
  }
  if(which.stock < 1 | which.stock > 5){
    warning("which.stock must be between 1 and 5.")
    return(NULL)
  }
  if(is.null(which.model)){
    warning("which.model must be 1 or 2, it is NULL.")
    return(NULL)
  }
  if(which.model != 1 & which.model != 2){
    warning("which.model must be 1 or 2, not ", which.model, ".")
    return(NULL)
  }

  mc <- model$mcmc
  proj <- mc$proj
  tac <- sort(unique(proj$TAC))
  p <- model$proj$ctl.options
  s.yr <- p[rownames(p) == "syrmeanm", 1]
  e.yr <- p[rownames(p) == "nyrmeanm", 1] + 2
  e.yr.1 <- e.yr - 1
  e.yr.2 <- e.yr - 2

  fc <- fixed.cutoffs
  proj.dat <- data.frame()
  for(t in 1:length(tac)){
    d <- proj[proj$TAC == tac[t],]
    d <- mcmc.thin(d, burnin, thin)
    n.row <- nrow(d)
    k <- c(tac[t] * 1000,
           length(which(d[,paste0("B", e.yr.1)] < d$X03B0)) / n.row,
           median(d[,paste0("B", e.yr.1)] / d$X03B0))
           #length(which(d[,paste0("B", e.yr.1)] < d$X09B0)) / n.row,
           #median(d[,paste0("B", e.yr.1)] / d$X09B0))

    if(t == 1){
      col.names <- c(latex.mlc(c(e.yr.1,
                                 "TAC (t)")),
                     latex.mlc(c(paste0("P(SB_{",
                                        e.yr.1,
                                        "}<"),
                                 "0.3SB_0)"),
                               math.bold = TRUE),
                     latex.mlc(c(paste0("Med(SB_{",
                                        e.yr.1,
                                        "}/"),
                                 "0.3SB_0)"),
                               math.bold = TRUE))
    }
    if(which.model == 2){
      k <- c(k,
             length(which(d[,paste0("B", e.yr.1)] < fc[which.stock])) / n.row,
             median(d[,paste0("B", e.yr.1)] / fc[which.stock]))
      if(t == 1){
        col.names <- c(col.names,
                       latex.mlc(c(paste0("P(SB_{",
                                          e.yr.1,
                                          "} <"),
                                   paste0(f(fc[which.stock] * 1000),
                                          "~t)")),
                                 math.bold = TRUE),
                       latex.mlc(c(paste0("Med(SB_{",
                                          e.yr.1,
                                          "} /"),
                                   paste0(f(fc[which.stock] * 1000),
                                          "~t)")),
                                 math.bold = TRUE))
      }
    }

    k <- c(k,
           length(which(d$UT > 0.2)) / n.row,
           length(which(d$UT > 0.1)) / n.row,
           # length(which(d$UT > 0.05)) / n.row,
          # length(which(d$UT > 0.03)) / n.row,
           # length(which(d$UT > 0.07)) / n.row,
          #length(which(d$UT > 0.08)) / n.row,
         # length(which(d$UT > 0.09)) / n.row,
           median(d$UT))
    if(t == 1){
      col.names <- c(col.names,
                     latex.mlc(c(paste0("P(U_{",
                                        e.yr.1,
                                        "}>"),
                                 "20\\%)"),
                               math.bold = TRUE),
                     latex.mlc(c(paste0("P(U_{",
                                       e.yr.1,
                                        "}>"),
                                 "10\\%)"),
                               math.bold = TRUE),
                    #  latex.mlc(c(paste0("P(U_{",
                      #                   e.yr.1,
                       #                  "}>"),
                         #         "5\\%)"),
                           #   math.bold = TRUE),
                   # latex.mlc(c(paste0("P(U_{",
                    #                    e.yr.1,
                      #                  "}>"),
                         #        "3\\%)"),
                             #  math.bold = TRUE),
                    #latex.mlc(c(paste0("P(U_{",
                      #                e.yr.1,
                       #              "}>"),
                          #   "7\\%)"),
                         # math.bold = TRUE),
                   # latex.mlc(c(paste0("P(U_{",
                      #                 e.yr.1,
                       #                "}>"),
                        #        "8\\%)"),
                         #     math.bold = TRUE),
                    #latex.mlc(c(paste0("P(U_{",
                               #        e.yr.1,
                             #          "}>"),
                              #  "9\\%)"),
                            #  math.bold = TRUE),
                     latex.math.bold(paste0("Med(U_{",
                                            e.yr.1,
                                            "})")))
    }
    proj.dat <- rbind(proj.dat, k)
  }
  colnames(proj.dat) <- col.names

  proj.dat
}

#' Fetch a data frame of the estimated MCMC parameters only
#'
#' @param mc A data frame of posteriors as seen in the MCMC output csv files
#'
#' @details If all values in a given column are different, it is assumed that
#'   the parameter was estimated.
#'
#' @return A data frame of estimated parameters
#' @export
#'
#' @examples
#' \donttest{}
get.estimated.params <- function(mc){

  posts <- apply(mc,
                 2,
                 function(x){
                   if(length(unique(x)) > 1)
                     return(x)
                 })
  ## Remove NULL list elements (fixed parameters)
  posts.lst <- posts[!sapply(posts, is.null)]
  do.call(cbind, posts.lst)
}

#' Calculate logs for several parameters using MCMC
#'
#' @param mc A data frame of posteriors as seen in the MCMC output csv files
#' @param log.params A vector of regular expressions to determine which
#'   parameters to apply the log function to
#'
#' @details The column names will be prepended with log_ for the parameters which
#'   had the log function applied
#'
#' @return a data frame (mc with the log columns appended)
#' @export
#'
#' @examples
#' \donttest{}
calc.logs <- function(mc,
                      log.params = c("^ro$",
                                     "^m$",
                                     "^m1$",
                                     "^m2$",
                                     "^rbar$",
                                     "^rinit$",
                                     "^q[1-9]+$")){
  ## Returns a data frame with the logs calculated for various parameters.
  ##
  ##
  ## log.params - the names of the parameters to perform log function.
  ##  Use regular expressions for things like q to represent q1, q2, ... q10

  nm <- colnames(mc)
  grp <- lapply(log.params,
                function(x){
                  grep(x, nm)})
  inds.lst <- grp[sapply(grp,
                         function(x){
                           length(x) > 0})]
  inds <- unique(do.call(c, inds.lst))
  colnames(mc)[inds] <- paste0("log_", colnames(mc)[inds])
  mc[,inds] <- apply(mc[,inds],
                     2,
                     log)
  mc
}

#' Change the column name 'm1' to 'm' unless both 'm1' and 'm2' exist
#'
#' @param mc A data frame of posteriors as seen in the MCMC output csv files
#'
#' @details Fo column names of data frame mc:
#'   If m1 and m2 both exist, no change
#'   If only m1 exists, change the name to m
#'
#' @return A data frame the same as mc but with modifications (see details)
#' @export
#'
#' @examples
#' \donttest{}
fix.m <- function(mc){

  grp <- grep("m[12]", colnames(mc))
  if(length(grp) == 1){
    colnames(mc)[grp] <- "m"
  }
  mc
}

#' Calculate logs for several parameters using MPD
#'
#' @param mpd A list of MPD outputs
#' @param log.params The names of the parameters to log
#'
#' @details The new list elements will have the same names but have 'log_' appended
#'
#' @return  a list (mpd with some new elements appended, the log-applied elements)
#' @export
#'
#' @examples
#' \donttest{}
calc.mpd.logs <- function(mpd,
                          log.params = c("^ro$",
                                         "^m$",
                                         "^m1$",
                                         "^m2$",
                                         "^rbar$",
                                         "^rinit$",
                                         "^q$")){
  ## Returns a the mpd data list with some parameters logged
  ##  and added to the list
  ##
  ## log.params - the names of the parameters to log. The new values
  ##  will have the same names but prepended with log_

  grp <- lapply(log.params,
                function(x){
                  grep(x, names(mpd))})
  inds.lst <- grp[sapply(grp,
                         function(x){
                           length(x) > 0})]
  inds <- unique(do.call(c, inds.lst))
  log.names <- paste0("log_", names(mpd)[inds])
  vals <- lapply(mpd[inds], log)
  names(vals) <- log.names
  c(mpd, vals)
}
