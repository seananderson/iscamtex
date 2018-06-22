#' Produces an xtable with parameters and priors
#'
#' @param model An iscam model object
#' @param xcaption Caption to appear in the calling document
#' @param xlabel The label used to reference the table in latex
#' @param font.size Size of the font for the table
#' @param space.size Size of the vertical spaces for the table
#' @param placement Latex code for placement of the table in document
#'
#' @return An xtable
#' @export
make.parameters.table <- function(model,
                                  xcaption = "default",
                                  xlabel   = "default",
                                  font.size = 9,
                                  space.size = 10,
                                  placement = "H"){

  get.bounds <- function(ind){
    ## Return the bounds string for row ind of the parameters
    paste0("[",
           params$lb[ind],
           ", ",
           params$ub[ind],
           "]")
  }

  get.vals <- function(ind){
    ## Return a 3-element vector for the number estimated, bounds, and prior
    ##  dist mean and SD or "Uniform" for the row ind of the parameters
    ##
    ## vec is a vector of 4 values, the phase, prior type (0=uniform, 1=normal,
    ##  2=lognormal, 3=beta, 4=gamma), the first and second parameter
    ##  of the prior.
    vec <- as.numeric(params[ind, 4:7])
    if(vec[1] < 1){
      return(c(0,
               "Fixed",
               paste0("$", f(params[ind, 1], 3), "$")))
    }else if(vec[2] == 0){
      return(c(1,
               get.bounds(ind),
               "Uniform"))
    }else if(vec[2] == 1){
      return(c(1,
               get.bounds(ind),
               paste0("Normal($\\ln(",
                      vec[4],
                      "), ",
                      vec[4],
                      "$)")))
    }else if(vec[2] == 2){
      return(c(1,
               get.bounds(ind),
               paste0("Lognormal($",
                      vec[3],
                      ", ",
                      vec[4],
                      "$)")))
    }else if(vec[2] == 3){
      return(c(1,
               get.bounds(ind),
               paste0("Beta($\\alpha = ",
                      vec[3],
                      ", \\beta = ",
                      vec[4],
                      "$)")))
    }else if(vec[2] == 4){
      return(c(1,
               get.bounds(ind),
               paste0("Gamma($k = ",
                      vec[3],
                      "), \\theta = ",
                      vec[4],
                      "$)")))
    }
    invisible()
  }

  if(class(model) == model.lst.class){
    model <- model[[1]]
    if(class(model) != model.class){
      stop("The structure of the model list is incorrect.")
    }
  }

  ctl <- model$ctl
  params <- as.data.frame(ctl$params)

  tab <- data.frame(param = character(),
                    num.est = character(),
                    bounds = character(),
                    prior = character(),
                    stringsAsFactors = FALSE)

  param.text <- c("Log recruitment ($\\ln(R_0)$)",
                  "Steepness ($h$)",
                  "Log natural mortality ($\\ln(M)$)",
                  "Log mean recruitment ($\\ln(\\overline{R})$)",
                  "Log initial recruitment ($\\ln(\\overline{R}_{init})$)",
                  "Variance ratio, rho ($\\rho$)",
                  "Inverse total variance, kappa ($\\kappa$)")

  param.vals <- do.call(rbind, lapply(1:nrow(params), get.vals))

  ## Selectivity parameters
  ## sel data frame has one column for each gear and 10 rows:
  ## 1  - selectivity type:
  ##       1) logistic selectivity parameters
  ##       2) selectivity coefficients
  ##       3) a constant cubic spline with age-nodes
  ##       4) a time varying cubic spline with age-nodes
  ##       5) a time varying bicubic spline with age & year nodes
  ##       6) fixed logistic (set isel_type=6, and estimation phase to -1)
  ##       7) logistic function of body weight.
  ##       8) logistic with weight deviations (3 parameters)
  ##       11) logistic selectivity with 2 parameters based on mean length
  ##       12) length-based selectivity coefficients with spline interpolation
  ## 2  - Age/length at 50% selectivity (logistic)
  ## 3  - STD at 50% selectivity (logistic)
  ## 4  - No. of age nodes for each gear (0=ignore)
  ## 5  - No. of year nodes for 2d spline(0=ignore)
  ## 6  - Phase of estimation (-1 for fixed) If neg number, it reflects a
  ##       mirroring of another gear's selectivity.
  ## 7  - Penalty wt for 2nd differences w=1/(2*sig^2)
  ## 8  - Penalty wt for dome-shaped w=1/(2*sig^2)
  ## 9  - Penalty wt for time-varying selectivity
  ## 10 - n_sel_blocks (number of selex blocks)

  sel <- ctl$sel
  dat <- model$dat
  indices <- dat$indices
  indices.df <- as.data.frame(do.call(rbind, indices))
  surv.gear.nums <- unique(indices.df$gear)
  surv.sel <- as.data.frame(sel[,surv.gear.nums])
  fish.sel <- as.data.frame(sel[,-surv.gear.nums])
  ## Get number estimated by looking at the phase row in the sel data frame
  surv.est <- surv.sel[6,]
  surv.est <- sum(surv.est > 0)
  fish.est <- fish.sel[6,]
  fish.est <- sum(fish.est > 0)
  ## Hardwired bounds of 0,1 for age-at-50% and 0,Inf for age-at-50% SD
  param.vals <- rbind(param.vals,
                      c(fish.est,
                        "[0, 1]",
                        "None"),
                      c(fish.est,
                        "[0, Inf)",
                        "None"))

  param.text <- c(param.text,
                "Fishery age at 50\\% logistic selectivity ($\\hat{a}_k$)",
                "Fishery SD of logistic selectivity ($\\hat{\\gamma}_k$)")

  ## Catchability  parameters
  ## q is a data frame with 1 column for each survey and 3 rows:
  ## 1 - prior type:
  ##      0) Uniformative prior
  ##      1) normal prior density for log(q)
  ##      2) random walk in q
  ## 2 - prior log(mean)
  ## 3 - prior SD

  ## q <- ctl$surv.q
  ## num.inds <- ctl$num.indices
  ## param.vals <- rbind(param.vals,
  ##                     c(num.inds,
  ##                       "None",
  ##                       "Normal($0.5, 1$)"))

  ## param.text <- c(param.text,
  ##                 "Survey catchability ($q_k$)")

  ## Fishing mortality and recruitment parameters
  ##
  par <- model$par
  num.f.params <- length(par$log_ft_pars)
  num.rec.params <- length(par$log_rec_devs)
  num.init.rec.params <- length(par$init_log_rec_devs)
  param.vals <- rbind(param.vals,
                      c(num.rec.params,
                        "None",
                        "Normal($0, \\tau$)"),
                      c(num.init.rec.params,
                        "None",
                        "Normal($0, \\tau$)"))

  param.text <- c(param.text,
                  "Log recruitment deviations ($\\omega_t$)",
                  "Initial log recruitment deviations ($\\omega_{init,t}$)")

  tab <- cbind(param.text, param.vals)
  colnames(tab) <- c(latex.bold("Parameter"),
                     latex.mlc(c("Number",
                                 "estimated")),
                     latex.mlc(c("Bounds",
                                 "[low, high")),
                     latex.mlc(c("Prior (mean, SD)",
                                 "(single value = fixed)")))

  size.string <- latex.size.str(font.size, space.size)
  print(xtable(tab,
               caption = xcaption,
               label = xlabel,
               align = get.align(ncol(tab))),
        caption.placement = "top",
        include.rownames = FALSE,
        sanitize.text.function = function(x){x},
        size = size.string,
        table.placement = placement)
}

#' Produce an xtable containing the catchability parameter estimates
#'
#' @param am1.lst A list containing the output from Herring model AM1
#' @param am2.lst A list containing the output from Herring model AM2
#' @param digits Number of digits after decimal point
#' @param xcaption Caption to appear in the calling document
#' @param xlabel The label used to reference the table in latex
#' @param font.size Size of the font for the table
#' @param space.size Size of the vertical spaces for the table
#' @param placement Latex code for placement of the table in document
#'
#' @details am1.lst and am2.lst are lists of the models AM1 and AM2
#'   to write q values for. If am2 is NULL, write only AM1 model output.
#'   Catchability  parameters:
#'   q is a data frame with 1 column for each survey and 3 rows:
#'   1 - prior type:
#'        0) Uniformative prior
#'        1) normal prior density for log(q)
#'        2) random walk in q
#'   2 - prior log(mean)
#'   3 - prior SD
#'
#' @return An xtable
#' @export
make.catchability.parameters.table <- function(am1.lst,
                                               am2.lst = NULL,
                                               digits = 3,
                                               xcaption = "default",
                                               xlabel   = "default",
                                               font.size = 9,
                                               space.size = 10,
                                               placement = "H"){

  lst <- list()
  st <- c("HG", "PRD", "CC", "SOG", "WCVI")
  for(i in 1:5){
    mdl <- am1.lst[[i]][[1]]
    mc <- mdl$mcmccalcs
    p.quants <- as.data.frame(mc$p.quants)
    ## AM1
    ctl.am1 <- mdl$ctl
    ## AM1 q estimates
    q1.est.am1 <- f(p.quants$q1[2], digits)
    q2.est.am1 <- f(p.quants$q2[2], digits)
    ## AM1 q priors
    q.am1 <- as.data.frame(t(ctl.am1$surv.q))
    q.am1$priormeanlog <- exp(q.am1$priormeanlog)
    p.vals.am1.s <- paste0("Normal($",
                           f(q.am1$priormeanlog[1], 3),
                           ", ",
                           f(q.am1$priorsd[1], 3),
                           "$)")
    p.vals.am1.d <- paste0("Normal($",
                           f(q.am1$priormeanlog[2], 3),
                           ", ",
                           f(q.am1$priorsd[2], 3),
                           "$)")
    sb.end.am1 <- f(mc$r.quants[3, 3], digits)
    sbo.am1 <- f(p.quants$sbo[2], digits)
    depl.am1 <- f(mc$depl.quants[,ncol(mc$depl.quants) - 1][2], digits)

    if(!is.null(am2.lst)){
      ## AM2
      mdl <- am2.lst[[i]][[1]]
      mc <- mdl$mcmccalcs
      p.quants <- as.data.frame(mc$p.quants)
      ctl.am2 <- mdl$ctl
      ## AM2 q estimates
      q1.est.am2 <- f(p.quants$q1[2], digits)
      q2.est.am2 <- f(p.quants$q2[2], digits)
      ## AM2 q priors
      q.am2 <- as.data.frame(t(ctl.am2$surv.q))
      q.am2$priormeanlog <- exp(q.am2$priormeanlog)
      p.vals.am2.s <- paste0("Normal($",
                             f(q.am2$priormeanlog[1], 3),
                             ", ",
                             f(q.am2$priorsd[1], 3),
                             "$)")
      p.vals.am2.d <- paste0("Normal($",
                             f(q.am2$priormeanlog[2], 3),
                             ", ",
                             f(q.am2$priorsd[2], 3),
                             "$)")
      sb.end.am2 <- f(mc$r.quants[3, 3], digits)
      sb.end.yr <- gsub("sb", "", rownames(mc$r.quants)[3])
      sbo.am2 <- f(p.quants$sbo[2], digits)
      depl.am2 <- f(mc$depl.quants[,ncol(mc$depl.quants) - 1][2], digits)
    }

    lst[[i]] <- rbind(c(st[i],
                      "AM1",
                      "Surface",
                      "None",
                      q1.est.am1,
                      q2.est.am1,
                      p.vals.am1.s,
                      sb.end.am1,
                      sbo.am1,
                      depl.am1),
                    c(st[i],
                      "AM1",
                      "Dive",
                      "None",
                      q1.est.am1,
                      q2.est.am1,
                      p.vals.am1.d,
                      sb.end.am1,
                      sbo.am1,
                      depl.am1))
    if(!is.null(am2.lst)){
      lst[[i]] <- rbind(lst[[i]],
                        c(st[i],
                          "AM2",
                          "Surface",
                          "None",
                          q1.est.am2,
                          q2.est.am2,
                          p.vals.am2.s,
                          sb.end.am2,
                          sbo.am2,
                          depl.am2),
                        c(st[i],
                          "AM2",
                          "Dive",
                          "None",
                          q1.est.am2,
                          q2.est.am2,
                          p.vals.am2.d,
                          sb.end.am2,
                          sbo.am2,
                          depl.am2))
    }
  }
  tab <- do.call(rbind, lst)
  colnames(tab) <- c(latex.bold("SAR"),
                     latex.bold("Model"),
                     latex.bold("Survey"),
                     latex.bold("Bounds"),
                     latex.mlc(c("Estimated",
                                 "q1")),
                     latex.mlc(c("Estimated",
                                 "q2")),
                     latex.bold("Prior (mean, SD)"),
                     latex.bold(paste0("SB\\subscr{",
                                       sb.end.yr,
                                       "}")),
                     latex.bold("SB\\subscr{0}"),
                     latex.mlc(c("Depletion",
                                 paste0("SB\\subscr{",
                                        sb.end.yr,
                                        "}/SB\\subscr{0}"))))

  addtorow <- list()
  addtorow$pos <- list(4, 8, 12, 16)
  addtorow$command <- c("\\midrule ",
                        "\\midrule ",
                        "\\midrule ",
                        "\\midrule ")
  size.string <- latex.size.str(font.size, space.size)
  print(xtable(tab,
               caption = xcaption,
               label = xlabel,
               align = get.align(ncol(tab))),
        caption.placement = "top",
        include.rownames = FALSE,
        sanitize.text.function = function(x){x},
        size = size.string,
        table.placement = placement,
        add.to.row = addtorow,
        booktabs = TRUE)
}

#' Produces an xtable with parameter estimates and priors
#'
#' @param model An iscam model object
#' @param digits Number of decimal points on percent columns
#' @param xcaption Caption to appear in the calling document
#' @param xlabel The label used to reference the table in latex
#' @param font.size Size of the font for the table
#' @param space.size Size of the vertical spaces for the table
#' @param placement Latex code for placement of the table in document
#'
#' @return An xtable
#' @export
make.parameters.est.table <- function(model,
                                      digits = 3,
                                      xcaption = "default",
                                      xlabel   = "default",
                                      font.size = 9,
                                      space.size = 10,
                                      placement = "H"){

  if(class(model) == model.lst.class){
    model <- model[[1]]
    if(class(model) != model.class){
      stop("The structure of the model list is incorrect.")
    }
  }

  mc <- model$mcmccalcs
  p.quants <- mc$p.quants
  mcmc.names <- colnames(p.quants)

  ## Append MPD values
  mpd <- model$mpd
  mpd.names <- names(mpd)

  ## Remove selectivity parameters
  mcmc.names <- mcmc.names[-grep("sel.*", mcmc.names)]
  p.quants <- p.quants[,-grep("sel.*", colnames(p.quants))]
  p.quants <- p.quants[,-grep("bo", colnames(p.quants))]

  mpd.param.vals <- NULL
  for(pname in mcmc.names){
    ## This is hack code because iscam is not outputting the same parameter
    ##  names for MPD and MCMC runs
    if(pname == "h"){
      pname <- "steepness"
    }
    if(pname == "m1"){
      pname <- "m"
    }
    if(pname == "bo"){
      pname <- "sbo"
    }
    match.q <- grep("q[[:digit:]]+",
                    pname)
    q.pars <- mpd$q
    if(length(match.q) > 0){
      ## The parameter starts with "q"
      split.val <- strsplit(pname,
                            "[^[:digit:]]")[[1]]
      q.num <- as.numeric(split.val[length(split.val)])
      this.par <- q.pars[q.num]
    }else if(pname != "sbo"){
      ## Match the mcmc name with the mpd name. Q and selectivity are special
      ##  cases, they must be extracted from vectors and matrices respectively
      this.par <- mpd[match(pname, mpd.names)]
    }
    mpd.param.vals <- c(mpd.param.vals, this.par)
  }

  names(mpd.param.vals) <- mcmc.names
  mpd.param.vals$bo <- NULL
  tab <- rbind(p.quants, as.numeric(mpd.param.vals))
  row.n <- rownames(tab)
  row.n[length(row.n)] <- "MPD"
  rownames(tab) <- row.n
  tab <- f(t(tab), digits)

  ## The next set of names only pertains to the ARF assessment, the q's
  ##  and sel's are modified to line up with each other.
  new.col <- c("$R_0$",
               "$Steepness (h)$",
               "$M$",
               "$\\overline{R}$",
               "$\\overline{R}_{init}$",
               "$\\rho$",
               "$\\vartheta$",
               "$q_1$",
               "$q_2$",
               "$\\tau$",
               "$\\sigma$")
  col.names <- colnames(tab)
  col.names <- latex.bold(latex.perc(col.names))
  col.names <- c(latex.bold("Parameter"), col.names)
  tab <- cbind(new.col, tab)
  colnames(tab) <- col.names

  size.string <- latex.size.str(font.size, space.size)
  print(xtable(tab,
               caption = xcaption,
               label = xlabel,
               align = get.align(ncol(tab))),
        caption.placement = "top",
        include.rownames = FALSE,
        sanitize.text.function = function(x){x},
        size = size.string,
        table.placement = placement,
        booktabs = TRUE)
}

#' Produce an xtable for the reference points
#'
#' @param model.am2 The AM1 iscam model object (Herring)
#' @param model.am1 The AM2 iscam model object (Herring)
#' @param digits Number of decimal places for the values
#' @param xcaption Caption to appear in the calling document
#' @param xlabel The label used to reference the table in latex
#' @param font.size Size of the font for the table
#' @param space.size Size of the vertical spaces for the table
#' @param placement Latex code for placement of the table in document
#'
#' @return An xtable
#' @export
make.ref.points.table <- function(model.am2,
                                  model.am1,
                                  digits = 3,
                                  xcaption = "default",
                                  xlabel   = "default",
                                  font.size = 9,
                                  space.size = 10,
                                  placement = "H"){
  ## Returns an xtable in the proper format for reference points
  ##
  ## digits - number of decimal places for the values
  ## xcaption - caption to appear in the calling document
  ## xlabel - the label used to reference the table in latex
  ## font.size - size of the font for the table
  ## space.size - size of the vertical spaces for the table
  ## placement - latex code for placement of the table in document

  if(class(model.am2) == model.lst.class){
    model <- model[[1]]
    if(class(model) != model.class){
      stop("The structure of the model.am2 list is incorrect.")
    }
  }

  if(class(model.am1) == model.lst.class){
    model <- model[[1]]
    if(class(model) != model.class){
      stop("The structure of the model.am1 list is incorrect.")
    }
  }

  proj <- model.am2$mcmccalcs$proj.quants
  tab.am2 <- model.am2$mcmccalcs$r.quants
  ## Replace SB projected year with correct values
  tab.am2[5,2:4] <- proj[,2]
  row.names <- tab.am2[,1]
  col.names.am2 <- colnames(tab.am2)
  ## Remove latex rownames
  tab.am2 <- as.matrix(tab.am2[,-1])
  tab.am2 <- apply(tab.am2, c(1, 2) , as.numeric)
  ## Format the non-proportion data to digits
  tab.am2.non <- tab.am2[-c(6,7), ]
  tab.am2.non <- f(tab.am2.non, digits)
  ## Format the proportion-at-age to two digits only
  tab.am2.prop <- tab.am2[c(6,7), ]
  tab.am2.prop <- f(tab.am2.prop, 2)
  tab.am2 <- rbind(tab.am2.non, tab.am2.prop)

  proj <- model.am1$mcmccalcs$proj.quants
  tab.am1 <- model.am1$mcmccalcs$r.quants
  ## Replace SB projected year with correct values
  tab.am1[5,2:4] <- proj[,2]
  row.names <- tab.am1[,1]
  col.names.am1 <- colnames(tab.am1)
  ## Remove latex rownames
  tab.am1 <- as.matrix(tab.am1[,-1])
  tab.am1 <- apply(tab.am1, c(1, 2) , as.numeric)
  ## Format the non-proportion data to digits
  tab.am1.non <- tab.am1[-c(6,7), ]
  tab.am1.non <- f(tab.am1.non, digits)
  ## Format the proportion-at-age to two digits only
  tab.am1.prop <- tab.am1[c(6,7), ]
  tab.am1.prop <- f(tab.am1.prop, 2)
  tab.am1 <- rbind(tab.am1.non, tab.am1.prop)

  tab <- cbind(row.names,
               as.data.frame(tab.am2),
               as.data.frame(tab.am1))

  colnames(tab) <- c(col.names.am2, col.names.am1[-1])

  addtorow <- list()
  addtorow$pos <- list(-1, nrow(tab))
  addtorow$command <- c(paste0("\\toprule",
                               latex.amp(),
                               latex.mcol(3,
                                          "c",
                                          latex.bold("AM2")),
                               latex.amp(),
                               latex.mcol(3,
                                          "c",
                                          latex.bold("AM1")),
                               latex.nline,
                               latex.cmidr("2-4", "lr"),
                               " ",
                               latex.cmidr("5-7", "lr")),
                        "\\bottomrule")
  size.string <- latex.size.str(font.size, space.size)
  print(xtable(tab,
               caption = xcaption,
               label = xlabel,
               align = get.align(ncol(tab))),
        caption.placement = "top",
        include.rownames = FALSE,
        sanitize.text.function = function(x){x},
        size = size.string,
        table.placement = placement,
        hline.after = c(0),
        add.to.row = addtorow,
        booktabs = TRUE)
}

#' Produce an xtable containing the values biomass, recruitment and others
#'
#' @param model An iscam model object
#' @param type 1 = biomass, 2 = recruitment, 3 = F, 4 = U, 5 = depletion
#' @param syr Start year
#' @param digits Number of decimal places for the values in the table
#' @param xcaption Caption to appear in the calling document
#' @param xlabel The label used to reference the table in latex
#' @param font.size Size of the font for the table
#' @param space.size Size of the vertical spaces for the table
#' @param placement Latex code for placement of the table in document
#'
#' @return An xtable
#' @export
make.value.table <- function(model,
                             type,
                             syr,
                             digits = 3,
                             xcaption = "default",
                             xlabel   = "default",
                             font.size = 9,
                             space.size = 10,
                             placement = "H",
                             tabular.environment = "tabular"){

  if(class(model) == model.lst.class){
    model <- model[[1]]
    if(class(model) != model.class){
      stop("The structure of the model list is incorrect.")
    }
  }

  if(type == 1){
    out.dat <- model$mcmccalcs$sbt.quants
  }else if(type == 2){
    out.dat <- model$mcmccalcs$recr.quants
  }else if(type == 3){
    out.dat <- model$mcmccalcs$f.mort.quants[[1]]
  }else if(type == 4){
    out.dat <- model$mcmccalcs$u.mort.quants[[1]]
  }else if(type == 5){
    out.dat <- model$mcmccalcs$depl.quants
  }else{
    stop("Type ", type, " not implemented.")
  }

  tab <- f(t(out.dat), digits)
  tab <- cbind(rownames(tab), tab)
  tab <- tab[tab[,1] >= syr,]
  col.names <- colnames(tab)
  col.names[1] <- "Year"
  col.names <- latex.bold(latex.perc(col.names))
  colnames(tab) <- col.names

  size.string <- latex.size.str(font.size, space.size)
  print(xtable(tab,
               caption = xcaption,
               label = xlabel,
               align = get.align(ncol(tab))),
        caption.placement = "top",
        include.rownames = FALSE,
        sanitize.text.function = function(x){x},
        size = size.string,
        table.placement = placement,
        booktabs = TRUE,
        tabular.environment = tabular.environment)
}

#' Produces an xtable containing spawning biomass and relative biomass
#'
#' @param model An iscam model object
#' @param syr Start year
#' @param digits Number of decimal places for the values in the table
#' @param xcaption Caption to appear in the calling document
#' @param xlabel The label used to reference the table in latex
#' @param font.size Size of the font for the table
#' @param space.size Size of the vertical spaces for the table
#' @param placement Latex code for placement of the table in document
#' @param tabular.environment 'tabular' or 'tabularx'
#'
#' @details Based on make.value.table, but wider with both and extra headers
#'
#' @return An xtable
#' @export
make.biomass.depletion.table <- function(model,
                                         syr,
                                         digits = 3,
                                         xcaption = "default",
                                         xlabel   = "default",
                                         font.size = 9,
                                         space.size = 10,
                                         placement = "H",
                                         tabular.environment = "tabular"){

  if(class(model) == model.lst.class){
    model <- model[[1]]
    if(class(model) != model.class){
      stop("The structure of the model list is incorrect.")
    }
  }

  out.dat <- model$mcmccalcs$sbt.quants
  out.dat <- rbind(out.dat, model$mcmccalcs$depl.quants)

  tab <- f(t(out.dat), digits)
  tab <- cbind(rownames(tab), tab)
  tab <- tab[tab[,1] >= syr,]
  ## Remove the projection year (last row)
  tab <- tab[-nrow(tab),]

  col.names <- colnames(tab)
  col.names[1] <- "Year"
  col.names <- latex.bold(latex.perc(col.names))
  colnames(tab) <- col.names

  addtorow <- list()
  addtorow$pos <- list(-1, nrow(tab))
  addtorow$command <- c(paste0("\\toprule",
                               latex.amp(),
                               latex.mcol(4,
                                          "c",
                                          latex.bold("Spawning Biomass")),
                               latex.amp(),
                               latex.mcol(4,
                                          "c",
                                          latex.bold("Depletion (SB\\subscr{t}/SB\\subscr{0})")),
                               latex.nline,
                               latex.cmidr("2-5", "lr"),
                               " ",
                               latex.cmidr("6-9", "lr")),
                        "\\bottomrule")

  size.string <- latex.size.str(font.size, space.size)
  print(xtable(tab,
               caption = xcaption,
               label = xlabel,
               align = get.align(ncol(tab))),
        caption.placement = "top",
        include.rownames = FALSE,
        sanitize.text.function = function(x){x},
        size = size.string,
        add.to.row = addtorow,
        table.placement = placement,
        hline.after = c(0),
        booktabs = TRUE,
        tabular.environment = tabular.environment)
}

#' Produces an xtable of the sensitivity parameter information as found in
#' the CSV file in the data directory
#'
#' @param tab The contents of the CSV file as read in by read.csv
#' @param xcaption Caption to appear in the calling document
#' @param xlabel The label used to reference the table in latex
#' @param font.size Size of the font for the table
#' @param space.size Size of the vertical spaces for the table
#' @param placement Latex code for placement of the table in document
#'
#' @return An xtable
#' @export
make.sens.parameter.table <- function(tab,
                                      xcaption = "default",
                                      xlabel   = "default",
                                      font.size = 9,
                                      space.size = 10,
                                      placement = "H"){

  ## Replace any | with a comma
  tab <- sub("\\|", ",", as.matrix(tab))
  colnames(tab) <- c(latex.bold("Scenario"),
                     latex.bold("Description"),
                     latex.bold("Parameters"))

  size.string <- latex.size.str(font.size, space.size)
  print(xtable(tab,
               caption = xcaption,
               label = xlabel,
               align = get.align(ncol(tab))),
        caption.placement = "top",
        include.rownames = FALSE,
        sanitize.text.function = function(x){x},
        size = size.string,
        table.placement = placement,
        booktabs = TRUE)
}

#' Produce an xtable for the values of q, including quantiles
#'
#' @param models A list of iscam model objects
#' @param model.names A vector of names of the models in the models list
#' @param digits Number of decimal places for the values in the table
#' @param xcaption Caption to appear in the calling document
#' @param xlabel The label used to reference the table in latex
#' @param font.size Size of the font for the table
#' @param space.size Size of the vertical spaces for the table
#' @param placement Latex code for placement of the table in document
#'
#' @return An xtable
#' @export
make.sens.q.table <- function(models,
                              model.names,
                              digits = 3,
                              xcaption = "default",
                              xlabel   = "default",
                              font.size = 9,
                              space.size = 10,
                              placement = "H"){

  quants <- lapply(models, function(x){
    t(f(x$mcmccalcs$q.quants, digits))})
  tab <- do.call(cbind, quants)
  tab <- cbind(rownames(tab), tab)
  col.names <- colnames(tab)
  col.names[1] <- "$q_k$"
  col.names <- latex.bold(latex.perc(col.names))
  colnames(tab) <- col.names

  ## Add the extra header spanning multiple columns
  addtorow <- list()
  addtorow$pos <- list()
  addtorow$pos[[1]] <- -1
  com <- paste0("\\toprule",
                latex.bold("Index"),
                latex.amp())
  for(i in 1:length(quants)){
    com <- paste0(com,
                  latex.mcol(ncol(quants[[i]]),
                             "c",
                             latex.bold(model.names[i])),
                  ifelse(i != length(quants), latex.amp(), ""))
  }
  com <- paste(com, latex.nline)
  addtorow$command <- com

  size.string <- latex.size.str(font.size, space.size)
  print(xtable(tab,
               caption = xcaption,
               label = xlabel,
               align = get.align(ncol(tab))),
        caption.placement = "top",
        include.rownames = FALSE,
        sanitize.text.function = function(x){x},
        size = size.string,
        add.to.row = addtorow,
        table.placement = placement,
        booktabs = TRUE)
}

#' Produce an xtable containing the variance parameter results data as loaded from
#' variance-parameter-results.csv
#'
#' @param var the variance parameter results data as loaded from
#'   variance-parameter-results.csv
#' @param which.model 1 = AM1, 2 = AM2 (Herring)
#' @param digits Number of decimal places for the values for the table
#' @param xcaption Caption to appear in the calling document
#' @param xlabel The label used to reference the table in latex
#' @param font.size Size of the font for the table
#' @param space.size Size of the vertical spaces for the table
#' @param placement Latex code for placement of the table in document
#'
#' @return An xtable
#' @export
make.variance.table <- function(var,
                                which.model = 1,
                                digits = 3,
                                xcaption = "default",
                                xlabel   = "default",
                                font.size = 9,
                                space.size = 10,
                                placement = "H"){
  ## var - the variance parameter results data as loaded from
  ##  variance-parameter-results.csv
  ## which.model to make the table for, 1 = AM1, 2 = AM2
  ## digits - number of decimal places for the values
  ## xcaption - caption to appear in the calling document
  ## xlabel - the label used to reference the table in latex
  ## font.size - size of the font for the table
  ## space.size - size of the vertical spaces for the table
  ## placement - latex code for placement of the table in document

  if(which.model == 1){
    model <- "AM1"
  }else if(which.model == 2){
    model <- "AM2"
  }else{
    stop("which.model must be 1 or 2.")
  }

  var.res <- var %>%
    as_tibble( ) %>%
    filter( Model == model ) %>%
    select( Parameter, SensitivityCase, InitialValues, EstimatedValues ) %>%
    rename( Case=SensitivityCase, Initial=InitialValues, Estimated=EstimatedValues ) %>%
    gather( Initial, Estimated, key="Type", value="Value" ) %>%
    unite( CaseType, Case, Type ) %>%
    mutate( CaseType=factor(CaseType, levels=unique(CaseType)) ) %>%
    spread( CaseType, Value ) %>%
    mutate( Parameter=factor(Parameter, levels=unique(variance.results$Parameter)) ) %>%
    arrange( Parameter )

  var.res <- as.data.frame(var.res)
  ## Change the order of the columns
  tab <- var.res[,c(1,2,8,3,9,4,10,5,11,6,12,7,13)]
  tab[,1] <- c("Log recruitment ($ln(R_0)$)",
               "Steepness ($h$)",
               "Log natural mortality ($ln(M)$)",
               "Log mean recruitment ($\\ln(\\overline{R})$)",
               "Log initial recruitment ($\\ln(\\overline{R}_{init})$)",
               "Variance ratio, rho ($\\rho$)",
               "Inverse total variance, kappa ($\\kappa$)",
               "Sigma ($\\sigma$)",
               "Tau ($\\tau$)")

  addtorow <- list()
  addtorow$pos <- list(0)
  addtorow$command <- paste0(latex.amp(),
                             latex.mcol(2,
                                        "c",
                                        latex.bold("Base")),
                             latex.amp(),
                             latex.mcol(2,
                                        "c",
                                        latex.bold("1")),
                             latex.amp(),
                             latex.mcol(2,
                                        "c",
                                        latex.bold("2")),
                             latex.amp(),
                             latex.mcol(2,
                                        "c",
                                        latex.bold("3")),
                             latex.amp(),
                             latex.mcol(2,
                                        "c",
                                        latex.bold("4")),
                             latex.amp(),
                             latex.mcol(2,
                                        "c",
                                        latex.bold("5")),
                             latex.nline,
                             ## underscores
                             latex.cmidr("2-3", "lr"),
                             " ",
                             latex.cmidr("4-5", "lr"),
                             " ",
                             latex.cmidr("6-7", "lr"),
                             " ",
                             latex.cmidr("8-9", "lr"),
                             " ",
                             latex.cmidr("10-11", "lr"),
                             " ",
                             latex.cmidr("12-13", "lr"),
                             latex.bold("Leading Parameters"),
                             latex.amp(),
                             latex.bold("Initial"),
                             latex.amp(),
                             latex.bold("Estimated"),
                             latex.amp(),
                             latex.bold("Initial"),
                             latex.amp(),
                             latex.bold("Estimated"),
                             latex.amp(),
                             latex.bold("Initial"),
                             latex.amp(),
                             latex.bold("Estimated"),
                             latex.amp(),
                             latex.bold("Initial"),
                             latex.amp(),
                             latex.bold("Estimated"),
                             latex.amp(),
                             latex.bold("Initial"),
                             latex.amp(),
                             latex.bold("Estimated"),
                             latex.amp(),
                             latex.bold("Initial"),
                             latex.amp(),
                             latex.bold("Estimated"),
                             latex.nline)

  size.string <- latex.size.str(font.size, space.size)
  print(xtable(tab,
               caption = xcaption,
               label = xlabel,
               align = get.align(ncol(tab))),
        caption.placement = "top",
        include.rownames = FALSE,
        include.colnames = FALSE,
        sanitize.text.function = function(x){x},
        size = size.string,
        add.to.row = addtorow,
        table.placement = placement,
        booktabs = TRUE)
}

#' Produce an xtable containing the catchability sensitivity values
#'
#' @param qa.lst A list of AM1 models for Herring sensitivities for case A
#' @param qb.lst A list of AM1 models for Herring sensitivities for case B
#' @param qc.lst A list of AM1 models for Herring sensitivities for case C
#' @param digits Number of digits after decimal point for the table
#' @param xcaption Caption to appear in the calling document
#' @param xlabel The label used to reference the table in latex
#' @param font.size Size of the font for the table
#' @param space.size Size of the vertical spaces for the table
#' @param placement Latex code for placement of the table in document
#'
#' @details Catchability  parameters:
#'   q is a data frame with 1 column for each survey and 3 rows:
#'   1 - prior type:
#'        0) Uniformative prior
#'        1) normal prior density for log(q)
#'        2) random walk in q
#'   2 - prior log(mean)
#'   3 - prior SD
#'
#' @return An xtable
#' @export
make.catchability.parameters.table.q.sens <- function(qa.lst,
                                                      qb.lst,
                                                      qc.lst,
                                                      digits = 3,
                                                      xcaption = "default",
                                                      xlabel   = "default",
                                                      font.size = 9,
                                                      space.size = 10,
                                                      placement = "H"){

  lst <- list()
  st <- c("HG", "PRD", "CC", "SOG", "WCVI")
  for(i in 1:5){
    mdl.qa <- qa.lst[[i]][[1]]
    mdl.qb <- qb.lst[[i]][[1]]
    mdl.qc <- qc.lst[[i]][[1]]
    mc.qa <- mdl.qa$mcmccalcs
    mc.qb <- mdl.qb$mcmccalcs
    mc.qc <- mdl.qc$mcmccalcs
    p.quants.qa <- as.data.frame(mc.qa$p.quants)
    p.quants.qb <- as.data.frame(mc.qb$p.quants)
    p.quants.qc <- as.data.frame(mc.qc$p.quants)
    ## AM1
    ctl.qa <- mdl.qa$ctl
    ctl.qb <- mdl.qb$ctl
    ctl.qc <- mdl.qc$ctl
    ## AM1 q estimates
    q1.est.qa <- f(p.quants.qa$q1[2], digits)
    q2.est.qa <- f(p.quants.qa$q2[2], digits)
    q1.est.qb <- f(p.quants.qb$q1[2], digits)
    q2.est.qb <- f(p.quants.qb$q2[2], digits)
    q1.est.qc <- f(p.quants.qc$q1[2], digits)
    q2.est.qc <- f(p.quants.qc$q2[2], digits)
    ## AM1 q priors
    q.qa <- as.data.frame(t(ctl.qa$surv.q))
    q.qb <- as.data.frame(t(ctl.qb$surv.q))
    q.qc <- as.data.frame(t(ctl.qc$surv.q))
    q.qa$priormeanlog <- exp(q.qa$priormeanlog)
    q.qb$priormeanlog <- exp(q.qb$priormeanlog)
    q.qc$priormeanlog <- exp(q.qc$priormeanlog)
    p.vals.qa <- paste0("Normal($",
                        f(q.qa$priormeanlog[1], 3),
                        ", ",
                        f(q.qa$priorsd[1], 3),
                        "$)")
    p.vals.qb <- paste0("Normal($",
                        f(q.qb$priormeanlog[1], 3),
                        ", ",
                        f(q.qb$priorsd[1], 3),
                        "$)")
    p.vals.qc <- paste0("Normal($",
                        f(q.qc$priormeanlog[1], 3),
                        ", ",
                        f(q.qc$priorsd[1], 3),
                        "$)")
    sb.end.qa <- f(mc.qa$r.quants[3, 3], digits)
    sb.end.qb <- f(mc.qb$r.quants[3, 3], digits)
    sb.end.qc <- f(mc.qc$r.quants[3, 3], digits)
    sb.end.yr <- gsub("sb", "", rownames(mc.qa$r.quants)[3])
    sbo.qa <- f(p.quants.qa$sbo[2], digits)
    sbo.qb <- f(p.quants.qb$sbo[2], digits)
    sbo.qc <- f(p.quants.qc$sbo[2], digits)
    depl.qa <- f(mc.qa$depl.quants[,ncol(mc.qa$depl.quants) - 1][2], digits)
    depl.qb <- f(mc.qb$depl.quants[,ncol(mc.qb$depl.quants) - 1][2], digits)
    depl.qc <- f(mc.qc$depl.quants[,ncol(mc.qc$depl.quants) - 1][2], digits)

    lst[[i]] <- rbind(c(st[i],
                      "AM1",
                      "None",
                      q1.est.qa,
                      q2.est.qa,
                      p.vals.qa,
                      sb.end.qa,
                      sbo.qa,
                      depl.qa),
                    c(st[i],
                      "AM1",
                      "None",
                      q1.est.qb,
                      q2.est.qb,
                      p.vals.qb,
                      sb.end.qb,
                      sbo.qb,
                      depl.qb),
                    c(st[i],
                      "AM1",
                      "None",
                      q1.est.qc,
                      q2.est.qc,
                      p.vals.qc,
                      sb.end.qc,
                      sbo.qc,
                      depl.qc))
  }
  tab <- do.call(rbind, lst)
  colnames(tab) <- c(latex.bold("SAR"),
                     latex.bold("Model"),
                     latex.bold("Bounds"),
                     latex.mlc(c("Estimated",
                                 "q1")),
                     latex.mlc(c("Estimated",
                                 "q2")),
                     latex.bold("Prior (mean, SD)"),
                     latex.bold(paste0("SB\\subscr{",
                                       sb.end.yr,
                                       "}")),
                     latex.bold("SB\\subscr{0}"),
                     latex.mlc(c("Depletion",
                                 paste0("SB\\subscr{",
                                        sb.end.yr,
                                        "}/SB\\subscr{0}"))))

  addtorow <- list()
  addtorow$pos <- list(3, 6, 9, 12)
  addtorow$command <- c("\\midrule ",
                        "\\midrule ",
                        "\\midrule ",
                        "\\midrule ")
  size.string <- latex.size.str(font.size, space.size)
  print(xtable(tab,
               caption = xcaption,
               label = xlabel,
               align = get.align(ncol(tab))),
        caption.placement = "top",
        include.rownames = FALSE,
        sanitize.text.function = function(x){x},
        size = size.string,
        table.placement = placement,
        add.to.row = addtorow,
        booktabs = TRUE)
}
