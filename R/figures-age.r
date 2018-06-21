#' Plot the age composition estimates
#'
#' @param model An iscam model object
#' @param which.gear An index for the gear to plot (see iscam data file)
#' @param fg The foreground color to use in the symbols() call
#' @param bg The background color to use in the symbols() call
#' @param inches Size of bubbles in inches in the symbols() call
#'
#' @return Nothing
#' @export
#'
#' @examples
#' \donttest{}
make.age.comp.estimates.plot <- function(model,
                                         which.gear,
                                         fg = gray(level = 0.1, alpha = 0.5),
                                         bg = gray(level = 0.5, alpha = 0.5),
                                         inches = 0.12){

  ## ahat <- t(model$mpd$ahat[[which.gear]])
  ## ahat is a matrix which needs to be broken up into smaller
  ##  matrices, one for each gear
  ahat <- model$mpd$A_hat
  nagv <- model$dat$num.age.gears.vec
  sage <- model$mpd$n_A_sage[1]
  nage <- model$mpd$n_A_nage[1]
  ## Need the age comp data for the years
  age.comps <- model$dat$age.comps

  a.props <- list()
  ind <- 1
  for(i in 1:length(nagv)){
    yrs <- as.data.frame(age.comps[[i]])$year
    a.props[[i]] <- ahat[ind:(ind + length(yrs) - 1),]
    rownames(a.props[[i]]) <- yrs
    colnames(a.props[[i]]) <- sage:nage
    ind <- ind + length(yrs)
  }
  dat <- a.props[[which.gear]]

  x <- data.frame(expand.grid(as.numeric(rownames(dat)),
                              as.numeric(colnames(dat))),
                  prop = as.vector(dat))
  yrs <- as.numeric(rownames(dat))
  names(x) <- c("yr", "age", "prop")
  max.prop <- max(dat)
  symbols(c(x[,1], -1),
          c(x[,2], -1),
          circles = sqrt(c(x[,3], max.prop)),
          inches = inches,
          ylim = c(sage, nage),
          xlim = c(min(yrs), max(yrs)),
          xlab = "",
          ylab = "Age",
          xaxt = "n",
          fg = fg,
          bg = bg)
  axis(1, yrs)
  axis(4)
}

#' Plot the age comp fits
#'
#' @param model An iscam model object
#' @param which.gear An index for the gear to plot (see iscam data file)
#' @param fg The foreground color to use in the symbols() call
#' @param bg The background color to use in the symbols() call
#' @param inches Size of bubbles in inches in the symbols() call
#'
#' @return Nothing
#' @export
#'
#' @examples
#' \donttest{}
make.age.comp.fit.plot <- function(model,
                                   which.gear,
                                   fg = gray(level = 0.1, alpha = 0.5),
                                   bg = gray(level = 0.5, alpha = 0.5),
                                   inches = 0.12){

  fit.dat <- as.data.frame(model$mpd$A_hat)
  comp.dat <- as.data.frame(model$mpd$d3_A)

  s.age <- model$mpd$n_A_sage[which.gear]
  n.age <- model$mpd$n_A_nage[which.gear]
  ages <- s.age:n.age
  n.ages <- length(s.age:n.age)

  ## resid.dat has unlabelled rows, which need to be extracted
  ##  correctly based on the values in the comp.dat data.frame
  which.rows <- which(comp.dat[,2] == which.gear)
  comp.dat <- comp.dat[which.rows ,]
  yrs <- comp.dat[, 1]

  fit.dat <- fit.dat[which.rows,]
  ## Remove the information columns, leaving only the comps
  comp.dat <- as.matrix(comp.dat[, -seq(1, 5)])
  obs.prop <- prop.table(comp.dat, 1)

  max.y <- max(fit.dat, obs.prop)
  n.side <- get.rows.cols(length(yrs))
  par(mfrow = n.side,
      oma = c(2, 3, 1, 1),
      mai = c(0.2, 0.4, 0.3, 0.2))
  for(yr in 1:length(yrs)){
    year <- yrs[yr]
    obs <- obs.prop[yr,]
    est <- fit.dat[yr,]
    plot(ages,
         obs,
         type = "h",
         xlab = "",
         ylab = "",
         main = year,
         las = 1,
         ylim = c(0, max.y))
    lines(ages, est, lty = 1, lwd = 2, col = 2)
  }
  mtext("Age", side = 1, line = 0, outer = TRUE)
  mtext("Proportion", side = 2, line = 1, outer = TRUE)
}

#' Plot the age composition residuals
#'
#' @param model An iscam model object
#' @param which.gear An index for the gear to plot (see iscam data file)
#' @param fg The foreground color to use in the symbols() call
#' @param bg The background color to use in the symbols() call
#' @param inches Size of bubbles in inches in the symbols() call
#'
#' @return Nothing
#' @export
#'
#' @examples
#' \donttest{}
make.age.comp.residuals.plot <- function(model,
                                         which.gear,
                                         fg = gray(level = 0.1, alpha = 0.5),
                                         bg = gray(level = 0.5, alpha = 0.5),
                                         inches = 0.12){

  resid.dat <- as.data.frame(model$mpd$A_nu)
  comp.dat <- as.data.frame(model$mpd$d3_A)

  s.age <- model$mpd$n_A_sage[which.gear]
  n.age <- model$mpd$n_A_nage[which.gear]
  ages <- s.age:n.age
  n.ages <- length(s.age:n.age)


  ## resid.dat has unlabelled rows, which need to be extracted
  ##  correctly based on the values in the comp.dat data.frame
  which.rows <- which(comp.dat[,2] == which.gear)
  comp.dat <- comp.dat[which.rows ,]
  yrs <- comp.dat[, 1]

  resid.dat <- resid.dat[which.rows,]
  ## Remove the information columns, leaving only the comps
  comp.dat <- as.matrix(comp.dat[, -seq(1, 5)])
  obs.prop <- prop.table(comp.dat, 1)

  plotBubbles(t(resid.dat),
              xval = yrs,
              yval = ages,
              size = 0.1,
              powr = 0.5,
              xlab = "Year",
              ylab = "Age",
              las = 1,
              cex = 1.25,
              axes = FALSE)
  axis(1, at = yrs, labels = yrs)
}

#' Plot the age composition data as seen in the iscam data file
#'
#' @param model An iscam model object
#' @param which.gear An index for the gear to plot (see iscam data file)
#'
#' @return Nothing
#' @export
#'
#' @examples
#' \donttest{}
make.age.comp.data.plot <- function(model,
                                    which.gear){

  a <- model$dat$age.comps[[which.gear]]
  yrs <- a[,1]
  a <- a[,-c(1, 2, 3, 4, 5)]
  plotBubbles(t(a),
              xval = yrs,
              yval = colnames(a),
              size = 0.1,
              powr = 0.5,
              xlab = "Year",
              ylab = "Age",
              cex = 0.75)
##              axes = FALSE)

}
