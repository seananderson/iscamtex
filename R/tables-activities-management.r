#' Produce an activities xtable for Herring assessment
#'
#' @param tab The table of data as read in by load.csv()
#' @param xcaption Caption to appear in the calling document
#' @param xlabel The label used to reference the table in latex
#' @param font.size Size of the font for the table
#' @param space.size Size of the vertical spaces for the table
#' @param placement Latex code for placement of the table in document
#'
#' @return An xtable
#' @export
make.activities.table <- function(tab,
                                  xcaption = "default",
                                  xlabel   = "default",
                                  font.size = 9,
                                  space.size = 10,
                                  placement = "H"){

  colnames(tab) <- c("",
                     latex.bold("HG"),
                     latex.bold("PRD"),
                     latex.bold("CC"),
                     latex.bold("SOG"),
                     latex.bold("HWCVI"))

  fsc <- c("\\midrule FSC",
           latex.mlc(c("SOK",
                       "(harvest of wild",
                       "SOK, closed ponds)"),
                     FALSE),
           latex.mlc(c("Whole herring",
                       "SOK",
                       "spawn-on-",
                       "boughs"),
                     FALSE),
           latex.mlc(c("SOK",
                       "(closed and open",
                       "ponds) spawn-on-",
                       "boughs"),
                     FALSE),
           latex.mlc(c("Whole herring",
                       "and spawn-on-",
                       "boughs"),
                     FALSE),
           latex.mlc(c("Whole herring",
                       "SOK (closed and",
                       "open ponds),",
                       "spawn-on-boughs"),
                     FALSE),
           " \\bottomrule ")
           ##paste0(latex.nline, " \\bottomrule "))

  tab <- rbind(tab, fsc)

  size.string <- latex.size.str(font.size, space.size)
  print(xtable(as.data.frame(tab),
               caption = xcaption,
               label = xlabel,
               align = get.align(ncol(tab))),
        caption.placement = "top",
        include.rownames = FALSE,
        sanitize.text.function = function(x){x},
        size = size.string,
        table.placement = placement,
        ##hline.after = c(0),
        booktabs = TRUE)
}

#' Produce a management xtable for Herring assessment
#'
#' @param tab The table of data as read in by load.csv()
#' @param xcaption Caption to appear in the calling document
#' @param xlabel The label used to reference the table in latex
#' @param font.size Size of the font for the table
#' @param space.size Size of the vertical spaces for the table
#' @param placement Latex code for placement of the table in document
#'
#' @return An xtable
#' @export
make.management.table <- function(tab,
                                  xcaption = "default",
                                  xlabel   = "default",
                                  font.size = 9,
                                  space.size = 10,
                                  placement = "H"){
  ## tab - the table of data as read in by load.csv()
  ## xcaption - caption to appear in the calling document
  ## xlabel - the label used to reference the table in latex
  ## font.size - size of the font for the table
  ## space.size - size of the vertical spaces for the table
  ## digits - number of decimal points on % columns
  ## placement - latex code for placement of the table in document

  tab <- tab[-c(1,2),]
  tmp <- remove.asterisks(tab[,-1])
  tab <- cbind(tab[,1], add.asterisks(f(tmp[[1]]), tmp[[2]]))

  colnames(tab) <- c(latex.bold("Year"),
                     latex.bold("CSAS"),
                     latex.bold("TAC"),
                     latex.mlc(c("Total",
                                 "landings")),
                     latex.bold("CSAS"),
                     latex.bold("TAC"),
                     latex.mlc(c("Total",
                                 "landings")),
                     latex.bold("CSAS"),
                     latex.bold("TAC"),
                     latex.mlc(c("Total",
                                 "landings")),
                     latex.bold("CSAS"),
                     latex.bold("TAC"),
                     latex.mlc(c("Total",
                                 "landings")),
                     latex.bold("CSAS"),
                     latex.bold("TAC"),
                     latex.mlc(c("Total",
                                 "landings")))
  ## Add the extra header spanning multiple columns
  addtorow <- list()
  addtorow$pos <- list()
  addtorow$pos[[1]] <- -1
  addtorow$command <- paste0("\\toprule",
                             latex.bold("SAR"),
                             latex.amp(),
                             latex.mcol(3, "c",
                                        latex.bold("HG")),
                             latex.amp(),
                             latex.mcol(3, "c",
                                        latex.bold("PRD")),
                             latex.amp(),
                             latex.mcol(3, "c",
                                        latex.bold("CC")),
                             latex.amp(),
                             latex.mcol(3, "c",
                                        latex.bold("SOG")),
                             latex.amp(),
                             latex.mcol(3, "c",
                                        latex.bold("WCVI")),
                             latex.nline)

  size.string <- latex.size.str(font.size, space.size)
  print(xtable(as.data.frame(tab),
               caption = xcaption,
               label = xlabel,
               align = get.align(ncol(tab))),
        caption.placement = "top",
        include.rownames = FALSE,
        sanitize.text.function = function(x){x},
        size = size.string,
        table.placement = placement,
        add.to.row = addtorow,
        ##hline.after = c(0),
        booktabs = TRUE)
}
