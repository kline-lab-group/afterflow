# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

MakeMethod <- function(c, m, f) {
  if (is.null(getGeneric(m))) {
    setGeneric(
      name = m,
      def = function(obj) {
        standardGeneric(m)
      }
    )
  }
  setMethod(
    f = m,
    signature = c,
    definition = f
  )
}

setClass(
  "FileHandler",
  slots = list(
    "main" = "character",
    "fcs" = "character", "rcode" = "character",
    "input" = "character", "output" = "character",
    "prefix" = "character", "suffix" = "character"
  )
)

MakeMethod(
  "FileHandler",
  "initialize",
  function(.Object) {
    .Object@main <- box_drive("kline-lab/databackup/Flow Cytometry/Hodgkin")
    .Object@fcs <- sprintf("%s/%s", .Object@main, "Flowjo_analysis/Export_Xiufen")
    .Object@rcode <- sprintf("%s/%s", .Object@main, "R_analysis")
    .Object@input <- sprintf("%s/%s", .Object@rcode, "input")
    .Object@output <- sprintf("%s/%s", .Object@rcode, "output")
    .Object@prefix <- "export"
    .Object@suffix <- "0321_XC"
    return(.Object)
  }
)
#
# "mdir" = ,
# "fdir" = "Flowjo_analysis/Export_Xiufen",
# "rdir" = "R_analysis",
# "idir" = "input",
# "odir" = "output",
# "prefix" = "export",
# "suffix" = "0321_XC",
# )
# )
#
# setGeneric(
#   name = "move", def = function(theObject) { standardGeneric("move") }
# )
