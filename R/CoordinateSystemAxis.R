#' Coordinate system axis
#'
#' @description This class implements the coordinate system axis class. The
#' axis is always associated with a coordinate system.
#'
#' @export
#' @docType class
CoordinateSystemAxis <- R6::R6Class("CoordinateSystemAxis",
  private = list(
    name = NULL,      # character
    abbreviation = NULL,
    direction = NULL, # e.g., "east", "north", "up"
    unit = NULL      # e.g., "m", "degree"
  ),
  public = list(
    #' @description Create a new axis instance for use in a coordinate system.
    #' @param name Name
    #' @param abbreviation Abbreviation
    #' @param direction Direction
    #' @param unit Unit
    #' @return An instance of this class or an error.
    initialize = function(name, abbreviation, direction, unit) {
      private$.name <- name
      private$.abbreviation <- abbreviation
      private$.direction <- direction
      private$.unit <- unit
    },

    print = function(...) {
      cat(sprintf("Axis: %s (%s), direction=%s, unit=%s\n",
                  private$.name, private$.abbreviation, private$.direction, private$.unit))
    }
  )
)
