CoordinateSystem <- R6::R6Class("CoordinateSystem",
  private = list(
    .axes = list()   # list of CoordinateSystemAxis
  ),
  public = list(
    #' @description Create a new coordinate system. The CS must have at least
    #' one axis.
    #' @param axes A `list` of instances of [CoordinateSystemAxis]. There must
    #' be at least one axis in the list.
    #' @return An instance of `CoordinateSystem` or an error.
    initialize = function(axes) {
      if (is.list(axes) && length(axes >= 1L) &&
          all(sapply(axes, inherits, "CoordinateSystemAxes")))
        private$.axes <- axes
      else
        stop("Argument `axes` is not a list containing `CoordinateSystemAxis` instances.", call. = FALSE)
    },

    #' @description Print a summary of the coordinate system to the console.
    #' @param ... Ignored.
    #' @return Self, invisibly.
    print = function(...) {
      cat("<Coordinate System>\n")
      for (ax in private$.axes) ax$print()
      invisible(self)
    },

    getDimension = function() {
      length(private$.axes)
    },

    getAxis = function(dimension) {
      private$.axes[[dimension]]
    }
  )
)
