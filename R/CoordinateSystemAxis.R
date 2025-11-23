# The AxisDirection class from the Java implementation is here represented as a character vector.
# The values in this vector are the only allowable values for the direction of a
# CoordinateSystemAxis
AxisDirection <- c(
  "COLUMN_NEGATIVE",  # Axis positive direction is towards lower pixel column.
  "COLUMN_POSITIVE",  # Axis positive direction is towards higher pixel column.
  "DISPLAY_DOWN",     # Axis positive direction is towards bottom of approximately vertical display surface.
  "DISPLAY_LEFT",     # Axis positive direction is left in display.
  "DISPLAY_RIGHT",    # Axis positive direction is right in display.
  "DISPLAY_UP",       # Axis positive direction is towards top of approximately vertical display surface.
  "DOWN",             # Axis positive direction is down relative to gravity.
  "EAST",             # Axis positive direction is π/2 radians clockwise from north.
  "EAST_NORTH_EAST",  # Axis positive direction is approximately east-north-east.
  "EAST_SOUTH_EAST",  # Axis positive direction is approximately east-south-east.
  "FUTURE",           # Axis positive direction is towards the future.
  "GEOCENTRIC_X",     # Axis positive direction is in the equatorial plane from the centre of the modelled earth towards the intersection of the equator with the prime meridian.
  "GEOCENTRIC_Y",     # Axis positive direction is in the equatorial plane from the centre of the modelled earth towards the intersection of the equator and the meridian π/2 radians eastwards from the prime meridian.
  "GEOCENTRIC_Z",     # Axis positive direction is from the centre of the modelled earth parallel to its rotation axis and towards its north pole.
  "NORTH",            # Axis positive direction is north.
  "NORTH_EAST", # Axis positive direction is approximately north-east.
  "NORTH_NORTH_EAST", # Axis positive direction is approximately north-north-east.
  "NORTH_NORTH_WEST", # Axis positive direction is approximately north-north-west.
  "NORTH_WEST",       # Axis positive direction is approximately north-west.
  "OTHER",            # Unknown or unspecified axis orientation.
  "PAST",             # Axis positive direction is towards the past.
  "ROW_NEGATIVE",     # Axis positive direction is towards lower pixel row.
  "ROW_POSITIVE",     # Axis positive direction is towards higher pixel row.
  "SOUTH",            # Axis positive direction is π radians clockwise from north.
  "SOUTH_EAST",       # Axis positive direction is approximately south-east.
  "SOUTH_SOUTH_EAST", # Axis positive direction is approximately south-south-east.
  "SOUTH_SOUTH_WEST", # Axis positive direction is approximately south-south-west.
  "SOUTH_WEST",       # Axis positive direction is approximately south-west.
  "UP",               # Axis positive direction is up relative to gravity.
  "WEST",             # Axis positive direction is 3π/2 radians clockwise from north.
  "WEST_NORTH_WEST",  # Axis positive direction is approximately west-north-west.
  "WEST_SOUTH_WEST")  # Axis positive direction is approximately west-south-west.

#' Coordinate system axis
#'
#' @description This class implements the coordinate system axis class. The
#' axis is always associated with a coordinate system.
#'
#' @docType class
CoordinateSystemAxis <- R6::R6Class("CoordinateSystemAxis",
  inherit = IdentifiedObject,
  private = list(
    .abbreviation = NA_character_,
    .direction = NA_character_,
    .unit = NA_character_
  ),
  public = list(
    #' @description Create a new axis instance for use in a coordinate system.
    #' @param name Character string. Name of the axis.
    #' @param abbreviation Character string. Abbreviation of the axis.
    #' @param direction Character string. Direction of the axis. Must be one
    #'   from a set of values.
    #' @param unit Character string. Unit of measure of the axis.
    #' @return An instance of this class or an error.
    initialize = function(name, abbreviation, direction, unit) {
      super$initialize(name)

      if (is.character(abbreviation) && length(abbreviation) == 1L && nzchar(abbreviation))
        private$.abbreviation <- abbreviation
      else
        stop("Axis abbreviation must be a character string.", call. = FALSE)

      if (is.character(direction) && length(direction) == 1L && toupper(direction) %in% AxisDirection)
        private$.direction <- direction
      else
        stop("Axis direction must be a character string from `AxisDirection`.", call. = FALSE)

      if (is.character(unit) && length(unit) == 1L && nzchar(unit))
        private$.unit <- unit
      else
        stop("Axis unit must be a character string.", call. = FALSE)
    },

    #' @description Print a summary of the coordinate system axis to the
    #'   console.
    #' @param ... Ignored.
    #' @return Self, invisible.
    print = function(...) {
      cat(sprintf("Axis: %s (%s), direction=%s, unit=%s\n",
                  private$.name, private$.abbreviation, private$.direction, private$.unit))
      invisible(self)
    },

    #' @description Retrieve the abbreviation of the axis. This method is
    #'   mandatory in the OGC standard.
    #' @return Character string with the abbreviation of the axis.
    getAbbreviation = function() {
      private$.abbreviation
    },

    #' @description Retrieve the direction of the axis. This method is
    #'   mandatory in the OGC standard.
    #' @return Character string with the direction of the axis.
    getDirection = function() {
      private$.direction
    },

    #' @description Retrieve the unit of measure of the axis. This method is
    #'   mandatory in the OGC standard.
    #' @return Character string with the unit of measure of the axis.
    getUnit = function() {
      private$.unit
    },

    #' @description Retrieve the minimum coordinate value of this axis, in units
    #' of the unit of measure of the axis.
    #' @return Negative infinity.
    getMinimumValue = function() {
      -Inf
    },

    #' @description Retrieve the maximum coordinate value of this axis, in units
    #' of the unit of measure of the axis.
    #' @return Positive infinity.
    getMaximumValue = function() {
      +Inf
    },

    #' @description Retrieve the minimum coordinate value of this axis, in units
    #' of the unit of measure of the axis.
    #' @return The character string "EXACT".
    getRangeMeaning = function() {
      "EXACT"
    }
  )
)
