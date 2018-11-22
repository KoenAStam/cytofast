#' @import methods
#' @include cfList-class.R
NULL

#' @rdname cfList-class
#' @usage NULL
#' @export
setGeneric("samples", function(object) standardGeneric("samples"))

#' @rdname cfList-class
#' @aliases samples,cfList-method
#' @usage NULL
setMethod("samples", signature = "cfList", definition =
            function(object) object@samples)

#' @rdname cfList-class
#' @usage NULL
#' @export
setGeneric("expr", function(object) standardGeneric("expr"))

#' @rdname cfList-class
#' @aliases expr,cfList-method
#' @usage NULL
setMethod("expr", signature = "cfList", definition =
            function(object) object@expr)

#' @rdname cfList-class
#' @usage NULL
#' @export
setGeneric("counts", function(object) standardGeneric("counts"))

#' @rdname cfList-class
#' @aliases counts,cfList-method
#' @usage NULL
setMethod("counts", signature = "cfList", definition =
            function(object) object@counts)

#' @rdname cfList-class
#' @usage NULL
#' @export
setGeneric("results", function(object) standardGeneric("results"))

#' @rdname cfList-class
#' @aliases results,cfList-method
#' @usage NULL
setMethod("results", signature = "cfList", definition =
            function(object) object@results)