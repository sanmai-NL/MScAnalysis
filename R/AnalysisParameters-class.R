# TODO: do not import all of methods
#' @import methods
NULL

#' @importClassesFrom feat Annotations
#' @exportClass AnalysisParameters
#' @export AnalysisParameters
AnalysisParameters <-
    methods::setClass(
        Class='AnalysisParameters',
        slots=base::list(
            ANNOTATIONS='Annotations',
            # TODO: use S4 container class
            DESIGN_MATRIX_LST='list',
            K_I='integer',
            TIMES_I='integer'),
        prototype=base::list(
            ANNOTATIONS=NULL,
            DESIGN_MATRIX_LST=NULL,
            K_I=NULL,
            TIMES_I=NULL),
        sealed=TRUE)
