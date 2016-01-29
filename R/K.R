## K measure
## A (simplistic) implementation of Sebastiani's K measure for text classification (2015). IMPORTANT: a positive integer confusion matrix \code{CONFUSION_IMAT} with ground truth columns and prediction rows is required.
## LITERATURE
#
# @inproceedings{Sebastiani2015,
# abstract = {We address the general problem of finding suitable evaluation measures for classification systems. To this end, we adopt an axiomatic approach, i.e., we discuss a number of properties ("axioms") that an evaluation measure for classification should arguably satisfy. We start our analysis by addressing binary classification. We show that F1, nowadays considered a standard measure for the evaluation of binary classification systems, does not comply with a number of them, and should thus be considered unsatisfactory. We go on to discuss an alternative, simple evaluation measure for binary classification, that we call K, and show that it instead satisfies all the previously proposed axioms. We thus argue that researchers and practitioners should replace F1 with K in their everyday binary classification practice. We carry on our analysis by showing that K can be smoothly extended to deal with single-label multi-class classification, cost-sensitive classification, and ordinal classification.},
# address = {New York, New York, USA},
# author = {Sebastiani, Fabrizio},
# booktitle = {Proceedings of the 2015 International Conference on Theory of Information Retrieval - ICTIR '15},
# doi = {10.1145/2808194.2809449},
# isbn = {9781450338332},
# keywords = {classification,evaluation,evaluation measures,ordinal classification.,text classification,thesis},
# month = {sep},
# pages = {11--20},
# publisher = {ACM Press},
# title = {{An Axiomatically Derived Measure for the Evaluation of Classification Algorithms}},
# url = {http://dl.acm.org/citation.cfm?id=2808194.2809449 http://dl.acm.org/citation.cfm?doid=2808194.2809449},
# year = {2015}
# }

#' Preconditions: CONFUSION_IMAT is a square positive matrix.
TP <- function(CONFUSION_IMAT) {
    CONFUSION_IMAT[1L, 1L]
}

#' Preconditions: CONFUSION_IMAT is a square positive matrix.
TN <- function(CONFUSION_IMAT) {
    CONFUSION_IMAT[2L, 2L]
}

#' Preconditions: CONFUSION_IMAT is a square positive matrix.
FN <- function(CONFUSION_IMAT) {
    CONFUSION_IMAT[2L, 1L]
}

#' Preconditions: CONFUSION_IMAT is a square positive matrix.
FP <- function(CONFUSION_IMAT) {
    CONFUSION_IMAT[1L, 2L]
}

AP <- function(CONFUSION_IMAT) {
    TP(CONFUSION_IMAT) + FN(CONFUSION_IMAT)
}

AN <- function(CONFUSION_IMAT) {
    TN(CONFUSION_IMAT) + FP(CONFUSION_IMAT)
}

#' Precision
π <- function(CONFUSION_IMAT) {
    TP(CONFUSION_IMAT) / (TP(CONFUSION_IMAT) + FP(CONFUSION_IMAT))
}

#' Recall (sensitivity)
ρ <- function(CONFUSION_IMAT) {
    TP(CONFUSION_IMAT) / (TP(CONFUSION_IMAT) + FN(CONFUSION_IMAT))
}

#' Specificity
σ <- function(CONFUSION_IMAT) {
    TN(CONFUSION_IMAT) / (FP(CONFUSION_IMAT) + TN(CONFUSION_IMAT))
}

balanced_accuracy <- function(CONFUSION_IMAT) {
    (ρ(CONFUSION_IMAT) + σ(CONFUSION_IMAT)) / 2
}

#' To calculate the K measure for a 2-class label (See \code{\link{K_SLC}} for a generalization to n > 2 classes).
#'
#' Preconditions: AP >= 0 and AN >= 0 and AP + AN > 0.
#' @export
K <- function(CONFUSION_IMAT) {
    AP_I <- AP(CONFUSION_IMAT)
    AN_I <- AN(CONFUSION_IMAT)
    stopifnot(AP_I >= 0)
    stopifnot(AN_I >= 0)
    stopifnot((AP_I + AN_I) > 0)
    if (AP_I > 0) {
        if (AN_I > 0) {
            return(ρ(CONFUSION_IMAT) + σ(CONFUSION_IMAT) - 1)
        } else {
            return(2 * σ(CONFUSION_IMAT) - 1)
        }
    } else {
        return(2 * ρ(CONFUSION_IMAT) - 1)
    }
}

########################

#' Number of objects predicted to belong to the correct class (\eqn{c_j}).
#' Preconditions: CONFUSION_IMAT is a square positive matrix.
TP_j <- function(CONFUSION_IMAT, J_I) {
    CONFUSION_IMAT[J_I, J_I]
}

#' Number of objects predicted to belong to the wrong class (not \eqn{c_j})
#' Preconditions: CONFUSION_IMAT is a square positive matrix.
FN_j <- function(CONFUSION_IMAT, J_I) {
    sum(CONFUSION_IMAT[, J_I]) - CONFUSION_IMAT[J_I, J_I]
}

#' Actual positives
AP_j <- function(CONFUSION_IMAT, J_I) {
    TP_j(CONFUSION_IMAT, J_I) + FN_j(CONFUSION_IMAT, J_I)
}

#' Recall (sensitivity)
#'
#' Preconditions: AP_J_I > 0
ρ_j <- function(CONFUSION_IMAT, J_I) {
    # TODO: use lazy initialization
    AP_J_I <- AP_j(CONFUSION_IMAT, J_I)
    stopifnot(AP_J_I > 0)
    TP_j(CONFUSION_IMAT, J_I) / AP_J_I
}

ξ_j <- function(CONFUSION_IMAT, J_I) {
    # TODO: use lazy initialization
    # TODO: use ifelse()?
    AP_J_I <- AP_j(CONFUSION_IMAT, J_I)
    stopifnot(AP_J_I >= 0)
    if (AP_J_I > 0) 1 else if (AP_J_I == 0) 0
}

#' To calculate the binary K measure for \eqn{n-}class label where \eqn{n > 1}
#'
#' Sebastiani (2015) calls this task single-label classification (SLC).
#' Preconditions: CONFUSION_IMAT's \eqn{n = m}
#' @export
K_SLC <- function(CONFUSION_IMAT) {
    N_CLASSES_I <- ncol(CONFUSION_IMAT)
    stopifnot(N_CLASSES_I == nrow(CONFUSION_IMAT))

    CLASSES_IVEC <- seq_len(N_CLASSES_I)
    MIDDLE_TERM_NUMERATOR_D <-
        sum(vapply(CLASSES_IVEC,
            function(J_I) ξ_j(CONFUSION_IMAT, J_I) * ρ_j(CONFUSION_IMAT, J_I),
            FUN.VALUE=numeric(length=1L)))
    MIDDLE_TERM_DENOMINATOR_D <-
        sum(vapply(CLASSES_IVEC,
            ξ_j,
            CONFUSION_IMAT=CONFUSION_IMAT,
            FUN.VALUE=numeric(length=1L)))

    return((N_CLASSES_I / (N_CLASSES_I - 1)) *
        (MIDDLE_TERM_NUMERATOR_D / MIDDLE_TERM_DENOMINATOR_D) -
        (1 / (N_CLASSES_I - 1)))
}

#' This function is purely technical. \pkg{h2o} does not store a confusion matrix for 2-class tasks (\code{H2OBinomialModel}) in the classifiers it outputs. This creates one based on the simple measures at the \eqn{F_1}-optimal threshold, or extracts it in case of \code{H2OMultinomialModel}.
#' Preconditions: currently implemented for two or three-class tasks only.
#' @export
get_confusion_matrix_of_model <- function(MODEL) {
    if (inherits(MODEL, 'H2OBinomialModel')) {
        ## Get F1-optimal threshold.
        MAX_F1_THRESHOLD_INDEX_I <-
            MODEL@model$cross_validation_metrics@metrics$max_criteria_and_metric_scores[1L, 'idx']
        ## Get measures at optimal threshold.
        MEASURES_HDF <-
            MODEL@model$cross_validation_metrics@metrics$thresholds_and_metric_scores[MAX_F1_THRESHOLD_INDEX_I + 1,]

        CONFUSION_IMAT <-
            matrix(c(
                MEASURES_HDF$tps,
                MEASURES_HDF$fns,
                MEASURES_HDF$fps,
                MEASURES_HDF$tns), nrow=2L, ncol=2L)
    } else if (inherits(MODEL, 'H2OMultinomialModel')) {
        ## Assumes a 3-class task. Just a quick and dirty, minimalistic implementation here.
        CONFUSION_IMAT <-
            t(as.matrix(MODEL@model$cross_validation_metrics@metrics$cm$table[1:3, 1:3]))
    }
}

### Tests

## The following tests suggest that the K measure satisfies Sebastiani's axioms (2015).
# IMB axiom
PERFECT_IMAT <- matrix(data=c(10, 0, 0, 10), nrow=2L, ncol=2L)
PERVERSE_IMAT <- matrix(data=c(0, 10, 10, 0), nrow=2L, ncol=2L)
stopifnot(K(PERFECT_IMAT) > K(PERVERSE_IMAT))
stopifnot(K_SLC(PERFECT_IMAT) > K_SLC(PERVERSE_IMAT))

# FIX axiom
stopifnot((K(PERFECT_IMAT) == K(PERFECT_IMAT * 2)) == K_SLC(PERFECT_IMAT))
stopifnot((K(PERVERSE_IMAT) == K(PERVERSE_IMAT * 2)) == K_SLC(PERFECT_IMAT))


# TODO: test all axioms in final implementation.
# TODO: add concrete test cases.
# stopifnot(K(CONFUSION_IMAT) == K_SLC(CONFUSION_IMAT))