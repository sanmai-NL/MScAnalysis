move_or_copy <- function(from, to, to_base, recursive)
     base::tryCatch(
        { base::file.rename(
            from=from,
            to=to) },
        warning=function(CONDITION) {
            base::tryCatch({
                base::file.copy(
                    from=from,
                    to=to_base,
                    recursive=recursive)
                SOURCE_FILE_PATH_STR <-
                    base::file.path(
                        to_base,
                        base::basename(from))
                DESTINATION_FILE_PATH_STR <-
                    base::file.path(
                        to_base,
                        base::basename(to))
                base::file.rename(
                    from=SOURCE_FILE_PATH_STR,
                    to=DESTINATION_FILE_PATH_STR) },
                warning=base::stop,
                error=base::stop)
            base::unlink(from, recursive=recursive) },
        error=base::stop)

#' @export
#' @import h2o
prediction_perform <- function(
    FOLD_IVEC,
    FOLD_COLUMN_NAME_STR='fold',
    SEED_I,
    DESIGN_MAT_FILE_PATH,
    TIME_I,
    Y_COLUMN_NAME_STR='C1') {

    TRAINING_HFRAME <-
        h2o::h2o.importFile(path=DESIGN_MAT_FILE_PATH)
    X_COLUMN_NAMES_CVEC <-
        base::setdiff(base::names(TRAINING_HFRAME), Y_COLUMN_NAME_STR)
    Y_HFAC <-
        h2o::as.factor(TRAINING_HFRAME[,Y_COLUMN_NAME_STR])
    TRAINING_HFRAME[,Y_COLUMN_NAME_STR] <- Y_HFAC
    ## TODO: fix upstream: H2O does not accept integer vectors
    NFOLD_HDVEC <- h2o::as.h2o(base::as.numeric(FOLD_IVEC))
    TRAINING_HFRAME[,FOLD_COLUMN_NAME_STR] <- NFOLD_HDVEC
    N_Y_LEVELS_I <- base::nrow(base::summary(Y_HFAC))
    FAMILY_STR <- if (N_Y_LEVELS_I == 2L) 'binomial' else 'multinomial'
    DISTRIBUTION_STR <- if (N_Y_LEVELS_I == 2L) 'bernoulli' else 'multinomial'
    futile.logger::flog.debug(
        'nlevels: %d, so family is %s',
        N_Y_LEVELS_I,
        FAMILY_STR)

    GENERAL_LEARNER_PARAMETERS_LST <-
        base::list(
            fold_column=FOLD_COLUMN_NAME_STR,
            keep_cross_validation_predictions=TRUE,
            x=X_COLUMN_NAMES_CVEC,
            y=Y_COLUMN_NAME_STR,
            training_frame=TRAINING_HFRAME)
    SPECIAL_LEARNER_PARAMETERS_LST <-
        base::list(
            ## TODO: fix upstream: chrashes
            # 'h2o.glm'=base::list(
            #     # solver='L_BFGS',
            #     family=FAMILY_STR),
            ## TODO: only supports binomial?
            #     standardize=FALSE,
            'h2o.randomForest'=base::list(
                # balance.classes=TRUE
                seed=SEED_I
                # TODO: (reproducibility) 'only reproducible when running single threaded'. So seems not to support a parameter:
                # single_node=TRUE, # TODO: reproducibility
                ),
            'h2o.gbm'=base::list(
                seed=SEED_I,
                distribution=DISTRIBUTION_STR),
            'h2o.deeplearning'=base::list(
                seed=SEED_I,
                # single_node=TRUE, # TODO: (reproducibility)
                reproducible=TRUE,
                # variable_importances=TRUE
                diagnostics=TRUE,
                # sparse=TRUE,
                # By default, H2O Deep Learning uses an adaptive learning rate (ADADELTA) for its stochastic gradient descent optimization.
                distribution=DISTRIBUTION_STR)
        )

    MODEL_IDS_LST <-
        base::lapply(
            base::names(SPECIAL_LEARNER_PARAMETERS_LST),
            function(LEARNER_FUN_STR) {
                DIR_PATH_STR <-
                    base::file.path(
                        stringi::stri_join(
                            DESIGN_MAT_FILE_PATH,
                            LEARNER_FUN_STR,
                            sep='.'),
                        TIME_I)
                if (!base::dir.exists(DIR_PATH_STR)) {
                    base::dir.create(DIR_PATH_STR, recursive=TRUE)
                }

                DIR_PATH_STR <-
                    base::normalizePath(DIR_PATH_STR, mustWork=TRUE)
                EFFECTIVENESS_FILE_PATH_STR <-
                    base::file.path(
                        DIR_PATH_STR,
                        'effectiveness.txt')
                # browser(expr=LEARNER_FUN_STR == 'h2o.deeplearning')
                if (!base::file.exists(EFFECTIVENESS_FILE_PATH_STR)) {
                    MODEL_DIR_PATH_STR <-
                        base::file.path(
                            DIR_PATH_STR,
                            'model')
                    ## Assume no other model-related files exist of the effectiveness text file does not exist.
                    MODEL <-
                        base::suppressWarnings(
                            base::do.call(
                                what=LEARNER_FUN_STR,
                                args=base::c(
                                    GENERAL_LEARNER_PARAMETERS_LST,
                                    SPECIAL_LEARNER_PARAMETERS_LST[[LEARNER_FUN_STR]]),
                                envir=base::getNamespace('h2o')))
                    MODEL_TEMP_FILE_PATH <-
                        h2o::h2o.saveModel(MODEL, path=base::tempdir())
                    move_or_copy(
                        from=MODEL_TEMP_FILE_PATH,
                        to=MODEL_DIR_PATH_STR,
                        to_base=DIR_PATH_STR,
                        recursive=FALSE)
                    base::saveRDS(
                        MODEL,
                        file=base::file.path(
                            DIR_PATH_STR,
                            'model.rds'))
                    utils::capture.output(
                        { base::print(MODEL) },
                        file=EFFECTIVENESS_FILE_PATH_STR)

                    for (PRED_HDF_KEY_INDEX_I in base::seq_along(MODEL@model$cross_validation_predictions)) {
                        PRED_HDF <-
                            h2o::h2o.getFrame(
                                id=MODEL@model$cross_validation_predictions[[PRED_HDF_KEY_INDEX_I]]$name)
                        PRED_HDF_FILE_PATH_STR <-
                            base::file.path(
                                DIR_PATH_STR,
                                base::sprintf(
                                    'fold_%d_predictions.csv',
                                    PRED_HDF_KEY_INDEX_I))
                        if (!base::file.exists(PRED_HDF_FILE_PATH_STR)) {
                            TEMP_PRED_HDF_FILE_PATH_STR <-
                                base::tempfile()
                            h2o::h2o.exportFile(
                                PRED_HDF,
                                path=TEMP_PRED_HDF_FILE_PATH_STR,
                                force=TRUE)
                            move_or_copy(
                                from=TEMP_PRED_HDF_FILE_PATH_STR,
                                to=PRED_HDF_FILE_PATH_STR,
                                to_base=DIR_PATH_STR,
                                recursive=FALSE)
                        }
                    }
                    CONFUSION_MATRIX_FILE_PATH_STR <-
                        # TODO: leave out
                        base::file.path(
                            DIR_PATH_STR,
                            'F1_optimal_CV_confusion_matrix.csv')
                    if (!base::file.exists(CONFUSION_MATRIX_FILE_PATH_STR)) {
                        utils::write.csv(
                            base::as.data.frame(
                                MODEL@model$cross_validation_metrics@metrics$cm$table),
                            file=CONFUSION_MATRIX_FILE_PATH_STR)
                    }
                    MODEL@model_id
                } else {
                    futile.logger::flog.info(
                        "Keeping previous results in '%s'. ",
                        DIR_PATH_STR)
                    base::character()
                }
            })
        # h2o::h2o.rm(ids=h2o.ls()$key)
        h2o::h2o.removeAll()
}

prediction_main <- function(
    ANALYSIS_PARAMETERS,
    COMPARISON_TYPES_IMAT_LST,
    FEATURES_EXTRACTION_PARAMETERS,
    JOB_NAME_STR,
    LABELS_COLNAMES_CVEC,
    SEED_I,
    Y_IVEC_LST) {
    futile.logger::flog.info('Starting prediction for ‘%s’. ', JOB_NAME_STR)

    H2OLOCAL <- h2o::h2o.init(nthreads=-1L) # max_mem_size='60g'

    TASK_INDEX_I <- 0L
    ## TODO: fix N_TASKS_I?
    N_TASKS_I <-
        base::sum(
            base::vapply(
                COMPARISON_TYPES_IMAT_LST,
                base::ncol,
                base::integer(length=1L))) *
        base::length(LABELS_COLNAMES_CVEC) *
        ANALYSIS_PARAMETERS@TIMES_I

    for (COMPARISON_TYPE_IMAT_INDEX in base::seq_along(COMPARISON_TYPES_IMAT_LST)) {
        COMPARISON_TYPE_IMAT <-
            COMPARISON_TYPES_IMAT_LST[[COMPARISON_TYPE_IMAT_INDEX]]
        for (COLUMN_INDEX_I in base::seq_len(
                base::ncol(COMPARISON_TYPE_IMAT))) {
            FEATURE_REPRESENTATIONS_CVEC <-
                COMPARISON_TYPE_IMAT[,COLUMN_INDEX_I]
            COMPARISON_NAME_STR <-
                stringi::stri_join(
                    FEATURE_REPRESENTATIONS_CVEC,
                    collapse='&')
            LOWER_RANK_APPROXIMATION_DESIGN_DMAT_LST <-
                base::lapply(
                    ANALYSIS_PARAMETERS@DESIGN_MATRIX_LST[FEATURE_REPRESENTATIONS_CVEC],
                    function(DESIGN_MATRIX) DESIGN_MATRIX@LOWER_RANK_APPROXIMATION_DMAT)
            AGGREGATED_DESIGN_DMAT <-
                base::do.call(
                    base::cbind,
                    LOWER_RANK_APPROXIMATION_DESIGN_DMAT_LST)

            for (LABEL_COLNAME_STR in LABELS_COLNAMES_CVEC) {
                DESIGN_DMAT_FILE_PATH <-
                    if (base::inherits(AGGREGATED_DESIGN_DMAT, 'denseMatrix')) {
                        base::file.path(
                            FEATURES_EXTRACTION_PARAMETERS@OUTPUT_DIR_PATH,
                            stringi::stri_join(
                                COMPARISON_NAME_STR,
                                LABEL_COLNAME_STR,
                                'csv',
                                sep='.'))
                    } else {
                        ## Store in SVMlight format, so that H2O can read it as sparse data.
                        base::file.path(
                            FEATURES_EXTRACTION_PARAMETERS@OUTPUT_DIR_PATH,
                            stringi::stri_join(
                                COMPARISON_NAME_STR,
                                LABEL_COLNAME_STR,
                                'SVMlight',
                                sep='.'))
                    }
                if (!base::file.exists(DESIGN_DMAT_FILE_PATH)) {
                    if (base::inherits(AGGREGATED_DESIGN_DMAT, 'denseMatrix')) {
                        utils::write.csv(
                            base::cbind(
                                data.table::as.data.table(
                                    methods::as(
                                        AGGREGATED_DESIGN_DMAT,
                                        'matrix')),
                                C1=Y_IVEC_LST[[LABEL_COLNAME_STR]]),
                            file=DESIGN_DMAT_FILE_PATH,
                            row.names=FALSE,
                            fileEncoding='UTF-8')
                    } else {
                        ## e1071::write.matrix.csr will name y 'C1'.
                        e1071::write.matrix.csr(
                            methods::as(
                                AGGREGATED_DESIGN_DMAT,
                                'matrix.csr'),
                            file=DESIGN_DMAT_FILE_PATH,
                            y=Y_IVEC_LST[[LABEL_COLNAME_STR]])
                    }

                    futile.logger::flog.info(
                        "Wrote design matrix to '%s'. ",
                        DESIGN_DMAT_FILE_PATH)
                }

                PARTITIONINGS <-
                    caret::createMultiFolds(
                        Y_IVEC_LST[[LABEL_COLNAME_STR]],
                        k=ANALYSIS_PARAMETERS@K_I,
                        times=ANALYSIS_PARAMETERS@TIMES_I)
                FOLDS_IMAT <-
                    base::vapply(
                        base::seq_len(ANALYSIS_PARAMETERS@TIMES_I) - 1L,
                        function(START_INDEX_I)
                            base::seq_len(ANALYSIS_PARAMETERS@K_I) + START_INDEX_I * ANALYSIS_PARAMETERS@K_I,
                        FUN.VALUE=base::integer(length=ANALYSIS_PARAMETERS@K_I))
                for (TIME_I in base::seq_len(base::ncol(FOLDS_IMAT))) {
                    ## Allocate fold number variable.
                    FOLD_IVEC <-
                        base::integer(length=base::length(Y_IVEC_LST[[LABEL_COLNAME_STR]]))
                    FOLD_VARIABLE_IVEC <- FOLDS_IMAT[,TIME_I]
                    # TODO: (efficiency)
                    for (FOLD_I in FOLD_VARIABLE_IVEC) {
                        FOLD_IVEC[-PARTITIONINGS[[FOLD_I]]] <- FOLD_I
                    }

                    TASK_INDEX_I <- TASK_INDEX_I + 1L
                    futile.logger::flog.info(
                        'Performing analysis %s run %d, job %d of %d: %s ... ',
                        JOB_NAME_STR,
                        TIME_I,
                        TASK_INDEX_I,
                        N_TASKS_I,
                        DESIGN_DMAT_FILE_PATH)
                    ## H2O chrashes intermittently and for vague reasons, while still being functional. The best strategy is to keep soldiering on ...
                    base::tryCatch(
                        { MScAnalysis::prediction_perform(
                            TIME_I=TIME_I,
                            FOLD_IVEC=FOLD_IVEC,
                            DESIGN_MAT_FILE_PATH=DESIGN_DMAT_FILE_PATH,
                            SEED_I=SEED_I) },
                        error=function(CONDITION)
                            base::warning(CONDITION))
                }
                ## Allegedly needed for H2O to remove out of scope objects from cache.
                base::gc()
            }
        }
    }
}