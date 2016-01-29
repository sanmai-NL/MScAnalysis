#' @export
evaluation_all_models <- function(OUTPUT_DIR_PATH) {
    TEMP_DIR_PATH <- tempdir()
    setwd(TEMP_DIR_PATH)
    futile.logger::flog.info("Writing evaluation report to '%s' ...", TEMP_DIR_PATH)

    MODELS_REL_FILES_PATHS_CVEC <-
        base::list.files(
            OUTPUT_DIR_PATH,
            pattern='^model\\.rds$',
            recursive=TRUE)

    MODELS_LST <-
        parallel::mclapply(
            base::file.path(OUTPUT_DIR_PATH, MODELS_REL_FILES_PATHS_CVEC),
            base::readRDS)

    stopifnot(length(MODELS_REL_FILES_PATHS_CVEC) > 0)

    ###
    ## Collect classification measure scores
    ###

    MODELS_K_SCORES_LST <-
        base::vapply(
            MODELS_LST,
            function(MODEL) K_SLC(get_confusion_matrix_of_model(MODEL)),
            FUN.VALUE=base::numeric(length=1L))

    RESULTS_CMAT <-
        stringi::stri_split_fixed(
            str=MODELS_REL_FILES_PATHS_CVEC,
            pattern='/',
            simplify=TRUE)
    CONDITION1_CMAT <-
        stringi::stri_split_fixed(
            RESULTS_CMAT[, 1L],
            pattern=base::c(' '),
            simplify=TRUE)
    CONDITION3_CMAT <-
        base::matrix(
            stringi::stri_split_fixed(
                CONDITION1_CMAT,
                pattern=base::c('='),
                simplify=TRUE),
            nrow=nrow(CONDITION1_CMAT))
    CONDITION2_CMAT <-
        stringi::stri_split_fixed(
            RESULTS_CMAT[, 2L],
            pattern='.',
            simplify=TRUE)
    FINAL_RESULTS_CMAT <- base::cbind(
        CONDITION3_CMAT[, 1L],
        CONDITION2_CMAT[, base::c(1L, 2L, 5L)],
        RESULTS_CMAT[, base::c(-1L, -2L, -4L)],
        CONDITION3_CMAT[, 9L:14L]
        )
    base::colnames(FINAL_RESULTS_CMAT) <-
        base::c('subset', 'feature_set', 'task', 'learner', 'run', 'k', 'feature_scoring',
            'SRF_N', 'STRCT_I', 'STRCT_N', 'STRCT_M')
    RESULTS_D_F <-
        dplyr::mutate(
            dplyr::as_data_frame(
                base::cbind(
                    base::as.data.frame(FINAL_RESULTS_CMAT),
                    K_score=MODELS_K_SCORES_LST,
                    file_path=MODELS_REL_FILES_PATHS_CVEC)))

    SRF_BODY_D_F <-
        dplyr::filter(
            RESULTS_D_F,
            subset == 'body:',
            feature_set == 'SRF')

    STRCT_BODY_D_F <-
        dplyr::filter(
            RESULTS_D_F,
            subset == 'body:',
            feature_set == 'STRCT')

    SRF_AND_STRCT_BODY_D_F <-
        dplyr::filter(
            RESULTS_D_F,
            subset == 'body:',
            feature_set == 'SRF&STRCT')

    SRF_CONCLUSION_BY_TYPE_D_F <-
        dplyr::filter(
            RESULTS_D_F,
            subset == 'conclusion_by_type:',
            feature_set == 'SRF')

    STRCT_CONCLUSION_BY_TYPE_D_F <-
        dplyr::filter(
            RESULTS_D_F,
            subset == 'conclusion_by_type:',
            feature_set == 'STRCT')

    SRF_AND_STRCT_CONCLUSION_BY_TYPE_D_F <-
        dplyr::filter(
            RESULTS_D_F,
            subset == 'conclusion_by_type:',
            feature_set == 'SRF&STRCT')

    SRF_CONCLUSION_WHOLE_D_F <-
        dplyr::filter(
            RESULTS_D_F,
            subset == 'conclusion_whole:',
            feature_set == 'SRF')

    STRCT_CONCLUSION_WHOLE_D_F <-
        dplyr::filter(
            RESULTS_D_F,
            subset == 'conclusion_whole:',
            feature_set == 'STRCT')

    SRF_AND_STRCT_CONCLUSION_WHOLE_D_F <-
        dplyr::filter(
            RESULTS_D_F,
            subset == 'conclusion_whole:',
            feature_set == 'SRF&STRCT')
    ##

    SUBSETS_CVEC <-
        c('body', 'conclusion_by_type',
        'conclusion_whole', 'conclusion_whole', 'conclusion_whole')

    TASKS_CVEC <-
        c('conclusion_type_I',
        'conclusion_type_I',
        'conclusion_type_is_denied_I',
        'conclusion_type_is_held_insufficient_I',
        'conclusion_type_is_granted_I')

    TASKS_LST <-
        base::list(
            base::list(
                SRF=SRF_BODY_D_F,
                STRCT=STRCT_BODY_D_F,
                SRF_AND_STRCT=SRF_AND_STRCT_BODY_D_F),
            base::list(
                SRF=SRF_CONCLUSION_BY_TYPE_D_F,
                STRCT=STRCT_CONCLUSION_BY_TYPE_D_F,
                SRF_AND_STRCT=SRF_AND_STRCT_CONCLUSION_BY_TYPE_D_F),
            base::list(
                SRF=SRF_CONCLUSION_WHOLE_D_F,
                STRCT=STRCT_CONCLUSION_WHOLE_D_F,
                SRF_AND_STRCT=SRF_AND_STRCT_CONCLUSION_WHOLE_D_F),
            base::list(
                SRF=SRF_CONCLUSION_WHOLE_D_F,
                STRCT=STRCT_CONCLUSION_WHOLE_D_F,
                SRF_AND_STRCT=SRF_AND_STRCT_CONCLUSION_WHOLE_D_F),
            base::list(
                SRF=SRF_CONCLUSION_WHOLE_D_F,
                STRCT=STRCT_CONCLUSION_WHOLE_D_F,
                SRF_AND_STRCT=SRF_AND_STRCT_CONCLUSION_WHOLE_D_F))

    EFFECTIVENESS_OVERVIEW_STR <- 'Subset\tTask\tFeature set\tM\tSD\n'
    for (TASK_LST_INDEX_I in seq_along(TASKS_LST)) {
        TASK_LST <- TASKS_LST[[TASK_LST_INDEX_I]]
        TASK_STR <- TASKS_CVEC[TASK_LST_INDEX_I]
        SUBSET_STR <- SUBSETS_CVEC[TASK_LST_INDEX_I]
        # TODO: (efficiency)
        FEATURE_SET_LST <-
            base::lapply(
                TASK_LST,
                function(D_F) dplyr::filter(D_F, task == TASK_STR)[['K_score']])
        names(FEATURE_SET_LST) <- NULL
        SCORE_BEANPLOT_SVG_FILE_PATH_STR <-
            stringi::stri_join(TASK_STR, SUBSET_STR, 'K_score', 'svg', sep='.')
        svg(file=SCORE_BEANPLOT_SVG_FILE_PATH_STR, antialias='none', width=15, height=15)
        base::do.call(
            beanplot::beanplot,
            base::list(FEATURE_SET_LST,
                col=c('#8dd3c7','#ffffb3','#bebada'),
                main=stringi::stri_join("Classifiers' K scores (per CV run)"),
                names=c('SRF', 'STRCT', 'SRF & STRCT'),
                what=c(1, 1, 1, 1),
                ylab='K score',
                add=FALSE))
        dev.off()

        for (FEATURE_REPRESENTATION_STR in names(TASK_LST)) {
            TASK_FEATURE_SET_D_F <-
                dplyr::filter(TASK_LST[[FEATURE_REPRESENTATION_STR]],
                    task == TASK_STR)
            # TODO: do not average across runs?
            CONFIGURATION_RANKING_D_F <-
                dplyr::arrange(
                    dplyr::ungroup(
                        dplyr::summarize(
                            dplyr::group_by(
                                dplyr::select(
                                    TASK_FEATURE_SET_D_F,
                                    run, learner, k, feature_scoring,
                                    SRF_N, STRCT_I, STRCT_N, STRCT_M,
                                    K_score),
                                # ~setdiff(colnames(TASK_FEATURE_SET_D_F), 'run')),
                                learner, k, feature_scoring, SRF_N, STRCT_I, STRCT_N, STRCT_M),
                            mean_K_score=mean(K_score))),
                    desc(mean_K_score))
            utils::write.csv(
                CONFIGURATION_RANKING_D_F,
                file=stringi::stri_join(TASK_STR, SUBSET_STR, FEATURE_REPRESENTATION_STR,
                    'K_score', 'csv', sep='.'),
                row.names=FALSE)
            EFFECTIVENESS_OVERVIEW_STR <-
                stringi::stri_join(
                    EFFECTIVENESS_OVERVIEW_STR,
                    sprintf('%s\t%s\t%s\t%.2f\t%.2f\n',
                        SUBSET_STR,
                        TASK_STR, FEATURE_REPRESENTATION_STR,
                        mean(CONFIGURATION_RANKING_D_F$mean_K_score),
                        sd(CONFIGURATION_RANKING_D_F$mean_K_score)))
            # message(EFFECTIVENESS_OVERVIEW_STR)
        }
    }
    cat(file='effectiveness_overview.tsv', EFFECTIVENESS_OVERVIEW_STR)

    utils::write.csv(
        RESULTS_D_F,
        file='results.csv',
        row.names=FALSE)

    statistical_analysis(
        RESULTS_DF=data.frame(RESULTS_D_F),
        OUTPUT_DIR_PATH=TEMP_DIR_PATH)

    REPORT_DIR_PATH_STR <- file.path(OUTPUT_DIR_PATH, 'report')
    futile.logger::flog.info(
        "Moving evaluation report to '%s' ...",
        REPORT_DIR_PATH_STR)

    setwd(OUTPUT_DIR_PATH)
    move_or_copy(
        from=TEMP_DIR_PATH,
        to=REPORT_DIR_PATH_STR,
        to_base=OUTPUT_DIR_PATH,
        recursive=TRUE)
}

evaluate_results <- function(RESULTS_DF) {
    RESULTS_DF$run <- as.factor(RESULTS_DF$run)
    RESULTS_DF$SRF_N <- as.factor(RESULTS_DF$SRF_N)
    RESULTS_DF$STRCT_N <- as.factor(RESULTS_DF$STRCT_N)
    RESULTS_DF$k <- as.factor(RESULTS_DF$k)
    RESULTS_DF$STRCT_M <- NULL
    RESULTS_DF$STRCT_I <- NULL
    RESULTS_DF$file_path <- NULL

    STRCTK <- subset(RESULTS_DF, feature_set=='STRCT')$K_score
    SRFK <- subset(RESULTS_DF, feature_set=='SRF')$K_score
    STRCTANDSRFK <- subset(RESULTS_DF, feature_set=='SRF&STRCT')$K_score

    return(RESULTS_DF)
}

statistical_analysis <- function(RESULTS_DF, OUTPUT_DIR_PATH) {
    ## Estimate linear regression model of configurations on K score
    CLEANED_UP_RESULTS_DF <- get_data(RESULTS_DF)
    BF_ALL_F <- formula(K_score ~.)
    BF_SPECIFIC_F <-
        formula(
            K_score ~ subset * task + subset * learner + feature_set * learner +
            task + learner + run + k + feature_scoring + SRF_N + STRCT_N + feature_set * SRF_N + feature_set * STRCT_N)
    # RESULTS_LM <- glm(BF_SPECIFIC_F, data=CLEANED_UP_RESULTS_DF)
    RESULTS_LM <- lm(BF_SPECIFIC_F, data=CLEANED_UP_RESULTS_DF)

    svg(file.path(OUTPUT_DIR_PATH, 'regression_residuals_dist.svg'))
    qqPlot(RESULTS_LM, main="QQ Plot")
    #@ Plot distribution of studentized residuals
    library(MASS)
    sresid <- studres(RESULTS_LM)
    hist(sresid, freq=FALSE, main="Distribution of Studentized residuals", xlab='Residuals of regression model')
    xfit <- seq(min(sresid), max(sresid), length=40)
    yfit <- dnorm(xfit)
    lines(xfit, yfit)
    dev.off()
    ## Produce regression model table
    library(xtable)
    idx <- order(abs(coef(summary(RESULTS_LM))[,1L]), decreasing=TRUE)
    RESULTS_LM_TABLE <- xtable(coef(summary(RESULTS_LM))[idx,])
    RESULTS_LM_TABLE_STR <- capture.output(print(RESULTS_LM_TABLE, type='html'))
    cat(file=base::file.path(OUTPUT_DIR_PATH, 'results_lm_table.html'), RESULTS_LM_TABLE_STR)

    ## Test hypothesis
    binom.test(
        length(which(STRCTANDSRFK < SRFK)),
        length(SRFK),
        p=0.5,
        alternative=base::c("greater"),
        conf.level=0.95)
}
