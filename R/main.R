#' @export
main_djdr_analysis <- function(
    ANNOTATIONS,
    ARC_CATEGORY_TABLES_DIR_PATH,
    FEATURES_EXTRACTION_PARAMETERS,
    JOB_NAME_STR,
    DESIGN_MATRIX_FACTORIZATION_RANK_FRACTION_D,
    SEED_I,
    SCORING_STR,
    SRF_N_I,
    STRCT_I_I,
    STRCT_M_I,
    STRCT_N_I) {

    ###
    ##  Feature extraction
    ###

    DESIGN_MATRIX_LST <-
        features_extract(
            ANNOTATIONS=ANNOTATIONS,
            ARC_CATEGORY_TABLES_DIR_PATH=ARC_CATEGORY_TABLES_DIR_PATH,
            FEATURES_EXTRACTION_PARAMETERS=FEATURES_EXTRACTION_PARAMETERS,
            DESIGN_MATRIX_FACTORIZATION_RANK_FRACTION_D=DESIGN_MATRIX_FACTORIZATION_RANK_FRACTION_D,
            SCORING_STR=SCORING_STR,
            SRF_N_I=SRF_N_I,
            STRCT_I_I=STRCT_I_I,
            STRCT_M_I=STRCT_M_I,
            STRCT_N_I=STRCT_N_I)
    ###
    ##  Analysis
    ###

    ANALYSIS_PARAMETERS <-
        AnalysisParameters(
            ANNOTATIONS=ANNOTATIONS,
            DESIGN_MATRIX_LST=DESIGN_MATRIX_LST,
            K_I=10L,
            TIMES_I=3L)

    LABELS_COLNAMES_CVEC <-
        if (stringi::stri_startswith_fixed(JOB_NAME_STR, 'conclusion_whole'))
            base::c(
                'conclusion_type_is_granted_I',
                'conclusion_type_is_denied_I',
                'conclusion_type_is_held_insufficient_I')
        else base::c('conclusion_type_I')

    FEATURE_REPRESENTATION_NAMES_CVEC <-
        base::names(
            ANALYSIS_PARAMETERS@DESIGN_MATRIX_LST)

    ## STRCT, SRF, and both:
    COMPARISON_TYPES_IMAT_LST <-
        base::lapply(
            base::seq_along(FEATURE_REPRESENTATION_NAMES_CVEC),
            FUN=utils::combn,
            x=FEATURE_REPRESENTATION_NAMES_CVEC)
    Y_IVEC_LST <-
        base::lapply(
            LABELS_COLNAMES_CVEC,
            function(LABEL_COLNAME_STR)
                base::unique(
                    ANALYSIS_PARAMETERS@ANNOTATIONS@DT,
                    # [, .SD, .SDcols=base::c('object_ID', LABEL_COLNAME_STR)],
                    by='object_ID')[[LABEL_COLNAME_STR]]
        )
    base::names(Y_IVEC_LST) <- LABELS_COLNAMES_CVEC

    prediction_main(
        ANALYSIS_PARAMETERS=ANALYSIS_PARAMETERS,
        COMPARISON_TYPES_IMAT_LST=COMPARISON_TYPES_IMAT_LST,
        FEATURES_EXTRACTION_PARAMETERS=FEATURES_EXTRACTION_PARAMETERS,
        JOB_NAME_STR=JOB_NAME_STR,
        LABELS_COLNAMES_CVEC=LABELS_COLNAMES_CVEC,
        SEED_I=SEED_I,
        Y_IVEC_LST=Y_IVEC_LST)

    ## Save image of workspace for debugging, reproducibility and documentation
    IMAGE_FILE_PATH_STR <-
        base::file.path(FEATURES_EXTRACTION_PARAMETERS@OUTPUT_DIR_PATH, '.Rdata')
    base::save.image(file=IMAGE_FILE_PATH_STR)
    return(IMAGE_FILE_PATH_STR)
}

## Total number of models estimated:
##
## 36 parameter combinations (conditions)
### k = {0.05, 0.1} × SCORING_STR = {'boolean', 'Pr'} × SRF_N_I = {1,2,3} × 1 [STRCT_I_I = 3] × STRCT_N_I = {1,2,3} × 1 [STRCT_N_I = STRCT_M_I]
### 2 * 2 * 3 * 1 * 3 * 1
## 3 data subsets -> 5 tasks
#### body: 1 label
#### conclusion_whole: 3 labels
#### conclusion_by_type: 1 label)
## 3 runs
## 3 learners =
## 36 * 3 * 5 * 3 = 1620

#' @export
main_analysis <- function() {
    ARC_CATEGORY_TABLES_DIR_PATH <-
        feat::FilesystemPath(
            base::Sys.getenv('ARC_CATEGORY_TABLES_DIR_PATH'))
    # TODO: create directory in constructor
    OUTPUT_DIR_PATH_STR <-
        base::Sys.getenv('OUTPUT_DIR_PATH_STR')

    SEED_I <- 1986L

    ## Generate table of all conditions/configurations
    ## (parameter value combinations) to be evaluated
    CONDITIONS_DF <-
        base::expand.grid(
            DESIGN_MATRIX_FACTORIZATION_RANK_FRACTION_D=base::c(0.05, 0.1),
            SCORING_STR=base::c('boolean', 'Pr'),
            SRF_N_I=1L:3L,
            STRCT_I_I=3L,
            STRCT_N_I=1L:3L,
            stringsAsFactors=FALSE,
            KEEP.OUT.ATTRS=FALSE)
    ## Restrict trail length to maximum N-gram length.
    ## A shorter trail length makes no sense, a longer one means unnecessary computation.
    CONDITIONS_DF[['STRCT_M_I']] <-
        CONDITIONS_DF[['STRCT_N_I']]

    ANNOTATIONS_TABLE_FILE_PATH <-
        feat::FilesystemPath(
            base::Sys.getenv('ANNOTATIONS_TABLE_FILE_PATH'))
    KEPT_ANNOTATIONS_DT_COLUMN_NAMES_CVEC <-
        base::c(
            'object_ID',
            'relative_file_path',
            'court_I',
            'year_I',
            'case_I',
            # 'segment_type_I',
            'conclusion_type_I',
            'conclusion_type_is_granted_I',
            'conclusion_type_is_denied_I',
            'conclusion_type_is_held_insufficient_I')
    # if (base::Sys.getenv('JOB_NAME') == '') {
    #     Djdr_process(
    #         ANNOTATIONS_TABLE_FILE_PATH_STR=ANNOTATIONS_TABLE_FILE_PATH_STR)
    # }
    ANNOTATIONS <-
        feat::metadata_read_annotations_table(
            ANNOTATIONS_TABLE_FILE_PATH_STR=ANNOTATIONS_TABLE_FILE_PATH,
            KEPT_ANNOTATIONS_DT_COLUMN_NAMES_CVEC=KEPT_ANNOTATIONS_DT_COLUMN_NAMES_CVEC)
    # DESCRIPTIVES_LST <-
    #     descriptives_produce(ANNOTATIONS@DT=ANNOTATIONs@DT)
    DOCUMENTS_DIR_PATH_STR <-
         base::Sys.getenv('DOCUMENTS_DIR_PATH_STR')
    DOCUMENTS_DIR_PATH <-
        feat::FilesystemPath(DOCUMENTS_DIR_PATH_STR)

    CONDITIONS_DF_FILE_PATH <-
        base::file.path(
            OUTPUT_DIR_PATH_STR,
            'conditions.csv')
    if (!base::file.exists(CONDITIONS_DF_FILE_PATH)) {
        utils::write.csv(
            CONDITIONS_DF,
            file=CONDITIONS_DF_FILE_PATH,
            row.names=FALSE)
    }
    for (CONDITIONS_ROW_INDEX_I in base::seq_len(base::nrow(CONDITIONS_DF))) {
        CONDITIONS_LST <-
            base::as.list(CONDITIONS_DF[CONDITIONS_ROW_INDEX_I,])
        JOB_NAME_STR <-
            stringi::stri_join(
                base::Sys.getenv('JOB_NAME'),
                stringi::stri_join(
                    base::names(CONDITIONS_LST),
                    CONDITIONS_LST,
                    sep="=",
                    collapse=' '),
                sep=': ')
        JOB_OUTPUT_DIR_PATH_STR <-
            base::file.path(
                OUTPUT_DIR_PATH_STR,
                JOB_NAME_STR)
        if (!base::dir.exists(JOB_OUTPUT_DIR_PATH_STR)) {
            base::dir.create(JOB_OUTPUT_DIR_PATH_STR)
        }
        FEATURES_EXTRACTION_PARAMETERS <-
            feat::FeaturesExtractionParameters(
                # TODO: redundant parameter?
                ANNOTATIONS_TABLE_FILE_PATH=ANNOTATIONS_TABLE_FILE_PATH,
                DOCUMENTS_DIR_PATH=DOCUMENTS_DIR_PATH,
                # TODO: redundant parameter?
                JOB_NAME_STR=JOB_NAME_STR,
                OUTPUT_DIR_PATH=feat::FilesystemPath(JOB_OUTPUT_DIR_PATH_STR))
        ARGS_LST <-
            base::c(ANNOTATIONS=ANNOTATIONS,
                    ARC_CATEGORY_TABLES_DIR_PATH=ARC_CATEGORY_TABLES_DIR_PATH,
                    FEATURES_EXTRACTION_PARAMETERS=FEATURES_EXTRACTION_PARAMETERS,
                    JOB_NAME_STR=JOB_NAME_STR,
                    SEED_I=SEED_I,
                    CONDITIONS_DF[CONDITIONS_ROW_INDEX_I,])
        base::do.call(
            main_djdr_analysis,
            args=ARGS_LST)
    }
}
