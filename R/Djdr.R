#' @import data.table
Djdr_process <- function(
    ANNOTATIONS_TABLE_FILE_PATH_STR) {
    ## Read annotation table
    KEPT_ANNOTATIONS_DT_COLUMN_NAMES_1_CVEC <-
        base::c('SHA256_value', 'document_name', 'sentence_ID', 'segment_type', 'conclusion_type')

    annotations_dt <-
        feat::metadata_read_annotations_table(
            ANNOTATIONS_TABLE_FILE_PATH_STR=ANNOTATIONS_TABLE_FILE_PATH_STR,
            KEPT_ANNOTATIONS_DT_COLUMN_NAMES_CVEC=KEPT_ANNOTATIONS_DT_COLUMN_NAMES_1_CVEC)@DT

    ## Find missing combinations of (document_name, segment_type)
    # MISSING_CASES_DT <-
    #     dplyr::filter(
    #         tidyr::complete(
    #             annotations_dt,
    #             document_name,
    #             segment_type),
    #         is.na(SHA256_value))

    # base::stopifnot(base::nrow(MISSING_CASES_DT) == 0L)

    ## Set info segments to NA
    ## Rows with NA are not interesting for this analysis.
    # data.table::setkeyv(annotations_dt, 'segment_type')
    annotations_dt[
        segment_type == 'info',
        segment_type := NA_character_]
        # dplyr::filter(
        #     annotations_dt,
        #     segment_type != 'info')

    # TODO: invert=FALSE parameter makes na.omit S3 method not generalizable.
    # It is used deliberately here to make the function call fail if the method call
    # has not been dispatched to the data.table implementation data.table:::na.omit.data.table,
    # which is substantially more efficient than stats::na.omit.data.frame.
    annotations_dt <-
        stats::na.omit(
            annotations_dt,
            # cols=base::c('segment_type'),
            # cols=base::c('conclusion_type'),
            invert=FALSE)

    #### Verify annotation table
    # TODO: Check and fix this function.
    # input_verify_annotation_table_consistency(
    #     ANNOTATIONS=ANNOTATIONS@DT,
    #     DOCUMENTS_DIR_PATH_STR=DOCUMENTS_DIR_PATH_STR)

    #### Verify segment_type annotations

    SEGMENT_TYPES_CVEC <-
        base::sort(base::c('body', 'conclusion'))
    CURRENT_SEGMENT_TYPES_CVEC <-
        base::sort(
            base::unique(
                annotations_dt[['segment_type']]))
    if (!base::identical(
            CURRENT_SEGMENT_TYPES_CVEC,
            SEGMENT_TYPES_CVEC))
        base::stop(
            futile.logger::flog.fatal(
                'The annotation table contains segment_type levels {%s}, but you specified a different SEGMENT_TYPES_CVEC {%s}. ',
                base::paste0(
                    CURRENT_SEGMENT_TYPES_CVEC,
                    collapse=', '),
                base::paste0(
                    SEGMENT_TYPES_CVEC,
                    collapse=', ')))

    #### Verify conclusion_type annotations

    CONCLUSION_TYPES_CVEC <-
        base::sort(
            base::c('denied', 'granted', 'held insufficient'))
    CURRENT_CONCLUSION_TYPES_CVEC <-
        base::sort(
            base::unique(
                annotations_dt[['conclusion_type']]))
    if (!base::identical(
            CURRENT_CONCLUSION_TYPES_CVEC,
            CONCLUSION_TYPES_CVEC))
        base::stop(
            base::sprintf(
                'The annotation table contains conclusion_type levels {%s}, but you specified a different CONCLUSION_TYPES_CVEC {%s}. ',
                base::paste0(
                    CURRENT_CONCLUSION_TYPES_CVEC,
                    collapse=', '),
                base::paste0(
                    CONCLUSION_TYPES_CVEC,
                    collapse=', ')))

    ####
    ## Work with annotation table to prepare for analysis
    ###

    ## Add file name
    annotations_dt <-
        dplyr::mutate(
            annotations_dt,
            relative_file_path=
            base::file.path(
                document_name,
                base::sprintf(
                    '%d.xml',
                    sentence_ID)))

    ## Split ECLI into variables.
    annotations_dt <-
        tidyr::separate(
            annotations_dt,
            col='document_name',
            into=base::c('ECLI', 'country', 'court', 'year', 'case'),
            sep="\\:")

    ## Remove some columns to recover memory. SHA256 hash value column since they were
    ## only useful for verification. ECLI and country are constant.
    annotations_dt[,
        base::c('SHA256_value', 'ECLI', 'country', 'sentence_ID') := NULL]

    ## Recode categorical data from string into integer representation
    COLNAMES_CVEC <-
        # TODO: do not hardcode?
        base::c('court', 'year', 'case', 'segment_type', 'conclusion_type')
    QUANTIFIED_COLUMN_NAMES_CVEC <-
        stringi::stri_join(COLNAMES_CVEC, '_I')
    COLUMN_LEVELS_LST <-
        base::lapply(
            COLNAMES_CVEC,
            function(COLNAME_STR)
                stringi::stri_unique(
                    annotations_dt[[COLNAME_STR]]))
    annotations_dt[,
        (QUANTIFIED_COLUMN_NAMES_CVEC):=base::list(
            base::match(
                court,
                COLUMN_LEVELS_LST[[1L]]),
            base::match(
                year,
                COLUMN_LEVELS_LST[[2L]]),
            base::match(
                case,
                COLUMN_LEVELS_LST[[3L]]),
            base::match(
                segment_type,
                COLUMN_LEVELS_LST[[4L]]),
            base::match(
                conclusion_type,
                COLUMN_LEVELS_LST[[5L]]))]

    BODY_DT <-
        annotations_dt[segment_type == 'body',][,
            object_ID := .GRP,
            by=.(court_I, year_I, case_I, segment_type_I)]

    CONCLUSION_WHOLE_DT <-
        annotations_dt[segment_type == 'conclusion',][,
        `:=`(
            object_ID=.GRP),
        by=.(court_I, year_I, case_I, segment_type_I)][,
        `:=`(
            conclusion_type_is_granted_I=.SD[,
                max(as.integer(conclusion_type == 'granted'))],
            conclusion_type_is_denied_I=.SD[,
                max(as.integer(conclusion_type == 'denied'))],
            conclusion_type_is_held_insufficient_I=.SD[,
                max(as.integer(conclusion_type == 'held insufficient'))]),
            by=object_ID,
            .SDcols=c('conclusion_type')]

    CONCLUSION_BY_CONCLUSION_TYPE_DT <-
        annotations_dt[segment_type == 'conclusion',][,
        object_ID := .GRP,
        by=.(court_I, year_I, case_I, segment_type_I, conclusion_type_I)]

    TASK_SETS_DT_LST <- base::list(
        all=annotations_dt,
        body=BODY_DT,
        conclusion_whole=CONCLUSION_WHOLE_DT,
        conclusion_by_type=CONCLUSION_BY_CONCLUSION_TYPE_DT)

    for (TASK_SETS_DT_NAME_STR in base::names(TASK_SETS_DT_LST)) {
        # TODO: remove redundant columns
        utils::write.table(
            col.names=TRUE,
            file=stringi::stri_join(
                ANNOTATIONS_TABLE_FILE_PATH_STR, TASK_SETS_DT_NAME_STR, sep='.'),
            fileEncoding='UTF-8',
            row.names=FALSE,
            sep='\t',
            x=TASK_SETS_DT_LST[[TASK_SETS_DT_NAME_STR]])
    }

    # TODO: return info
    base::stop('Wrote annotation tables. Please adapt script now so that annotation tables are not re-created. ')
}