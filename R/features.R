features_extract <- function(
    ANNOTATIONS,
    ARC_CATEGORY_TABLES_DIR_PATH,
    # TODO: rename to JOB parameters
    DESIGN_MATRIX_FACTORIZATION_RANK_FRACTION_D,
    FEATURES_EXTRACTION_PARAMETERS,
    SCORING_STR,
    SRF_N_I,
    STRCT_I_I,
    STRCT_M_I,
    STRCT_N_I) {

    ###
    ## Surface features
    ###

    SRF_SRILM_NGRAMCOUNT_PARAMETERS_CVEC <-
        ## -sort and -tolower only make sense for SRF, as SRF counts on text files
        ## whereas STRCT produces its count file in custom way, not based on flat text.
        base::c(
            # '-debug', '0',
            ## -minprune should be > N_I!
            '-minprune', base::as.character(SRF_N_I + 1L),
            '-no-sos', '-no-eos',
            ## Disable smoothing
            '-addsmooth', '0',
            ## -order should be equal to N_I!
            '-order', base::as.character(SRF_N_I),
            '-sort',
            '-tolower')

    SRF_SRILM_NGRAM_PARAMETERS_CVEC <-
        base::c(
            # '-debug', '1',
            '-no-sos', '-no-eos')

    SRF_FEATURE_REPRESENTATION <-
        feat::SrfFeatureRepresentation(
            # TODO: min_N
            N_I=SRF_N_I,
            DESIGN_MATRIX_FACTORIZATION_RANK_FRACTION_D=DESIGN_MATRIX_FACTORIZATION_RANK_FRACTION_D,
            OBJECT_SEPARATOR_STR=' ',
            SCORING_STR=SCORING_STR,
            SRILM_NGRAM_PARAMETERS_CVEC=SRF_SRILM_NGRAM_PARAMETERS_CVEC,
            SRILM_NGRAMCOUNT_PARAMETERS_CVEC=SRF_SRILM_NGRAMCOUNT_PARAMETERS_CVEC,
            XPATH_STR='/alpino_ds/sentence/text()')

    ###
    ## Structural features
    ###

    STRCT_SRILM_NGRAMCOUNT_PARAMETERS_CVEC <-
        ## -sort and -tolower only make sense for SRF, as SRF counts on text files
        ## whereas STRCT produces its count file in custom way, not based on flat text.
        base::c(
            # '-debug', '0',
            ## As the STRCT feature representation outputs fractional feature scores,
            ## the following CLI argument is required:
            '-float-counts',
            ## -minprune should be > N_I!
            '-minprune', base::as.character(STRCT_N_I + 1L),
            '-no-sos', '-no-eos',
            ## Disable smoothing
            '-addsmooth', '0',
            ## -order should be equal to N_I!
            '-order', base::as.character(STRCT_N_I),
            '-sort')

    STRCT_SRILM_NGRAM_PARAMETERS_CVEC <-
        base::c(
            # '-debug', '1',
            '-no-sos', '-no-eos')
    ## TODO: make parameters
    ARC_CATEGORIES_CVEC <-
        base::c('cat', 'pos', 'rel')
    ARC_CATEGORIES <-
        feat::strct_read_arc_category_tables(
            ARC_CATEGORIES_CVEC=ARC_CATEGORIES_CVEC,
            ARC_CATEGORY_TABLES_DIR_PATH=ARC_CATEGORY_TABLES_DIR_PATH)
    # TODO: add sense, lcat?
    STRCT_FEATURE_REPRESENTATION <-
        feat::StrctFeatureRepresentation(
            # TODO: min_N
            ALL_NGRAMS_UP_TO_N_I_B=TRUE,
            ARC_CATEGORIES=ARC_CATEGORIES,
            DESIGN_MATRIX_FACTORIZATION_RANK_FRACTION_D=DESIGN_MATRIX_FACTORIZATION_RANK_FRACTION_D,
            I_I=STRCT_I_I,
            M_I=STRCT_M_I,
            N_I=STRCT_N_I,
            SCORING_STR=SCORING_STR,
            SRILM_NGRAM_PARAMETERS_CVEC=STRCT_SRILM_NGRAM_PARAMETERS_CVEC,
            SRILM_NGRAMCOUNT_PARAMETERS_CVEC=STRCT_SRILM_NGRAMCOUNT_PARAMETERS_CVEC,
            XPATH_STR='/alpino_ds/node//node')

    FEATURE_REPRESENTATIONS_LST <-
        base::list(
            SRF=SRF_FEATURE_REPRESENTATION,
            STRCT=STRCT_FEATURE_REPRESENTATION)
    # TODO: use something better than vec.len heuristic
    # TODO: use writeRDS only
    utils::capture.output(
        { utils::str(FEATURES_EXTRACTION_PARAMETERS, vec.len=100L)
          utils::str(FEATURE_REPRESENTATIONS_LST, vec.len=100L) },
        file=base::file.path(
            FEATURES_EXTRACTION_PARAMETERS@OUTPUT_DIR_PATH,
            'feat_parameters.txt'))

    DESIGN_MATRIX_LST <-
        feat::objects_process_collection(
            DT=ANNOTATIONS@DT,
            FEATURES_EXTRACTION_PARAMETERS=FEATURES_EXTRACTION_PARAMETERS,
            FEATURE_REPRESENTATIONS_LST=FEATURE_REPRESENTATIONS_LST)

    return(DESIGN_MATRIX_LST)
}