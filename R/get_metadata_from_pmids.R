#' Downloads metadata from Pubmed API for a column of PMID's in a data
#' frame
#'
#' @param df A dataframe containing a column of PMID's
#'
#' @param column The name of the column containing PMID's
#'
#' @param api_key A valid Pubmed API key
#'
#' @return A data frame containing the original columns as well as
#'     five additional columns:
#'
#'     The `pubmed_dl_success` column is TRUE in the case that
#'     metadata were successfully downloaded from Pubmed; FALSE in the
#'     case that an error occurred during downloading (e.g. due to a
#'     number that is well-formed but does not correspond to a true
#'     PMID); NA in the case that the supplied PMID is not well-formed
#'     (e.g. NA or non-numeric).
#'
#'     The `doi` column returns a DOI that corresponds to the PMID
#'     supplied if one is found, NA otherwise.
#'
#'     The `languages` column contains a JSON-encoded list of
#'     languages for the article in question.
#'
#'     The `pubtypes` column contains a JSON-encoded list of
#'     publication types for the article in question.
#'
#'     The `authors` column contains a JSON-encoded list of authors
#'     for the article in question.
#'
#' @export
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#'
#' @examples
#'
#' \dontrun{
#' ## Read in API key
#' ak <- readLines("api_key.txt")
#'
#' ## Example publications and their corresponding PMID's (some valid
#' ## and some not)
#' pubs <- tibble::tribble(
#'   ~pmid,
#'   "29559429",
#'   "28837722",
#'   NA,
#'   "borp",
#'   "98472657638729"
#' )
#'
#' ## Download Pubmed metadata
#' pm_meta <- get_metadata_from_pmids(pubs, "pmid", ak)
#'
#' ## Extract DOI's for those that were successfully downloaded
#' pm_meta %>%
#'   dplyr::filter(pubmed_dl_success)
#'   dplyr::select(pmid, doi)
#'
#' ## A tibble: 2 × 2
#' ##   pmid     doi                    
#' ##   <chr>    <chr>                  
#' ## 1 29559429 10.1136/bmj.k959       
#' ## 2 28837722 10.1001/jama.2017.11502
#' 
#' }

get_metadata_from_pmids <- function(df, column, api_key) {
    
    out <- tryCatch({

        ## Check that the column exists in the supplied df
        assertthat::assert_that(
                        column %in% colnames(df),
                        msg = paste(
                            "Column", column,
                            "is not present in supplied data frame"
                        )
                    )

        ## Check that the supplied data frame does not already have
        ## the columns that this function will add
        assertthat::assert_that(
                        mean(
                            c(
                                "pubmed_dl_success",
                                "doi",
                                "languages",
                                "pubtypes",
                                "authors"
                            )
                            %in%
                            colnames(df)
                        ) == 0,
                        msg = paste(
                            "The supplied data frame cannot contain",
                            "columns with the following names:",
                            "pubmed_dl_success, doi, languages,",
                            "pubtypes, authors"
                            
                        )
                    )

        ## Pull out the well formed PMID's to be checked
        pmids <- df %>%
            dplyr::filter(
                grepl("^[0-9]+\\.?[0-9]+$", !!dplyr::sym(column))
            ) %>%
            dplyr::pull(column)

        ## Add the new columns
        df$pubmed_dl_success <- as.logical(NA)
        df$doi <- as.character(NA)
        df$languages <- as.character(NA)
        df$pubtypes <- as.character(NA)
        df$authors <- as.character(NA)

        for (id in pmids) {

            ## Download the metadata
            pm_metadata <- get_metadata_from_one_pmid(id, api_key)

            ## Apply it to the data frame

            df <- df %>%
                dplyr::mutate(
                    pubmed_dl_success = ifelse(
                        !!dplyr::sym(column) == id,
                        pm_metadata$pubmed_dl_success,
                        .data$pubmed_dl_success
                    )
                )
            
            df <- df %>%
                dplyr::mutate(
                    doi = ifelse(
                        !!dplyr::sym(column) == id,
                        pm_metadata$doi,
                        .data$doi
                    )
                )

            df <- df %>%
                dplyr::mutate(
                           languages = ifelse(
                               !!dplyr::sym(column) == id,
                                       ifelse(
                                           ! is.na(pm_metadata$languages),
                                           jsonlite::toJSON(pm_metadata$languages),
                                           NA
                                       ),
                               .data$languages
                           )
                       )

            df <- df %>%
                dplyr::mutate(
                           pubtypes = ifelse(
                               !!dplyr::sym(column) == id,
                                      ifelse(
                                          ! is.na(pm_metadata$pubtypes),
                                          jsonlite::toJSON(pm_metadata$pubtypes),
                                          NA
                                      ),
                               .data$pubtypes
                           )
                       )

            df <- df %>%
                dplyr::mutate(
                           authors = ifelse(
                               !!dplyr::sym(column) == id,
                                     ifelse(
                                         ! is.na(pm_metadata$authors),
                                         jsonlite::toJSON(pm_metadata$authors),
                                         NA
                                     ),
                               .data$authors
                           )
                       )
            
        }

        out <- df

    },
    error = function(cond) {
        message("Error downloading Pubmed metadata")
        message("Here's the original error message:")
        message(paste(cond, "\n"))
        ## Choose a return value in case of error
        return(FALSE)
    },
    warning = function(cond) {
        message("Warning downloading Pubmed metadata")
        message("Here's the original error message:")
        message(paste(cond, "\n"))
        ## Choose a return value in case of error
        return(TRUE)
    },
    finally = {
        ## To execute regardless of success or failure
    })

    return(out)
}
