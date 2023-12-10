#' Checks a column of PMID's for whether or not they would appear in a
#' Pubmed search result.
#'
#' @param df A dataframe containing a column of PMID's
#'
#' @param column The name of the column containing PMID's
#'
#' @param query A character string containing a valid Pubmed search
#'     query
#' 
#' @param api_key A valid Pubmed API key
#'
#' @param batch_size An integer greater than 0 and less than 10000
#'
#' @return A data frame containing the original columns, as well as
#'     two additional ones: `pm_checked` and `found_in_pm_query`.
#'
#'     The new `pm_checked` column is TRUE if Pubmed was successfully
#'     queried and NA if Pubmed was not checked for that PMID (this
#'     may occur in cases where the PMID to be checked is not
#'     well-formed).
#'
#'     The new `found_in_pm_query` column is TRUE if the PMID in
#'     question would appear in a search of Pubmed defined by the
#'     query provided; FALSE if it would not appear in such a search
#'     and NA if the PMID in question was not checked (this may occur
#'     in cases where the PMID is not well-formed).
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
#'   "28961465",
#'   "32278621",
#'   "one hundred of them",
#'   "28837722",
#'   "28961465"
#' )
#'
#' ## Check which ones were authored by Carlisle:
#' intersection_check(pubs, "pmid", "Carlisle[Author]", ak)
#' 
#' }

intersection_check <- function (
                                df,
                                column,
                                query,
                                api_key,
                                batch_size = 1000
                                ) {

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
                                "pubmed_check_success",
                                "found_in_pubmed_query"
                            )
                            %in%
                            colnames(df)
                        ) == 0,
                        msg = paste(
                            "The supplied data frame cannot contain",
                            "columns with the following names:",
                            "pubmed_check_success,",
                            "found_in_pubmed_query"
                        )
                    )

        ## Check that API key is well-formed
        api_key <- stringr::str_trim(api_key)
        assertthat::assert_that(
                        grepl(
                            "^[0-9a-f]{36}$",
                            as.character(api_key)
                        ),
                        msg="Pubmed API key is not well-formed"
                    )

        ## Check that Pubmed API is reachable
        assertthat::assert_that(
                        ! httr::http_error(
                             "https://eutils.ncbi.nlm.nih.gov/entrez/"
                                ),
                        msg="Unable to connect to Pubmed API"
                    )

        ## Check that batch size is integer between 1 and 10000
        assertthat::assert_that(
                        is.numeric(batch_size),
                        msg="Batch size must be a number"
                    )
        assertthat::assert_that(
                        batch_size == round(batch_size),
                        msg="Batch size must be an integer"
                    )
        assertthat::assert_that(
                        batch_size > 0 &
                        batch_size <= 10000,
                        msg="Batch size must be between 1 and 10,000"
                    )

        ## Pull out the well formed PMID's to be checked
        pmids <- df %>%
            dplyr::filter(
                       grepl("^[0-9]+\\.?[0-9]+$", !!dplyr::sym(column))
                   ) %>%
            dplyr::group_by(!!dplyr::sym(column)) %>%
            dplyr::slice_head() %>%
            dplyr::select(!!dplyr::sym(column))

        message(paste(nrow(pmids), "unique PMID's to check"))
        
        ## Add new columns
        pmids$pm_checked <- FALSE
        pmids$found_in_pm_query <- as.logical(NA)

        while (sum (pmids$pm_checked) < nrow(pmids)) {

            pmid_batch <- pmids %>%
                dplyr::filter(! .data$pm_checked) %>%
                utils::head(n=batch_size)
            
            pmid_search_term <- pmid_batch$pmid %>%
                paste(collapse="[PMID] OR ") %>%
                paste0("[PMID]")

            search_term <- paste0(
                "(",
                query,
                ") AND (",
                pmid_search_term,
                ")"
            )

            pubmed_search <- list(
                api_key = api_key,
                term = search_term,
                retmax = batch_size,
                db = "pubmed"
            )
            
            result <- httr::POST(
                "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/esearch.fcgi",
                body=pubmed_search,
                encode="form"
            ) %>%
                xml2::read_xml()

            closeAllConnections()

            found_pmids <- xml2::xml_find_all(
                result,
                "/eSearchResult/IdList/Id"
            ) %>%
                xml2::xml_text()

            ## Mark the ones that have just been done as checked
            pmids <- pmids %>%
                dplyr::mutate(
                    pm_checked = ifelse(
                        .data$pmid %in% pmid_batch$pmid,
                        TRUE,
                        .data$pm_checked
                    )
                ) %>%
                dplyr::mutate(
                    found_in_pm_query = ifelse(
                        .data$pmid %in% found_pmids,
                        TRUE,
                        .data$found_in_pm_query
                    )
                ) %>%
                dplyr::mutate(
                    found_in_pm_query = ifelse(
                        .data$pmid %in% pmid_batch$pmid &
                        ! .data$pmid %in% found_pmids,
                        FALSE,
                        .data$found_in_pm_query
                    )
                )

            n_done <- pmids %>%
                dplyr::filter(.data$pm_checked) %>%
                nrow()
            
            prop_done <- round(100 * n_done / nrow(pmids), digits=3)
            
            message(
                paste0(
                    prop_done,
                    "% done"
                )
            )
        }

        out <- df %>%
            dplyr::left_join(pmids, by=column)
        
    },
    error=function(cond) {
        message(
            paste(
                "Error:",
                cond
            )
        )

        return(NA)
    },
    warning=function(cond) {
        message(
            paste(
                "Warning:",
                cond
            )
        )

        return(NA)
    },
    finally={
    })

    return(out)
   
}
