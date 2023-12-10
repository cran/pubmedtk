#' Returns a list of PMID's for a provided Pubmed search
#'
#' @param query A Pubmed search query
#'
#' @param api_key A valid Pubmed API key
#'
#' @return A named list with 3 elements:
#'
#'     `$pubmed_search_success`, which is TRUE in the case that the
#'     provided query was searched successfully on Pubmed and FALSE
#'     otherwise.
#'
#'     `$n_results`, the number of results for the search as reported
#'     by Pubmed
#'
#'     `$pmids`, a list of PMID's corresponding to the Pubmed search
#'     results for the query provided
#'
#' @export
#'
#' @importFrom magrittr %>%
#'
#' @examples
#'
#' \dontrun{
#' ## Read in API key
#' ak <- readLines("api_key.txt")
#'
#' ## Download PMID's for search query
#' results <- get_pmids_from_one_search("Carlisle B[Author]", ak)
#'
#' ## Extract first result
#' results$pmids[1]
#' }

get_pmids_from_one_search <- function (query, api_key) {

    out <- tryCatch({
        
        pubmed_search <- list(
            api_key = api_key,
            db = "pubmed",
            term = query,
            retmode="xml",
            retmax = 10000
        )

        result <- httr::POST(
         "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/esearch.fcgi",
                            body=pubmed_search,
                            encode="form"
                        ) %>%
            xml2::read_xml()

        closeAllConnections()

        ## Check for error
        errors <- xml2::xml_find_all(
                            result,
                            "/eSearchResult/ERROR"
                        )
        
        assertthat::assert_that(
                        length(errors) == 0,
                        msg=paste(
                            "Pubmed download error:",
                            xml2::xml_text(errors)
                        )
                    )

        ## Process results
        
        numres <- xml2::xml_find_all(
            result,
            "/eSearchResult/Count"
        ) %>%
            xml2::xml_text() %>%
            as.numeric()

        ## Check for more than 10k results
        if (numres > 10000) {
            message(
                paste(
                    "Warning: More than 10,000 results returned for:",
                    query
                )
            )
        }
        
        pmids <- xml2::xml_find_all(
                           result,
                           "/eSearchResult/IdList/Id"
                       ) %>%
            xml2::xml_text()
        
        list(
            pubmed_search_success = TRUE,
            n_results = numres,
            pmids = pmids
        ) %>%
            return()
        
    },
    error = function(cond) {
        message(paste("Error in Pubmed search:", query))
        message("Here's the original error message:")
        message(paste(cond, "\n"))
        ## Choose a return value in case of error
        list(
            pubmed_search_success = FALSE,
            n_results = NA,
            pmids = NA
        ) %>%
            return()
    },
    warning = function (cond) {
        message(paste("Warning in Pubmed search:", query))
        message("Here's the original error message:")
        message(paste(cond, "\n"))
        ## Choose a return value in case of error
        list(
            pubmed_search_success = FALSE,
            n_results = NA,
            pmids = NA
        ) %>%
            return()
    },
    finally = {
        ## To execute regardless of success or failure
    })

    return(out)
}
