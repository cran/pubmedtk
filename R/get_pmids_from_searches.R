#' Downloads PMID results for a column of Pubmed search queries in a
#' data frame
#'
#' @param df A dataframe containing a column of Pubmed search
#'     queries. This data frame cannot have columns with the following
#'     names: `pubmed_search_success`, `pmids`.
#'
#' @param column The name of the column containing Pubmed search
#'     queries
#'
#' @param api_key A valid Pubmed API key
#'
#' @param quiet A boolean TRUE or FALSE. If TRUE, no progress messages
#'     will be printed during download. FALSE by default, messages
#'     printed for every version downloaded showing progress.
#'
#' @return A data frame containing the original columns as well as
#'     three additional columns:
#'
#'     The `pubmed_search_success` column is TRUE in the case that the
#'     search rcesults were successfully obtained from Pubmed; FALSE
#'     in the case that an error occurred in search (e.g. due to a
#'     search query that is not well-formed).
#'
#'     The `n_results` column contains the number of research results
#'     for the query provided.
#'
#'     The `pmids` column returns a JSON-encoded list of PMID's for
#'     the search query provided.
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
#' ## Example Pubmed searches, some valid, some not, some with more
#' ## than 10k results
#' searches <- tribble(
#'   ~terms,
#'   "Carlisle B[Author]",
#'   "NCT00267865",
#'   "(Clinical Trial[Publication Type])",
#'   ""
#' )
#'
#' ## Download search results
#' results <- get_pmids_from_searches(searches, "terms", ak)
#' }

get_pmids_from_searches <- function (
                                     df,
                                     column,
                                     api_key,
                                     quiet = FALSE
                                     ) {

    out <- tryCatch({

        ## Check that API key is well-formed
        api_key <- stringr::str_trim(api_key)
        assertthat::assert_that(
                        grepl(
                            "^[0-9a-f]{36}$",
                            as.character(api_key)
                        ),
                        msg="Pubmed API key is not well-formed"
                    )

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
                                "pubmed_search_success",
                                "pmids"
                            )
                            %in%
                            colnames(df)
                        ) == 0,
                        msg = paste(
                            "The supplied data frame cannot contain",
                            "columns with the following names:",
                            "pubmed_search_success, pmid"
                            
                        )
                    )

        ## Check that `quiet` is boolean
        assertthat::assert_that(
                        quiet == TRUE | quiet == FALSE,
                        msg = paste(
                            "The `quiet` argument must be",
                            "TRUE or FALSE"
                        )
                    )

        ## Pull out the queries to be searched
        queries <- df %>%
            dplyr::pull(column)

        if (! quiet) {
            message(
                paste(
                    length(queries),
                    "queries to be searched"
                )
            )
        }
        
        ## Add the new columns
        df$pubmed_search_success <- as.logical(NA)
        df$n_results <- as.numeric(NA)
        df$pmids <- as.character(NA)

        query_count <- 0
        
        for (query in queries) {

            ## Error checking
            if (! is.na(query)) {
                ## Perform search
                pm_search_result <- get_pmids_from_one_search(
                    query,
                    api_key
                )
            } else {
                
                pm_search_result <- list(
                    pubmed_search_success = FALSE,
                    n_results = 0,
                    pmids = NA
                )
            }

            ## Apply it to the data frame

            df <- df %>%
                dplyr::mutate(
                           pubmed_search_success = ifelse(
                               ! is.na (query) & !!dplyr::sym(column) == query,
                               pm_search_result$pubmed_search_success,
                               .data$pubmed_search_success
                           )
                       )
            
            df <- df %>%
                dplyr::mutate(
                           n_results = ifelse(
                               ! is.na(query) & !!dplyr::sym(column) == query,
                               pm_search_result$n_results,
                               .data$n_results
                           )
                       )

            df <- df %>%
                dplyr::mutate(
                           pmids = ifelse(
                               ! is.na(query) & !!dplyr::sym(column) == query,
                                   ifelse(
                                       ! is.na(pm_search_result$pmids),
                                       jsonlite::toJSON(pm_search_result$pmids),
                                       NA
                                   ),
                               .data$pmids
                               
                           )
                       )

            query_count <- query_count + 1
            
            prop_done <- round(100 * query_count / length(queries), digits=3)

            if (! quiet) {
                message(
                    paste0(
                        Sys.time(),
                        " Done ",
                        query_count,
                        " of ",
                        length(queries),
                        " (",
                        prop_done,
                        "%)"
                    )
                )
            }
        }

        return(df)
        
    },
    error = function(cond) {
        message("Error downloading Pubmed search result")
        message("Here's the original error message:")
        message(paste(cond, "\n"))
        ## Choose a return value in case of error
        return(FALSE)
    },
    warning = function(cond) {
        message("Warning downloading Pubmed search result")
        message("Here's the original error message:")
        message(paste(cond, "\n"))
        ## Choose a return value in case of error
        return(TRUE)
        
    },
    finally = {
        
    })

    return(out)
    
}
