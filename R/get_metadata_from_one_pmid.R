#' Downloads metadata from Pubmed API for a single provided PMID and
#' exports 
#'
#' @param pmid A single PMID, e.g. "29559429"
#'
#' @param api_key A valid Pubmed API key
#'
#' @return A named list with 7 elements:
#'
#'     `$pubmed_dl_success`, which is TRUE in the case that a
#'     corresponding Pubmed record was found and metadata
#'     downloaded and FALSE otherwise.
#'
#'     `$doi`, a character string containing the DOI for the
#'     publication with the PMID in question.
#'
#'     `$languages`, a list of languages corresponding to the
#'     publication with the PMID in question.
#'
#'     `$pubtypes`, a list of publication types corresponding to the
#'     publication with the PMID in question.
#'
#'     `$pubdate`, the listed publication date
#'
#'     `$epubdate`, the listed e-publication date
#'
#'     `$authors`, a list of authors of the publication with the PMID
#'     in question.
#'
#'     `$abstract`, a character string containing the abstract for the
#'     publication with the PMID in question.
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
#' ## Download Pubmed metadata
#' mdata <- get_metadata_from_one_pmid("29559429", ak)
#'
#' ## Extract first author
#' mdata$authors[1]
#' }

get_metadata_from_one_pmid <- function(pmid, api_key) {
    
    out <- tryCatch({

        ## Check that PMID is well-formed
        ## https://www.nlm.nih.gov/bsd/mms/medlineelements.html#pmid
        assertthat::assert_that(
                        grepl(
                            "^[0-9]+\\.?[0-9]+$",
                            as.character(pmid)
                        ),
                        msg="PMID is not well-formed"
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

        pubmed_query <- list(
            api_key = api_key,
            db = "pubmed",
            id = pmid,
            retmode = "xml"
        )

        result <- httr::POST(
        "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/esummary.fcgi",
                            body = pubmed_query,
                            encode = "form"
        ) %>%
            xml2::read_xml()

        closeAllConnections()

        ## Check for error
        errors <- xml2::xml_find_all(
                            result,
                            "/eSummaryResult/ERROR"
                        )
        
        assertthat::assert_that(
                        length(errors) == 0,
                        msg=paste(
                            "Pubmed download error:",
                            xml2::xml_text(errors)
                        )
                    )
        
        languages <- xml2::xml_find_all(
            result,
            "/eSummaryResult/DocSum/Item[contains(@Name, 'LangList')]/Item[contains(@Name, 'Lang')]"
        ) %>%
            xml2::xml_text()

        pubtypes <- xml2::xml_find_all(
            result,
            "/eSummaryResult/DocSum/Item[contains(@Name, 'PubTypeList')]/Item[contains(@Name, 'PubType')]"
        ) %>%
            xml2::xml_text()

        pubdate <- xml2::xml_find_all(
            result,
            "/eSummaryResult/DocSum/Item[@Name='PubDate']"
        ) %>%
            xml2::xml_text()

        epubdate <- xml2::xml_find_all(
            result,
            "/eSummaryResult/DocSum/Item[@Name='EPubDate']"
        ) %>%
            xml2::xml_text()

        authors <- xml2::xml_find_all(
            result,
            "/eSummaryResult/DocSum/Item[contains(@Name, 'AuthorList')]/Item[contains(@Name, 'Author')]"
        ) %>%
            xml2::xml_text()

        doi <- xml2::xml_find_first(
            result,
            "/eSummaryResult/DocSum/Item[contains(@Name, 'DOI')]"
        ) %>%
            xml2::xml_text()

        ## Now efetch for the abstract
       
        pubmed_query <- list(
            api_key = api_key,
            db = "pubmed",
            id = pmid,
            retmode = "xml"
        )

        result <- httr::POST(
        "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi",
                            body = pubmed_query,
                            encode = "form"
        ) %>%
            xml2::read_xml()

        closeAllConnections()

        abstract <- xml2::xml_find_all(
            result,
            "/PubmedArticleSet/PubmedArticle/MedlineCitation/Article/Abstract/AbstractText"
        ) %>%
            xml2::xml_text()

        if (length(abstract) == 0) {
            abstract <- ""
        }

        list(
            pubmed_dl_success = TRUE,
            doi = doi,
            languages = languages,
            pubtypes = pubtypes,
            pubdate = pubdate,
            epubdate = epubdate,
            authors = authors,
            abstract = abstract
        ) %>%
            return()
                
    },
    error = function(cond) {
        message(paste("Error downloading PMID:", pmid))
        message("Here's the original error message:")
        message(paste(cond, "\n"))
        ## Choose a return value in case of error
        list(
            pubmed_dl_success = FALSE,
            doi = NA,
            languages = NA,
            pubtypes = NA,
            pubdate = NA,
            epubdate = NA,
            authors = NA,
            abstract = NA
        ) %>%
            return()
    },
    warning = function(cond) {
        message(paste("Warning downloading PMID:", pmid))
        message("Here's the original warning message:")
        message(paste(cond, "\n"))
        ## Choose a return value in case of error
        list(
            pubmed_dl_success = FALSE,
            doi = NA,
            languages = NA,
            pubtypes = NA,
            pubdate = NA,
            epubdate = NA,
            authors = NA,
            abstract = NA
        ) %>%
            return()
    },
    finally = {
        ## To execute regardless of success or failure
    })

    return(out)
}
