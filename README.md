# pubmedtk

"Pubmed toolkit," an R package that provides functions for downloading
data via the Pubmed API and interpreting them.

## Installing

You can install this package via CRAN with the following:

```
install.packages("pubmedtk")
```

Or if you want the most up to date version, you can do so via
`devtools`:

```
install.packages("devtools")
library(devtools)
install_github("bgcarlisle/pubmedtk")
library(pubmedtk)
```

## Pubmed API

You will need a Pubmed API key to access the Pubmed API. This is a
36-character code that is provided by Pubmed. If you don't have one,
you can generate one by following these instructions:

1. Go to [Pubmed](https://pubmed.ncbi.nlm.nih.gov/)
2. Log in
3. Account > Account settings > API Key Management
4. Generate an API key
5. Save this in a plain-text file called `api_key.txt` in the root of
   your project and read this into your code using something like the
   following: `ak <- readLines("api_key.txt")`
6. Recommended: If you're using git for version control, open the
   `.gitignore` file and add `api_key.txt` to the file, so that you
   don't accidentally expose your Pubmed credentials to the world.
   
For all the examples below, assume that the `api_key.txt` file has
already been written to disk.

## Functions provided by `pubmedtk`

This package provides five functions: `get_pmids_from_one_search()`,
`get_pmids_from_searches()`, `get_metadata_from_one_pmid()`,
`get_metadata_from_pmids()`, and `intersection_check()`.

### `get_pmids_from_one_search()`

Returns a named list of PMID's for a provided Pubmed search with 3
elements:

1. `$pubmed_search_success`, which is TRUE in the case that the
provided query was searched successfully on Pubmed and FALSE
otherwise.
2. `$n_results`, the number of results for the search as reported by
Pubmed
3. `$pmids`, a list of PMID's corresponding to the Pubmed search
results for the query provided

Example:

```
## Read in API key
ak <- readLines("api_key.txt")

## Download PMID's for search query
result <- get_pmids_from_one_search("Carlisle B[Author]", ak)

## Extract first result
result$pmids[1]
```

### `get_pmids_from_searches()`

This function downloads PMID results for a column of Pubmed search
queries in a data frame and returns a data frame containing the
original columns as well as three additional columns:

1. The `pubmed_search_success` column is TRUE in the case that the
search rcesults were successfully obtained from Pubmed; FALSE in the
case that an error occurred in search (e.g. due to a search query that
is not well-formed).
2. The `n_results` column contains the number of research results for
the query provided.
3. The `pmids` column returns a JSON-encoded list of PMID's for the
search query provided.

Note that only 10,000 PMID's will be returned if your search has more
than this number of results.

Example:

```
library(tidyverse)

## Read in API key
ak <- readLines("api_key.txt")

## Example Pubmed searches, some valid, some not, some with more than
## 10k results
searches <- tribble(
  ~terms,
  "Carlisle B[Author]",
  "NCT00267865",
  "(Clinical Trial[Publication Type]) AND ((\"2021/01/01\"[Date - Publication] : \"2022/12/31\"[Date - Publication]))",
  ""
)

## Download search results
results <- get_pmids_from_searches(searches, "terms", ak)
```

### `get_metadata_from_one_pmid()`

Downloads metadata from the Pubmed API for a single PMID, and returns
a named list of 7 elements:

1. `$pubmed_dl_success`, which is TRUE in the case that a
corresponding Pubmed record was found and metadata downloaded and
FALSE otherwise.
2. `$doi`, a character string containing the DOI for the publication
with the PMID in question.
3. `$languages`, a list of languages corresponding to the publication
with the PMID in question.
4. `$pubtypes`, a list of publication types corresponding to the
publication with the PMID in question.
5. `$pubdate`, a string containing the publication date.
6. `$epubdate`, a string containing the e-publication date.
7. `$authors`, a list of authors of the publication with the PMID in
question.

Example:

```
## Read in API key
ak <- readLines("api_key.txt")

## Download Pubmed metadata
mdata <- get_metadata_from_one_pmid("29559429", ak)

## Extract first author
mdata$authors[1]
```

### `get_metadata_from_pmids()`

Downloads metadata from Pubmed API for a column of PMID's in a data
frame, and returns a data frame containing the original columns as
well as five additional columns:

1. The `pubmed_dl_success` column is TRUE in the case that metadata
were successfully downloaded from Pubmed; FALSE in the case that an
error occurred during downloading (e.g. due to a number that is
well-formed but does not correspond to a true PMID); NA in the case
that the supplied PMID is not well-formed (e.g. NA or non-numeric).
2. The `doi` column returns a DOI that corresponds to the PMID
supplied if one is found, NA otherwise.
3. The `languages` column contains a JSON-encoded list of languages
for the article in question.
4. The `pubtypes` column contains a JSON-encoded list of publication
types for the article in question.
5. The `pubdate` column contains a string of the publication date.
6. The `epubdate` column contains a string of the e-publication date.
7. The `authors` column contains a JSON-encoded list of authors for
the article in question.

Example:

```
library(tidyverse)

## Read in API key
ak <- readLines("api_key.txt")

## Example publications and their corresponding PMID's (some valid and
## some not)
pubs <- tribble(
  ~pmid,
  "28837722",
  NA,
  "98472657638729",
  "borp",
  "29559429"
)

## Download Pubmed metadata
pm_meta <- get_metadata_from_pmids(pubs, "pmid", ak)

## Extract DOI's for those that were successfully downloaded
pm_meta |>
  filter(pubmed_dl_success) |>
  select(pmid, doi)

## A tibble: 2 × 2
##   pmid     doi                    
##   <chr>    <chr>                  
## 1 28837722 10.1001/jama.2017.11502
## 2 29559429 10.1136/bmj.k959       
```

### `intersection_check()`

This function takes a Pubmed search query and a set of PMID's and
indicates for each PMID whether it would or would not be contained in
the results of the provided search query. The function returns the
original data frame with the column of PMID's to be checked, as well
as two additional columns: `pm_checked` and `found_in_pm_query`.

The new `pm_checked` column is TRUE if Pubmed was successfully queried
and NA if Pubmed was not checked for that PMID (this may occur in
cases where the PMID to be checked is not well-formed).

The new `found_in_pm_query` column is TRUE if the PMID in question
would appear in a search of Pubmed defined by the query provided;
FALSE if it would not appear in such a search and NA if the PMID in
question was not checked (this may occur in cases where the PMID is
not well-formed).

Example:

```
library(tidyverse)

## Read in API key
ak <- readLines("api_key.txt")

## Example publications and their corresponding PMID's (some valid and
## some not)
pubs <- tribble(
  ~pmid,
  "29559429",
  "28837722",
  "28961465",
  "32278621",
  "one hundred of them",
  "28837722",
  "28961465"
)

## Check which PMID's are authored by someone named "Carlisle"
intersection_check(pubs, "pmid", "Carlisle[Author]", ak)

## # A tibble: 7 × 3
##   pmid                pm_checked found_in_pm_query
##   <chr>               <lgl>      <lgl>            
## 1 29559429            TRUE       TRUE             
## 2 28837722            TRUE       TRUE             
## 3 28961465            TRUE       FALSE            
## 4 32278621            TRUE       FALSE            
## 5 one hundred of them NA         NA               
## 6 28837722            TRUE       TRUE             
## 7 28961465            TRUE       FALSE            
```


