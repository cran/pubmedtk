# pubmedtk 1.0.4

* Fixed bug in `intersection_check()` where it would crash if the
  column supplied was not named "pmid"
* Fixed bug in `get_pmids_from_searches()` where an NA search can
  cause unexpected results
* Added `quiet` option to `intersection_check()` to disable progress
  messages during download

---

# pubmedtk 1.0.3

* Added `$abstract` download to `get_metadata_from_one_pmid()` and
  corresponding column to `get_metadata_from_pmids()`

---

# pubmedtk 1.0.2

* Added optional progress feedback during download for
  `get_pmids_from_searches()` and `get_metadata_from_pmids()`
* Added `$pubdate` and `$epubdate` download to
  `get_metadata_from_one_pmid()` and corresponding columns to
  `get_metadata_from_pmids()`
* Improved feedback for non-valid input

---

# pubmedtk 1.0.1

* Fix bug with uncommented instructions in code
* Added web reference to API in Description
* Minor changes to name for consistency with CRAN (Pubmed -> 'Pubmed')

---

# pubmedtk 1.0.0

* Initial CRAN submission.
* Added `get_pmids_from_one_search()`
* Added `get_pmids_from_searches()`
* Added `get_metadata_from_one_pmid()`
* Added `get_metadata_from_pmids()`
* Added `intersection_check()`
