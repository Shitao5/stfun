#' Read Data from Elasticsearch Query JSON
#'
#' This function is used to extract data from a JSON file containing the results of an Elasticsearch query. It converts the results into a tibble, then flattens the '_source' column and removes the '_index', '_id', and '_score' columns.
#'
#' @param json_file The path to the JSON file containing the Elasticsearch query results.
#' @return Returns a tibble containing the data extracted from the query results.
#'
#' @examples
#' \dontrun{
#' # Read Elasticsearch query results from a JSON file
#' query_result <- read_es_query("path/to/query_result.json")
#'
#' # View the first few rows of data
#' head(query_result)
#' }
#'
#' @import jsonlite
#' @import dplyr
#' @import tidyr
#'
#' @export
read_es_query <- function(json_file) {
  json = jsonlite::fromJSON(json_file)
  json$hits$hits %>%
    as_tibble() %>%
    unnest_wider(any_of("_source")) %>%
    select(-all_of(c("_index", "_id", "_score")))
}
