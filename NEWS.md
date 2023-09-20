# stfun 0.1.21

- Added `read_es_query()`, which facilitates the reading and tidying of Elasticsearch query results into a tibble.

# stfun 0.1.20

- Added `warn = FALSE` to `readLines(con)` to remove warning when using `format_input()`.

# stfun 0.1.19

- Modified the function name from `quote_text()` to `format_input()` and introduced two additional parameters: `type`, which determines whether quotation marks are included, and `sep`, which governs the formatting of the output.

# stfun 0.1.18

-  Improved the `progress()` function to no longer display the task progress as 100% when the task is completed.

# stfun 0.1.17

- Add the `quote_text()` function to wrap a column of strings with double quotes and commas so that they can be used as input for other functions (e.g., `c()`).

# stfun 0.1.16

- Add `extract_highlights()` to extract highlighted sentences from a Markdown file.
Now I can easily identify and collect important sentences enclosed in `**` for further analysis or presentation.
