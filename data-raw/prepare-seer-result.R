# Convert the DevCan display table into a tidy external-validation fixture.
# Run from the package root after replacing data-raw/seer_result.csv.

source_file <- "data-raw/seer_result.csv"
output_file <- "tests/testthat/fixtures/seer-devcan-all-gamma.csv"

x <- utils::read.csv(
  source_file,
  check.names = FALSE,
  stringsAsFactors = FALSE,
  na.strings = ""
)
end_labels <- names(x)[-1L]
ends <- suppressWarnings(as.numeric(end_labels))
ends[end_labels == "90+"] <- Inf

parse_number <- function(x) {
  suppressWarnings(as.numeric(gsub("[^0-9.-]", "", x)))
}

blocks <- lapply(0:18, function(block) {
  point_row <- 1L + 4L * block
  valid <- seq.int(block + 1L, 19L)
  data.frame(
    start = suppressWarnings(as.numeric(x[point_row, 1L])),
    end = ends[valid],
    risk = parse_number(x[point_row, valid + 1L]),
    lower = parse_number(x[point_row + 1L, valid + 1L]),
    upper = parse_number(x[point_row + 2L, valid + 1L])
  )
})
result <- do.call(rbind, blocks)

stopifnot(
  nrow(result) == 190L,
  !anyNA(result),
  !anyDuplicated(result[c("start", "end")]),
  all(result$lower <= result$upper)
)
utils::write.csv(result, output_file, row.names = FALSE, na = "")