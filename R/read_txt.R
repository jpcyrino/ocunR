#' Read a Text File with Linguistic Data
#'
#' File must comply with the following format: 3 lines per data, first line is the sentence in the
#' language with morphological segmentation, second line is the gloss and third line is the translation.
#' This function will therefore reject files with a not divisible by three count of lines.
#'
#' @param txt.file Text file containing the data.
#' @return A data frame with three columns: language, gloss and translation
#' @export

read_txt <- function(txt.file) {
  raw.data <- readr::read_delim(txt.file, "\t", escape_double = FALSE,
                                col_names = FALSE, col_types= readr::cols(),
                                trim_ws = TRUE)

  # Check if the data meets the multiple of three row requirement.
  # Error is thrown in the contrary.
  if(nrow(raw.data) %% 3 != 0) stop('Number of rows (lines) is not multiple of 3, possible corrupted data. Please check file!')

  proc.data <- data.frame(raw.data[seq(1,nrow(raw.data),by=3),1],
                          raw.data[seq(2,nrow(raw.data),by=3),1],
                          raw.data[seq(3,nrow(raw.data),by=3),1])
  colnames(proc.data) <- c("language","gloss","translation")
  proc.data
}
