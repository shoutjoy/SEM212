#' item
#'
#' @param ... input
#'
#' @return list
#' @export
item <- function(...) {
  return(c(...))
}

#
#' Function to create blocks------------
#'
#' @param ... list data
#' @param dataset data.frame
#' @param output 'match', 'match_colnames' is compare data.frame
#'
#' @return list blocks
#' @export
#'
#' @examples
#'
#' \dontrun{
#' edu_blocks_named2 <- blocks(
#'   Support = item(1:4),
#'   Advising = item(5:8),
#'   Tutoring= item( 9:12),
#'   Value = item(13:16),
#'   Satisfaction = item(17:19),
#'   Loyalty =item(20:23),
#'   output = "match_colnames"
# ')
#' edu_blocks_named2
#'
#' # # Example usage
#' edu_blocks_named1 <- blocks(
#'   Support = item("sup.help", "sup.under", "sup.safe", "sup.conc"),
#'   Advising = item("adv.comp", "adv.acces", "adv.comm", "adv.qual"),
#'   Tutoring = item("tut.prof", "tut.sched", "tut.stud", "tut.qual"),
#'   Value = item("val.devel", "val.deci", "val.meet", "val.info"),
#'   Satisfaction = item("sat.glad", "sat.expe", "sat.over"),
#'   Loyalty = item("loy.proud", "loy.recom", "loy.asha", "loy.back"),
#'   dataset = education , # Provide your dataset here
#'   output = "match_colnames"
#' )
#'
#'
#' }
#'
blocks <- function(..., dataset = NULL, output = "match") {
  block_list <- list(...)

  # Combine the blocks into a named list
  names_list <- names(block_list)
  if (length(names_list) == 0) {
    stop("Please provide names for the blocks.")
  }

  combined_blocks <- list()
  last_value <- 0  # Variable to keep track of the last value in a block

  for (i in seq_along(names_list)) {
    current_block <- block_list[[i]]

    # Check if all items are character
    is_char <- all(sapply(current_block, is.character))

    # Check if all items are numeric
    is_numeric <- all(sapply(current_block, is.numeric))

    # If items are character and output is "match", keep them as is
    if (is_char && output == "match") {
      combined_blocks[[names_list[i]]] <- current_block
    } else if (is_numeric && output == "match") {  # If items are numeric and output is "match", convert to numeric
      combined_blocks[[names_list[i]]] <- as.numeric(current_block)
    } else if (output == "match_colnames") {  # If output is "match_colnames", match items with column names
      # Check if current_block is numeric and retrieve column names from dataset
      if (is.numeric(current_block)) {
        current_block <- colnames(dataset)[current_block]
      }
      # Keep only the column names that exist in the dataset
      current_block <- current_block[current_block %in% colnames(dataset)]
      combined_blocks[[names_list[i]]] <- current_block
    } else {  # If items are mixed or output is not "match_colnames", convert to numeric
      current_block <- as.numeric(current_block)
      if (length(current_block) > 0) {
        combined_blocks[[names_list[i]]] <- (last_value + 1):(last_value + length(current_block))
        last_value <- max(combined_blocks[[names_list[i]]])
      } else {
        # Handle the case when the block is empty
        combined_blocks[[names_list[i]]] <- integer(0)
      }
    }
  }

  return(combined_blocks)
}
