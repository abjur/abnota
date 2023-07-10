#' new_proc
#'
#' @param ... params for R6 object
#'
#' @export
#'
new_proc <- function(...) {
  processo <- R6::R6Class(
    "processo",
    public = list(
      id = NULL,
      movs = NULL,
      gpt = NULL,
      conditions = NULL,
      initialize = function(id = NA, movs = NA, gpt_result = NA, conditions = NA) {
        self$id <- id
        self$movs <- movs
        self$gpt <- gpt_result
        self$conditions <- conditions
        self$show()
      },
      set_gpt_result = function(val) {
        self$gpt <- val
      },
      add_gpt_result = function(val) {
        self$gpt <- c(self$gpt, val)
      },
      set_conditions = function(val) {
        self$conditions <- val
      },
      add_conditions = function(val) {
        self$conditions <- paste(self$conditions, val, sep = " & ")
      },
      show = function() {
        cat(paste0("Processo ", self$id, "\n"))
        print(utils::head(self$movs))
        print(self$gpt)
      },
      show_conditions = function() {
        cat(self$conditions)
      }
    )
  )
  processo$new(...)
}