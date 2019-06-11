#' Run a regression and return the results in a tibble. Useful for running multiple regressions with purrr::pmap.
#' All inputs are fed directly into as.formula, and should follow the relevant specifications.
#'
#' @param lhs Character vector with left hand side variable.
#' @param rhs Character vector with right hand side variable(s).
#' @param fe Character vector with fixed effects. Optional.
#' @param iv Character vector with instruments. Optional.
#' @param clus Character vector of clustering variable(s). Optional.
#' @param data_str String to be evaluated to produce dataset. Use single quotes inside double quotes to avoid need for escaping.
#' @param tidy TRUE/FALSE. Return a tidied data.frame rather than a fitted felm object? Optional.
#'
#' @return tibble with summary statistics and fitted output (felm or tidied data.frame).
#' @import data.table
#' @import tibble
#' @export
#'
#' @examples # Load data
#' data("Wages", package="plm")
#' setDT(Wages)
#'
#' reg_tibble <- as_tibble(
#'   expand.grid(lhs = "lwage", rhs = c("exp", "exp + wks"),
#'               fe = "married", clus = "0", data_str = c("Wages", "Wages[bluecol == 'no']"),
#'               stringsAsFactors = F))
#'
#' reg_output <- cbind(reg_tibble, purrr::pmap_dfr(reg_tibble, reg_felm))
#'
#' # Can also save regression output as a tidied data.frame
#' reg_output_tidy <- cbind(reg_tibble, purrr::pmap_dfr(reg_tibble, reg_felm, tidy = T))
#'
#' # These can be used directly in stargazer or huxtable
#' stargazer::stargazer(reg_output$fit)
#' huxtable::huxreg(reg_output$fit)
#' huxtable::huxreg(reg_output_tidy$fit_tidy)
#'
#' # Easy to select which regressions to display
#' stargazer::stargazer(reg_output %>% filter(rhs == "exp + wks") %>% pull(fit))
#' huxtable::huxreg(reg_output %>% filter(rhs == "exp + wks") %>% pull(fit))
reg_felm <- function(lhs, rhs, fe = "0", iv = "0", clus = "0", data_str, tidy = FALSE) {
  # Parse and evaluate data string to load it
  data <- eval(parse(text = data_str))

  # Extract all character variables, limit data to observations with all present (makes summary stats easier)
  all_vars <- unlist(stringr::str_extract_all(c(lhs, rhs, fe, iv, clus), pattern = "[a-zA-Z][a-zA-Z_0-9]*"))
  data <- data[complete.cases(data[, all_vars, with = F])] # data.table evaluation for speed

  fmla <- sprintf("%s ~ %s | %s | %s | %s", lhs, rhs, fe, iv, clus)
  fit <- strip_felm(lfe::felm(as.formula(fmla), data = data))

  output <- tibble(n = nrow(data)) # Need a tibble, this requires list-columns
  # Can add other custom summary statistics here

  # Depending on whether tidy was specified, return either a tidy version of results or the full fit.
  # N.B. In either case, return within a list so that it will be a list-column
  if (tidy) {
    output$fit_tidy = list(broom::tidy(fit))
  } else {
    output$fit = list(fit)
  }
  output
}
