#' Contraceptive Relavence Rate Over Time
#'
#' @param dat A data frame containing observed contraceptive use data with columns:
#'   `iso` (country code), `year` (reference year), and `cp` (contraceptive use modern %).
#' @param est A data frame containing estimated contraceptive use data with columns:
#'   `iso`, `Year`, `Median`, `L95`, `U95`, `L80`, `U80`.
#' @param iso_code Numeric Value for a country code 
#' @param CI The confidence interval for the graphs, either 95%, 80% or other.
#'
#' @return A ggplot object displaying the contraceptive use over time.
#' @export
#'
#' @examples
plot_cp <- function(dat, est, iso_code, CI = NULL) {
  est2 <- est %>% filter(iso == iso_code)
  dat <- dat %>% filter(iso == iso_code)
  p <- ggplot(est2, aes(x = Year, y = Median)) +
    geom_line(color = "blue", size = 1.2) +  
    geom_point(aes(x = year, y = cp), data = dat, color = "black") + 
    labs(x = "Time", y = "Modern Use (%)", 
         title = paste(est2$`Country or area`[1])) +
    theme_minimal()
  if (is.null(CI)) {
    CI <- 95}
  if (!is.null(CI) && !is.na(CI)) {
    if (CI == 95) {
      p <- p + geom_ribbon(aes(ymin = L95, ymax = U95), fill = "darkgrey", alpha = 0.5)  
    } else if (CI == 80) {
      p <- p + geom_ribbon(aes(ymin = L80, ymax = U80), fill = "darkgrey", alpha = 0.5)  
    }
  }
  
  return(p)}
