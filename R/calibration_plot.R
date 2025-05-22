#' Create a calibration plot following meta-analysis THIS IS NOT OPTIMAL
#'
#'This may not be the optimal approach as smoothed calibraiton is plotted using all observations from internal-external cross-validation.
#'
#' @inherit meta_analysis_cont
#'
#' @returns Creates a plot showing all predicted and observed values from  internal-external cross-validation as well as the calibraion slope line fom the meta-analysis.
#'
#' @examples
calibration_plot_cont <- function(predictions_df, study_var_name) {
  calib_results <- get_performance_by_study(
    study_var_name = study_var_name, 
    by_study_predictions_df = predictions_df, 
    evaluate_performance = evaluate_performance_cont_obs_pred_calib_slope_int) 
  
  
  predictions_df[,study_var_name] <- factor(predictions_df[,study_var_name])
  plot <- predictions_df |> 
    ggplot2::ggplot(ggplot2::aes(x = pred, y = actual)) +
    ggplot2::geom_point(size = 0.75) +
    ggplot2::geom_abline(data = calib_results, aes(
      intercept = intercept, 
      slope = coef, 
      linetype = "Calibration line"), color = "black", show.legend = FALSE) +
    ggplot2::geom_smooth(aes(linetype = "Smoothed Calibration"), method="auto", se=FALSE, fullrange=FALSE, level=0.95, size = 0.5, colour = "blue") +
    ggplot2::geom_abline(aes(linetype = "Perfect calibration", intercept = 0, slope = 1), color = "red", show.legend = FALSE, size = 1.2) +
    ggplot2::facet_wrap(facets = vars(study), nrow = 2) +
    ggplot2::scale_linetype_manual(
      name = "", 
      values = c("Calibration line" = "solid", "Smoothed Calibration" = "solid", "Perfect calibration" = "dashed")) +
    ggplot2::labs(y = "Observed", x = "Predicted", title = "Calibration Plots") +
    ggplot2::guides(linetype = guide_legend(override.aes = list(
      color = c("black", "blue", "red"), 
      linetype = c("solid", "solid", "dashed"), 
      size = c(0.5, 0.5, 0.5)
    ))) +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "top") 
  
    return(plot)
}
