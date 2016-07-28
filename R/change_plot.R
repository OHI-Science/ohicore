#' change_plot.
#'
#' This function compares OHI scores from the current analysis and a previous commit.  The output is an interactive html plot that is saved
#' in the working directory in a file called 'changePlot_figures'.
#'
#' @param repo The repository name, e.g., 'ohi-global'.
#' @param scenario The scenario folder name that contains the 'scores.csv' file, e.g., 'eez2014'.
#' @param commit The 7 digit sha number identifying the commit, e.g., '4da6b4a'.  Otherwise, it is compared to the previous
#' commit.
#' @param fileSave Name for the figure.  This file will be saved in a folder called 'changePlot_figures'.
#' @param save_csv If TRUE, the difference csv file will be saved.
#' @param save_png If TRUE, a static png of the image will be saved. 
#' 
#' 
#' @export
change_plot = function(repo = "ohi-global", scenario="eez2014", commit="previous", 
           fileSave, save_csv=FALSE, save_png=FALSE){
  
  repo2 <- sprintf("../%s", repo)
  
  if(commit=="previous"){
    commit2 = substring(git2r::commits(git2r::repository(repo2))[[1]]@sha, 1, 7)
  } else{
    if (commit == "final_2014"){
      commit2 = '4da6b4a'
    } else {commit2 = commit}
  }
  
  tmp <- git2r::remote_url(git2r::repository(repo2))
  org <- stringr::str_split(tmp, "/")[[1]][4]
  
  path = paste0(scenario, '/scores.csv')
  
  data_old <- read_git_csv(paste(org, repo, sep="/"), commit2, path) %>%
    dplyr::select(goal, dimension, region_id, old_score=score)
  
  data_new <- read.csv(file.path(path)) %>%
    dplyr::left_join(data_old, by=c('goal', 'dimension', 'region_id')) %>%
    dplyr::mutate(change = score-old_score)
  
  p <- ggplot2::ggplot(data_new, aes(x=goal, y=change, color=dimension)) +
    #geom_point(shape=19, size=1) +
    theme_bw() + 
    labs(title=paste(scenario, commit, sep=" "), y="Change in score", x="") +
    scale_x_discrete(limits = c("Index", "AO", "SPP", "BD", "HAB", "CP", "CS", "CW", "FIS", "FP", 
                                "MAR", "ECO", "LE", "LIV", "NP", "LSP", "SP", "ICO", "TR")) +
    scale_colour_brewer(palette="Dark2") +
    geom_jitter(aes(text=paste0("rgn = ", region_id)), position = position_jitter(width=0.2, height=0), shape=19, size=1)
  
  plotly_fig <- plotly::ggplotly(p)
  htmlwidgets::saveWidget(plotly::as.widget(plotly_fig), "tmp_file.html", selfcontained=TRUE)
  
  my.file.rename <- function(from, to) {
    todir <- dirname(to)
    if (!isTRUE(file.info(todir)$isdir)) dir.create(todir, recursive=TRUE)
    file.rename(from = from,  to = to)
  }
  
  my.file.rename(from = "tmp_file.html",
                 to = file.path('changePlot_figures', paste0(fileSave, "_changePlot_", Sys.Date(), '.html')))
  
  
  if(save_png){
    ggplot2::ggsave(file.path('figures/DataCheck', paste0(fileSave, "_changePlot_", Sys.Date(), '.png')), width=8, height=5)
  }
  
  if(save_csv){
    write.csv(data_new, file.path(repo, 'figures/DataCheck', paste0(fileSave, "_diff_data_", Sys.Date(), '.csv')), row.names=FALSE)
  }
}

#'
#' @examples
#' add_numbers(1, 2) ## returns 3
#'
#
