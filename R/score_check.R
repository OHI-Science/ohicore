#' score_check
#' 
#' Used to error check scores after addition of new data by comparing current scores to a previous commit. 
#' Outputs are saved in a file within the github repository called "check scores". 
#'  Outputs include 1) an interactive html plot to examine the change 
#' (current score minus previous score) in all score dimensions (status, trend, resilience, pressure, future) 
#' across all goals/subgoals; 2) a static png plot of the interactive html plot; 3) a csv file with the 
#' current and previous scores; 4) a comparison of the NA scores     
#' 
#' @param scenario_year  if there are multiple scenario years with an assessment, choose the year of interest 
#' @param commit  commit that the current scores are compared to ('previous' for last commit or 7 digit SHA, e.g., '4da6b4a')
#' @param file_name  descriptive name used to save file outputs
#' @param save_csv  TRUE/FALSE, to save a csv file of the data
#' @param save_png  TRUE/FALSE, save static png file of interactive plot
#' @param NA_compare  TRUE/FALSE, compares the NA values between the datasets
#'
#' @return Returns an interactive html plot of the change in scores (current score - previous score) for all score 
#'         dimensions and goals/subgoals.  Other outputs can also be saved.
#'#' 
#'
#' @keywords ohicore
#' @examples
#' \dontrun{
#' } 
#' @export


score_check = function(scenario_year, commit="previous", 
                       file_name, save_csv=FALSE, save_png=FALSE, NA_compare=TRUE){
  
  cat("Wait for it....this takes a few seconds \n\n")
  
  path_components <- unlist(strsplit(getwd(), "/"))
  scenario_name <- path_components[length(path_components)]
  repo_name <- path_components[length(path_components) -1]
  repo_path <- paste(path_components[1:(length(path_components)-1)], collapse = '/')
  scenario_path <- paste(path_components[1:(length(path_components))], collapse = '/')
  
  # get commit SHA
  if(commit=="previous"){
    commit2 = substring(git2r::commits(git2r::repository(repo_path))[[1]]@sha, 1, 7)
  } else{
    if (commit == "final_2014"){
      commit2 = '4da6b4a'
    } else {commit2 = commit}
  }
  
  
  # Get repository name
  tmp <- git2r::remote_url(git2r::repository(repo_path))
  org <- stringr::str_split(tmp, "/")[[1]][4]
  
  
  # get data from previous commit
  data_old <- read.csv(file.path("https://raw.githubusercontent.com", org, repo_name, commit2, scenario_name, "scores.csv")) %>%
    dplyr::rename(old_score=score) 
  
  # create dummy year variable if there is no year variable in the data
  if(sum(names(data_old)=="year") < 1){
    
    data_new <- read.csv("scores.csv") %>%
      dplyr::left_join(data_old, by=c('goal', 'dimension', 'region_id')) %>%
      dplyr::mutate(year = substring(date(), 21, 24)) %>%  # uses current year as year
      dplyr::mutate(change = score-old_score)
    
    scenario_year <- substring(date(), 21, 24)
    
  } else{
    data_new <- read.csv("scores.csv") %>%
      dplyr::left_join(data_old, by=c('year', 'goal', 'dimension', 'region_id')) %>%
      dplyr::mutate(change = score-old_score)
    
  }
  
  ## get region names, if available (this needs to be called "regions_list" and located in the "spatial" folder)
  if(length(list.files("spatial", pattern="regions_list.csv"))>0){
    
    rgns <- read.csv("spatial/regions_list.csv", stringsAsFactors = FALSE) %>%
      dplyr::select(region_id = rgn_id, rgn_name)
    
    data_new <- data_new %>%
      dplyr::left_join(rgns, by="region_id") %>%
      dplyr::mutate(rgn_name = ifelse(region_id == 0, "Region", rgn_name))
  } else{
    data_new$rgn_name = ""
  }
  
  suppressWarnings(
    p <- ggplot2::ggplot(filter(data_new, year==scenario_year), aes(x=goal, y=change, color=dimension)) +
      #geom_point(shape=19, size=1) +
      theme_bw() + 
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
      labs(title=paste("Score compared to commit:", commit, sep=" "), y="Change in score", x="") +
      scale_x_discrete(limits = c("Index", "AO", "SPP", "BD", "HAB", "CP", "CS", "CW", "FIS", "FP", 
                                  "MAR", "ECO", "LE", "LIV", "NP", "LSP", "SP", "ICO", "TR")) +
      scale_colour_brewer(palette="Dark2") +
      geom_jitter(aes(text=paste0("rgn = ", region_id, "\n", rgn_name)), position = position_jitter(width=0.2, height=0), shape=19, size=1)
  )
  
  plotly_fig <- plotly::ggplotly(p, width = 800, height = 450)
  htmlwidgets::saveWidget(plotly::as_widget(plotly_fig), "tmp_file.html", selfcontained=TRUE)
  
  # Function to save files in particular place  
  my.file.rename <- function(from, to) {
    todir <- dirname(to)
    if (!isTRUE(file.info(todir)$isdir)) dir.create(todir, recursive=TRUE)
    file.rename(from = from,  to = to)
  }
  
  my.file.rename(from = "tmp_file.html",
                 to = file.path('score_check', paste0(file_name, "_score_check_", Sys.Date(), '.html')))
  
  cat("An interactive plot in the 'score_check' folder has been created \n")
  
  if(save_png){
    ggplot2::ggsave(file.path('score_check', paste0(file_name, "_check_plot_", Sys.Date(), '.png')), width=8, height=5)
    cat("A png plot has been saved in the 'score_check' folder \n")
  }
  
  if(save_csv){
    write.csv(data_new, file.path('score_check', paste0(file_name, "_diff_data_", Sys.Date(), '.csv')), row.names=FALSE)
    cat("A csv file comparing the scores has been saved in the 'score_check' folder \n")
  }
  
  if(NA_compare){
    data_NA <- data_new %>%
      filter(year == scenario_year) %>%
      mutate(NA_same = ifelse(is.na(score) & is.na(old_score), 1, 0)) %>%
      mutate(NA_new = ifelse(is.na(score), 1, 0)) %>%
      mutate(NA_old = ifelse(is.na(old_score), 1, 0)) %>%
      mutate(diff_new = NA_new - NA_same) %>%
      mutate(diff_old = NA_old - NA_same) %>%
      summarize(new = sum(diff_new),
                old = sum(diff_old))
    
    cat("\n NA check results: \n")
    
    if(sum(data_NA) == 0){
      cat(sprintf("Excellent! The number of NA values in %s has not changed! \n", scenario_year))
    } else{
      cat(sprintf("The new version of data has an additional %s missing values compared to the previous version \n
                  The previous version of data has an additional %s missing values compared to the new version \n
                  Examine the .csv file in the 'score_check' folder to determine where these discrepancies occur", 
                  data_NA$new, data_NA$old))
    }
  }
  
}

