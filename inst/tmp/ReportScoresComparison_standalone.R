devtools::load_all()

# NOTE: if get "Error in rawToChar(ret) : embedded nul in string: ..." then delete output md file and re-run.

# paths ----
dir_conf = list(
  'amphitrite'=list(  # BB's Windows 8 on MacBook Pro VMWare
    annex = 'Z:/bbest On My Mac/neptune_cyberduck'),
  'salacia'=list(  # BB's Mac
    data  = '/Volumes/data_edit',
    local = '/Volumes/local_edit',
    annex = '/Volumes/data_edit/git-annex'),
  'beastie3'=list(  # Melanie's Windows 8 on MacBook Pro VMWare
    data  = '//neptune.nceas.ucsb.edu/data_edit',
    local = '//neptune.nceas.ucsb.edu/local_edit',
    annex = '//neptune.nceas.ucsb.edu/data_edit/git-annex')
)[[tolower(sub('\\..*', '', Sys.info()['nodename']))]]

# variables ----
do.Radical.to.scores = F
dir_rad    = file.path(dir_conf$local, 'src/toolbox/scenarios/global_2013a/results')
dir_scores = file.path(dir_conf$annex, 'Global/NCEAS-OHI-Scores-Archive/scores')
scenarios = list(
  '2012' = c(
    scores.csv     = sprintf('%s/scores.Global2012.www2013_2014-04-02.csv', dir_scores),
    scores_old.csv = sprintf('%s/scores.Global2012.www2013_2013-10-08.csv', dir_scores)),
  '2013' = c(
    scores.csv     = sprintf('%s/scores.Global2013.www2013_2014-04-02.csv', dir_scores),
    scores_old.csv = sprintf('%s/scores.Global2013.www2013_2013-10-08.csv', dir_scores)))
dir_report  = file.path(dir_conf$annex, 'Global/NCEAS-OHI-Scores-Archive/reports')
html        = 'Comparison_Global_2014-04-02_vs_2013-10-08.html'
title       = 'Comparison Global: 2014-04-02 vs 2013-10-08'
changelog   = '
* 2014-04-02: fixed pressures rescaling ([issue #48](https://github.com/OHI-Science/ohicore/issues/48))\n
...
* 2014-10-08: handed results to Radical for website\n'
#cat('\nDiscussion. Major differences for 2012 with LIV / ECO / LE score seem OK b/c of ECO Eritrea and other LIV substitutions.\n')
open_html = T
rgn_labels = read.csv('inst/extdata/layers.Global2013.www2013/rgn_labels.csv') %.%
  select(region_id=rgn_id, region_label=label) %.%
  rbind(data.frame(region_id    = 0,
                   region_label = 'GLOBAL'))

# convert Radical to scores ----
if (do.Radical.to.scores){  
  for (f in list.files(dir_rad, pattern='OHI_results_for_Radical_[0-9-]+\\.csv$')){ # f = list.files(dir_rad, pattern='OHI_results_for_Radical_[0-9-]+\\.csv$')[1] # f='OHI_results_for_Radical_2013-12-13.csv'
    f_date = stringr::str_extract(f, '[0-9-]+')  
    
    d = read.csv(file.path(dir_rad, f), na.strings='')
    if ('likely future state' %in% d$dimension){
      d = mutate(d, dimension = revalue(dimension, c('likely future state'='future')))
    } else {
      d = mutate(d, dimension = revalue(dimension, c('likely_future_state'='future')))
    }
    
    for (yr in unique(d$scenario)){
      d_yr = d %.%
        filter(scenario==yr) %.%
        select(goal, dimension, region_id, score=value)
      write.csv(d_yr, sprintf('%s/scores.Global%d.www2013_%s.csv', dir_scores, yr, f_date))
    }  
  }
}
  
# files ----
f_rmd = ifelse(
  file.exists('inst/report/report_compare.Rmd'),
  'inst/report/report_compare.Rmd',
  system.file('report/report_compare.Rmd', package='ohicore'))

# output files
f_html = sprintf('%s/%s', dir_report, html)
f      = tools::file_path_sans_ext(html)
f_md   = sprintf('%s/%s.md' , dir_report, f)
f_csv  = sprintf('%s/%s.csv', dir_report, f)

# knit ----
if (file.exists(f_md)) unlink(f_md)
knitr::knit(f_rmd, f_md)
markdown::markdownToHTML(f_md, f_html, options=c('hard_wrap','use_xhtml','smartypants','toc'))
if (open_html) browseURL(f_html)
