#' Report Scores
#' 
#' Generate report on calculated scores of the Ocean Health Index.
#' 
#' @param scenario list of (conf, layers, scores, spatial)
#' @param directory output directory of report
#' @param filename output filename of report (should be html, pdf implementation later)
#' @return Returns path to report
#' @seealso \code{\link{Conf}}, \code{\link{Layers}} and \code{\link{scores}}
#' @details You'll need the \href{http://www.imagemagick.org}{ImageMagick} program, especially the \code{convert} executable.  On Windows, you can try installing with the following
#' \code{if (!require('installr')) install.packages('installr')
#' # installr::install.ImageMagick() # doesn't seem to work
#' installr::install.URL('http://www.imagemagick.org/download/binaries/ImageMagick-6.8.7-10-Q16-x86-dll.exe')}
#' @keywords ohi report
#' @export
ReportScores = function(scenario = list(conf   = ohicore::conf.Global2013.www2013, 
                                        layers = ohicore::layers.Global2013.www2013, 
                                        scores = ohicore::scores.Global2013.www2013,
                                        spatial = ifelse(file.exists(system.file('extdata/spatial.www2013', package='ohicore')),
                                                         system.file('extdata/spatial.www2013', package='ohicore'),
                                                         system.file('inst/extdata/spatial.www2013', package='ohicore'))),
                        directory = path.expand('~/myohi/scenario.Global2013.www2013/reports'),
                        filename = 'report_Global2013_www2013.html', 
                        # options:
                        open_html=T, overwrite=F, global_only=T,
                        # include:
                        do_flowers=T, do_tables=T, 
                        # to implement...
                        do_maps=T, do_histograms=T, do_paths=T, do_equations=T, 
                        debug=F, ...){
  

  require(knitr); require(markdown)
  #setwd('~/Code/ohicore'); load_all(); launchApp()

  # TODO: read scenario.R for this info
  conf        = scenario$conf
  layers      = scenario$layers
  scores      = scenario$scores
  dir_spatial = scenario$spatial
  
  dir_report  = directory # file.path(dir_scenario, 'reports')
  f_report    = filename  # TODO: pdf

  # report template
  f_rmd = ifelse(file.exists(system.file(     'report/report.Rmd', package='ohicore')),
                             system.file(     'report/report.Rmd', package='ohicore'),
                             system.file('inst/report/report.Rmd', package='ohicore'))

  # create directories
  dir.create(dir_report, recursive=T, showWarnings=F)

  # output files
  f      = tools::file_path_sans_ext(f_report)
  f_md   = sprintf('%s/%s.md'  , dir_report, f)
  f_html = sprintf('%s/%s.html', dir_report, f)
  f_pdf  = sprintf('%s/%s.pdf' , dir_report, f)

  # knit ----
  # global_only=F; overwrite=T
  knitr::knit(f_rmd, f_md)
  markdown::markdownToHTML(f_md, f_html, options=c('hard_wrap','use_xhtml','smartypants','toc'))
  if (open_html) browseURL(f_html)
  
#   if (.Platform$OS.type == 'windows'){
#     system(sprintf('cd %s; pandoc -s --toc %s -o %s', dirname(f_md), f_md, f_pdf)); shell.exec(f_pdf)
#   } else if (.Platform$OS.type == 'unix'){
#     system(sprintf('cd %s; pandoc -s --toc %s -o %s', dirname(f_md), f_md, f_pdf)); system(sprintf('open %s', f_pdf))
#   }
  
}