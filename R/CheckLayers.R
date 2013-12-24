#' Check Layers
#' 
#' Check all the input layers as defined by layers.csv and update required fields
#' 
#' @aliases CheckLayers
#' @param layers.csv full path to the layers.csv file.
#' @param layers.dir full path to the directory containing the layers files.
#' @param flds_id character vector of unique identifiers, typically
#' spatial, eg c('region_id', 'country_id', 'saup_id'), described in your \code{\link{Conf}$layers_id_fields}.
#' @param if True (default), extra diagnostics are output
#' @return warning messages
#' @details The CheckLayers() function iterates through all the layers
#' in layers.csv and updates the following field names, which can be NA for any except flds:
#' \itemize{
#'  \item{\emph{fld_id_num} - name of field used as spatial identifier, if numeric}
#'  \item{\emph{fld_id_chr} - name of field used as spatial identifier, if character}
#'  \item{\emph{fld_category} - name of field used as category}
#'  \item{\emph{fld_year} - ame of field used as year}
#'  \item{\emph{fld_val_num} - name of field used as value, from fld_value, if numeric}
#'  \item{\emph{fld_val_chr} - name of field used as value, from fld_value, if character}
#'  \item{\emph{flds} - data fields used for the layer}
#' }
#' Additional diagnostic fields are updated:
#' \itemize{
#'   \item{\emph{file_exists} - input filename exists}
#'   \item{\emph{val_min} - minimum value, if numeric}
#'   \item{\emph{val_max} - maximum value, if numeric}
#'   \item{\emph{val_0to1} - TRUE if value ranges between 0 and 1}
#'   \item{\emph{flds_unused} - unused fields from input file when guessing prescribed field names (aboves)}
#'   \item{\emph{flds_missing} - fields expected, as given by Layers units, and not found}
#'   \item{\emph{rows_duplicated} - given the combination of all row-identifying fields (and excluding value fields), the number of rows which are duplicates}
#'   \item{\emph{num_ids_unique} - number of unique ids, as provided by just the unique instances of the fld_id}
#' }
#' @keywords layers
#' @examples
#' \dontrun{
#'   CheckLayers(layers.csv, layers.dir, c('rgn_id','cntry_key','saup_id'))
#' }
#' @export
CheckLayers = function(layers.csv, layers.dir, flds_id, verbose=T, msg.indent='  '){
  # for each layer listed in layers.csv, check for file in dir.layers,
  # and update layers.csv with information about the file's existence and identified fields for assembling into layers_data.csv
    
  # read in
  m = read.csv(layers.csv)
  
  # fields used by SelectLayer()
  m$fld_id_num     = NA
  m$fld_id_chr     = NA
  m$fld_category   = NA
  m$fld_year       = NA
  m$fld_val_num    = NA
  m$fld_val_chr    = NA

  # diagnostic fields
  m$file_exists     = F
  m$val_min         = NA
  m$val_max         = NA
  m$val_0to1        = NA
  m$flds_unused     = NA
  m$flds_missing    = NA
  m$rows_duplicated = NA
  m$num_ids_unique  = NA
  
  for (i in 1:nrow(m)){ # i=1
    
    # identify layer and read in file
    layer = m$layer[i]
    if (verbose) cat(sprintf('%s%s\n', msg.indent, layer,'\n'))
    path = file.path(layers.dir, m$filename[i])
    if (!file.exists(path)) {
      next
    }
    m$file_exists[i] = T
    d = read.csv(path, stringsAsFactors=F)
    #names(d) = tolower(names(d))
    
    # get field types
    fld_types = sapply(as.list(d), class)
    
    # id field
    idx.ids = which(tolower(names(d)) %in% flds_id)
    if (length(idx.ids)>0){
      # if more than one id field, then presume lookup table and get the id field entirely unique rows
      if (length(idx.ids)>1){
        fld_id = names(d)[idx.ids[lapply(as.list(d[,idx.ids]), anyDuplicated)==0]]
      } else {
        fld_id = names(d)[idx.ids]
      }
      
      # assign id field based on type
      if (fld_types[fld_id]=='character'){
        m$fld_id_chr[i] = fld_id
      } else {
        m$fld_id_num[i] = fld_id
      }
      
      # assign metadata check
      #if (layer=='cn_cntry_rgn') browser()
      m$num_ids_unique  = length(unique(d[[fld_id]]))
    }
    
    # units field
    fld_value = tolower(chartr('/ ','..', m$fld_value[i])) # translate slash or space to a single dot
    if (!fld_value %in% names(d)){
      m$flds_missing[i] = paste(m$flds_missing[i], fld_value)
    } else {
      if (fld_types[fld_value]=='character'){
        m$fld_val_chr[i] = fld_value
      } else {
        m$fld_val_num[i] = fld_value
        
        # add metadata checks
        m$val_min[i]  = min(d[[fld_value]], na.rm=T)
        m$val_max[i]  = max(d[[fld_value]], na.rm=T)
        m$val_0to1[i] = m$val_min[i] >=0 & m$val_max[i]<=1
      }
    }
    
    # year    
    if ('year' %in% names(d)) m$fld_year[i] = 'year'
    
    # get other fields not assigned
    flds_assigned = as.vector(na.omit(t(m[i, c('fld_id_num','fld_id_chr','fld_category','fld_year','fld_val_num','fld_val_chr')])))
    flds_other = setdiff(names(d), flds_assigned)
    
    # category - presume last remaining unidentified field
    #if (layer=='rnk_hab_presence') browser()
    if (length(flds_other>0)) m$fld_category[i] = flds_other[1]
    
    # check for any duplicated rows in layer, which forces the dcast to use length as the input value (BIG PROBLEM)
    n.rows.duplicated = sum(duplicated(d[,na.omit(unlist(m[i, c('fld_id_num','fld_id_chr','fld_category','fld_year')]))]))
    if (n.rows.duplicated>0) m$rows_duplicated[i] = n.rows.duplicated
    
    # still unassigned?    
    if (length(flds_other)>1){
      m$flds_unused[i] = paste(flds_other[-1], collapse=',')
    }
  }
  files.missing = subset(m, file_exists==F)
  if (nrow(files.missing)>0) warning(paste(c('Missing files...', sprintf('    %s: %s/%s', files.missing$layer, files.missing$directory, files.missing$filename)), collapse='\n'))
  flds.missing = subset(m, file_exists==T & !is.na(flds_missing))
  if (nrow(flds.missing)>0) warning(paste(c('Missing fields...', sprintf('    %s: %s', flds.missing$layer, flds.missing$flds_missing)), collapse='\n'))
  flds.unused = subset(m, file_exists==T & !is.na(flds_unused))
  if (nrow(flds.unused)>0) warning(paste(c('Unused fields...', sprintf('    %s: %s', flds.unused$layer, flds.unused$flds_unused)), collapse='\n'))  
  rows.duplicated = subset(m, file_exists==T & !is.na(rows_duplicated))
  if (nrow(rows.duplicated)>0) warning(paste(c('Rows duplicated...', sprintf('    %s: %s', rows.duplicated$layer, rows.duplicated$rows_duplicated)), collapse='\n'))
  write.csv(m, layers.csv, row.names=F, na='')
}