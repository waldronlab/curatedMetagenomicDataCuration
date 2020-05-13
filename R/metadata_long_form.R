#' read a metadata tsv file and convert to long form
#' 
#' This is basically an auxilliary function for converting
#' datasets from the wide-format metadata to long-form, or "tidy"
#' format.
#' 
#' @param fname character(1) giving a path to one of the 
#' metadata.tsv files.
#' 
#' @importFrom dplyr select_if %>% all_of bind_rows bind_cols
#' @importFrom tidyr pivot_longer
#' 
#' @author Sean Davis <seandavi@gmail.com>
#' 
#' @return 
#' a 6-column dataframe
#' 
#' @seealso metadata_long_form()
#' 
#' @export
study_long_form = function(fname) {
  id_cols =c('sampleID', 'subjectID')
  df = readr::read_tsv(fname, col_types = readr::cols())
  if(!('subjectID' %in% colnames(df))) {
    df$subjectID='NA'
  }
  df$subjectID=as.character(df$subjectID)
  df$sampleID=as.character(df$sampleID)
  df1 = df[,id_cols]
  df = df[,!(colnames(df) %in% id_cols)]
  df_ret = list()
  if(sum(sapply(df,is.numeric)))
    df_ret[['numeric']] = df %>% 
      dplyr::select_if(is.numeric) %>% 
      dplyr::bind_cols(df1) %>%
      tidyr::pivot_longer(-all_of(id_cols), names_to='variable',values_to = 'numeric_value')
  if(sum(sapply(df,is.logical)))
    df_ret[['logical']] = df %>% 
    dplyr::select_if(~ is.logical(.) & !is.character(.)) %>% 
    dplyr::bind_cols(df1) %>%
    tidyr::pivot_longer(-all_of(id_cols), names_to='variable',values_to = 'logical_value')
  
  if(sum(sapply(df,is.character)))
    df_ret[['character']] = df %>% 
    dplyr::select_if(is.character) %>% 
    dplyr::bind_cols(df1) %>%
    tidyr::pivot_longer(-all_of(id_cols), names_to='variable',values_to = 'string_value')
  dplyr::bind_rows(df_ret)
}

#' convert all metadata.tsv files in package to long form
#' 
#' This function runs through the metadata.tsv files in 
#' the installed package, running `study_long_form()` on
#' each, then binds them together, appending a `study` column.
#' 
#' @importFrom dplyr bind_rows
#' 
#' @author Sean Davis <seandavi@gmail.com>
#' 
#' @return
#' a data.frame with the following columns:
#' 
#' - study
#' - sampleID
#' - subjectID
#' - variable: the column name from the wide-form data
#' - numeric_value: if a numeric value, this value will be non-NA
#' - string_value: if a character value, this value will be non-NA
#' - logical_value: if a logical value, this value will be non-NA
#' 
#' @examples 
#' ret = metadata_long_form()
#' dim(ret)
#' ret
#' table(ret$study)
#' summary(ret)
#' 
#' @export
metadata_long_form <- function() {
  avail_metadata_tsv = list.files(system.file('curated', package='curatedMetagenomicDataCuration'),
                                  full.names = TRUE,
                                  recursive = TRUE,
                                  pattern='*metadata.tsv')
  studies = sub('_metadata\\.tsv', '', sub('.*curated/.*/','',avail_metadata_tsv))
  metadata_list = lapply(avail_metadata_tsv, study_long_form)
  names(metadata_list) = studies
  return(dplyr::bind_rows(metadata_list, .id='study'))
}
