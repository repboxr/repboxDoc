example = function() {
  library(repboxDoc)
  project_dirs = repboxExplore::get_project_dirs("~/repbox/projects_share")
  df = rdoc_find_in_text_fixed(project_dirs,pattern="edicaid")
  df$project_dir
}

rdoc_find_in_text_fixed = function(project_dirs, pattern,  doc_type="art", doc_file_form_pref = c("mocr_md","html","pdf_txt")) {
  restore.point("rdoc_find_in_text")
  doc_df = repbox_doc_file_select(project_dirs,doc_type =doc_type,doc_file_form_pref = doc_file_form_pref)
  doc_files = doc_df$doc_file

  doc_df$has_pattern = sapply(doc_df$doc_file, function(file) {
    txt = merge.lines(readLines(file))
    stri_detect_fixed(txt, pattern)
  })
  doc_df  = doc_df %>% filter(has_pattern)
  doc_df
}
