repbox_doc_types = function(project_dir) {
  doc_dirs = repbox_doc_dirs(project_dir)
  unique(rdoc_type(doc_dirs))
}

repbox_art_pdf_file = function(project_dir) {
  doc_dirs = repbox_doc_dirs(project_dir, "pdf","art")
  rdoc_pdf_file(doc_dirs)
}

repbox_all_pdf_file = function(project_dir) {
  doc_dirs = repbox_doc_dirs(project_dir, "pdf")
  rdoc_pdf_file(doc_dirs)
}

repbox_pdf_file = function(project_dir, doc_type=NULL) {
  doc_dirs = repbox_doc_dirs(project_dir, "pdf", doc_type=doc_type)
  rdoc_pdf_file(doc_dirs)
}


repbox_doc_dirs = function(project_dir, doc_form=NULL, doc_type=NULL) {
  doc_dirs = list.dirs(file.path(project_dir, "doc"),recursive = FALSE)
  if (!is.null(doc_form)) {
    doc_forms = rdoc_form(doc_dirs)
    doc_dirs = doc_dirs[doc_forms %in% doc_form]
  }
  if (!is.null(doc_type)) {
    doc_types = rdoc_type(doc_dirs)
    doc_dirs = doc_dirs[doc_types %in% doc_type]
  }
  doc_dirs
}

doc_dir_to_project_dir = function(doc_dir) {
  dirname(dirname(doc_dir))
}

doc_dir_to_artid = function(doc_dir) {
  basename(dirname(dirname(doc_dir)))
}


rdoc_tabs_file = function(doc_dir) {
  restore.point("rdoc_load_tabs")
  file.path(doc_dir, "tab_df.Rds")
}

rdoc_load_tabs = function(doc_dir) {
  restore.point("rdoc_load_tabs")
  file = rdoc_tabs_file(doc_dir)
  if (file.exists(file)) return(readRDS(file))
  return(NULL)
}



rdoc_pdf_file = function(doc_dir, full.names=TRUE) {
  pdf_dir = file.path(doc_dir,"pdf")
  list.files(pdf_dir, glob2rx("*.pdf"),full.names = full.names)
}

rdoc_html_file = function(doc_dir, full.names=TRUE) {
  dir = file.path(doc_dir,"html")
  list.files(dir, glob2rx("*.htm*"),full.names = full.names)
}

rdoc_has_pdf = function(doc_dir) {
  length(rdoc_pdf_file(doc_dir))>1
}
rdoc_has_html = function(doc_dir) {
  length(rdoc_html_file(doc_dir))>1
}


rdoc_load_page_df = function(doc_dir) {
  restore.point("rdoc_load_page_df")
  out_file = file.path(doc_dir,"page_df.Rds")
  if (!file.exists(out_file)) return(NULL)
  readRDS(out_file)
}

rdoc_load_part_df = function(doc_dir) {
  file = file.path(get_rdoc_route_dir(doc_dir),"part_df.Rds")
  if (!file.exists(file)) return(NULL)
  text_df = readRDS(file)
  text_df
}

rdoc_load_tab_df = function(doc_dir) {
  file = file.path(get_rdoc_route_dir(doc_dir), "tab_df.Rds")
  if (file.exists(file)) return(readRDS(file))

  return(NULL)
}


rdoc_document_url = function(doc_dir) {
  project_dir = doc_dir_to_project_dir(doc_dir)
  art = readRDS.or.null(file.path(project_dir, "repdb","art.Rds"))[[1]]
  art$pdf_url
}

# Try to load meta data for article
# in order to generate link to original tables
rdoc_load_art_meta_data = function(doc_dir) {
  project_dir = doc_dir_to_project_dir(doc_dir)
  art = readRDS.or.null(file.path(project_dir, "repdb","art.Rds"))[[1]]
  art
}

rdoc_has_art_mocr = function(project_dir) {
  file.exists(file.path(project_dir, "doc","art_mocr", "ocr.Rds"))
}
