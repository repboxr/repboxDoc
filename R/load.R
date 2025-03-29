example = function() {
  project_dirs = repboxExplore::get_project_dirs("~/repbox/projects_share")
  repbox_doc_files_info(project_dirs)
  sel_df = repbox_doc_file_select(project_dirs)
}

repbox_doc_file_select = function(project_dir, doc_type="art", doc_file_form_pref = c("html","pdf","mocr_md", "pdf_txt"), doc_form=NULL) {
  restore.point("repbox_doc_file_select")
  info_df = repbox_doc_files_info(project_dir, doc_type=doc_type, doc_form=doc_form)
  sel_df = info_df %>%
    filter(doc_file_form %in% doc_file_form_pref) %>%
    group_by(project_dir, doc_type) %>%
    mutate(
      rank = match(doc_file_form, doc_file_form_pref)
    ) %>%
    arrange(rank) %>%
    slice(1) %>%
    ungroup()
  sel_df
}

repbox_doc_files_info = function(project_dir,doc_form=NULL, doc_type=NULL) {
  restore.point("repbox_doc_files_info")
  doc_dirs = repbox_doc_dirs(project_dir,doc_form=doc_form, doc_type = doc_type)
  project_dirs = doc_dir_to_project_dir(doc_dirs)


  df = data.frame(artid = basename(project_dirs),doc_type = rdoc_type(doc_dirs),doc_form=rdoc_form(doc_dirs), doc_file_form = NA_character_, doc_file = NA_character_, doc_dir = doc_dirs, project_dir = project_dirs)

  pdf_df = df %>%
    filter(doc_form == "pdf") %>%
    mutate(doc_file = rdoc_pdf_file(doc_dir))

  html_df = df %>%
    filter(doc_form == "html") %>%
    mutate(doc_file = rdoc_html_file(doc_dir))

  other_df = df %>%
    filter(!doc_form %in% c("html","pdf"))

  info_df = bind_rows(
    pdf_df,
    pdf_df %>% mutate(doc_file = file.path(doc_dir, "doc.txt")),
    html_df,
    other_df %>% mutate(doc_file = file.path(doc_dir, "doc.md")),
    other_df %>% mutate(doc_file = file.path(doc_dir, "doc.html"))
  )
  info_df = info_df %>%
    filter(file.exists(info_df$doc_file)) %>%
    mutate(
      file_ext = tolower(tools::file_ext(doc_file)),
      doc_file_form = case_when(
        doc_form == file_ext ~ file_ext,
        TRUE ~ paste0(doc_form, "_", file_ext)
      )
    )
  info_df
}

repbox_doc_types = function(project_dir) {
  doc_dirs = repbox_doc_dirs(project_dir)
  unique(rdoc_type(doc_dirs))
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


rdoc_tabs_file = function(doc_dir) {
  restore.point("rdoc_load_tabs")
  file.path(doc_dir, "tab_df.Rds")
}

rdoc_load_with_cache = function(doc_dir, base_file, cache_field, cache=NULL) {
  if (!is.null(cache[[cache_field]])) return(cache[[cache_field]])
  file = file.path(doc_dir,base_file)
  if (!file.exists(file)) return(NULL)
  res = readRDS(file)
  if (!is.null(cache)) cache[[cache_field]] = res
  res
}


rdoc_load_ref_li = function(doc_dir, cache=NULL) {
  rdoc_load_with_cache(doc_dir, "ref_li.Rds", "ref_li", cache)
}



rdoc_load_page_df = function(doc_dir, cache=NULL) {
  rdoc_load_with_cache(doc_dir, "page_df.Rds", "page_df", cache)
}

rdoc_load_sent_df = function(doc_dir, cache=NULL) {
  rdoc_load_with_cache(doc_dir, "sent_df.Rds", "sent_df", cache)
}

rdoc_load_part_df = function(doc_dir, cache=NULL) {
  rdoc_load_with_cache(doc_dir, "part_df.Rds", "part_df", cache)
}

rdoc_load_tab_df = function(doc_dir) {
  rdoc_load_with_cache(doc_dir, "tab_df.Rds", "tab_df", cache)
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
