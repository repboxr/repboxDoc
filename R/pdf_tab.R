example = function(){
  doc_dir = "~/repbox/projects_reg/aejpol_3_4_8"
  doc_dir = "~/repbox/projects_reg/testart"
  doc_dir = "~/repbox/projects_reg/aejapp_3_1_3"
  doc_dir = "~/repbox/projects_reg/aejapp_3_2_2"

  rdoc_extract_raw_tabs(doc_dir, overwrite=TRUE, by_page=FALSE)
}

rdoc_pdf_extract_tabs = function(doc_dir, overwrite=!FALSE, by_page=FALSE, page_df=NULL) {
  restore.point("rdoc_extract_pdf_tabs")
  tab_file = rdoc_tabs_file(doc_dir)
  if (file.exists(tab_file) & !overwrite) return(list())

  if (is.null(page_df)) {
    page_df = rdoc_load_page_df(doc_dir)
  }
  if (is.null(page_df)) {
    cat("\nNo pages extracted for document ", doc_dir,"\n")
    return(list())
  }

  raw = rdoc_pdf_extract_raw_tabs(doc_dir, overwrite, by_page, page_df=page_df)

  tab_df = ExtractSciTab::refine_raw_tab(raw)

  #show_cell_df_html(tab_df$cell_df[[2]])

  # We overwrite the panel and num_row_block definitions
  # to be more aligned with other table extraction methods
  # e.g. from HTML or later using AI
  i = 1
  for (i in seq_len(NROW(tab_df))) {
    res = refine_cell_df_and_add_panel_info(tab_df$cell_df[[i]])
    tab_df$cell_df[[i]] = res$cell_df
    tab_df$row_df[[i]] = res$row_df
    tab_df$panel_df[[i]] = res$panel_df
  }


  tab_df$start_page = lines_to_pages(tab_df$start_line,page_df)
  tab_df$end_page = lines_to_pages(tab_df$end_line, page_df)
  tab_df$start_pline = lines_to_plines(tab_df$start_line, tab_df$start_page, page_df)
  tab_df$end_pline = lines_to_plines(tab_df$end_line, tab_df$end_page, page_df)

  pdf_url = rdoc_document_url(doc_dir)

  tab_df$pdf_file = page_df$source_file[tab_df$start_page]
  tab_df$html_file = ""
  if (!is_empty(pdf_url)) {
    tab_df$url_org_tab = paste0(pdf_url,"#page=", tab_df$start_page)
  } else {
    tab_df$url_org_tab = ""
  }

  save_rds_create_dir(tab_df, tab_file)
  tab_df
}

# Extract tabs using ExtractSciTab. Later we will modify it.
rdoc_pdf_extract_raw_tabs = function(doc_dir, overwrite=FALSE, by_page=FALSE, page_df=NULL, save=TRUE) {
  restore.point("rdoc_pdf_extract_raw_tabs")
  raw_file = file.path(doc_dir, "/raw_tabs.Rds")
  if (file.exists(raw_file) & !overwrite) return(readRDS(raw_file))

  if (is.null(page_df)) {
    page_df = rdoc_load_page_df(doc_dir)
  }
  if (is.null(page_df)) {
    cat("\nNo pages extracted for article in ", doc_dir,"\n")
    return(NULL)
  }
  raw = ExtractSciTab::extract_raw_tables_from_text(txt = merge.lines(page_df$txt))
  if (save) {
    saveRDS(raw,raw_file)
  }
  invisible(raw)
}


