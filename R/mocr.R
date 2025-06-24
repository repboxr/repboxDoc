example = function() {
  liberar(repboxDoc)
  project_dir = "~/repbox/projects_share/aejapp_1_2_4"
  project_dir = "~/repbox/projects_share/aeri_1_2_6"
  project_dir = "~/repbox/projects_share/qje_3036349"

  repbox_process_all_docs(project_dir, just_doc_form = "mocr")


  repbox_process_all_docs(project_dir, just_doc_type="art", just_doc_form = "mocr")
  rstudioapi::filesPaneNavigate(project_dir)
}

change_file_ext = function(file, new_ext) {
  paste0(tools::file_path_sans_ext(file), ".", new_ext)
}

rdoc_mocr_process = function(doc_dir, ...) {
  restore.point("rdoc_mocr_process")
  repboxUtils::repbox_set_problem_options(project_dir = doc_dir_to_project_dir(doc_dir))
  #stop()
  ocr = readRDS(file.path(doc_dir, "ocr.Rds"))
  pages = ocr$pages
  num_pages = NROW(pages)
  if (num_pages==0) return(NULL)

  library(rmarkdown)
  library(rmistral)
  pages = try(mocr_md_to_html_mono(doc_dir, ocr), silent=TRUE)
  if (is(pages, "try-error")) {
    cat("\nMonolithic pandoc conversion md to html did not work for ", doc_dir, ". Try page-by-page conversion\n")
    pages = mocr_md_to_html_by_page(doc_dir, ocr)
  }


  pages$page = 1:NROW(pages)

  page_df = pages[,c("page","markdown","html")]
  page_df = rename.cols(page_df, "markdown", "md")
  page_df$img_vec = lapply(pages$images, function(image_df) {
    if (NROW(image_df)==0) return("")
    paste0(image_df$id, collapse=",")
  })

  page_df = page_df %>%
    mutate(
      nchar_md = nchar(md),
      page_start_md = nchar_md,
      page_end_md = cumsum(nchar_md),

      nchar_html = nchar(html),
      page_start_html = nchar_html,
      page_end_html = cumsum(nchar_html),
    ) %>%
    mutate(
      page_start_md = c(1, page_end_md[-n()]+1),
      page_start_html = c(1, page_end_md[-n()]+1)
    )

  page_df$num_table_frac = stri_count_fixed(page_df$html, "<table")
  saveRDS(page_df, file.path(doc_dir, "page_df.Rds"))


  html = paste0(page_df$html, collapse = "\n")
  tab_df = mocr_html_extract_tables(html)
  saveRDS(tab_df, file.path(doc_dir, "tab_df.Rds"))

  part_df = mocr_parse_html_parts(page_df)
  saveRDS(part_df, file.path(doc_dir, "part_df.Rds"))


  md_file = paste0(doc_dir,"/doc.md")
  rmistral::mistral_ocr_save_md(ocr, md_file,by_page = FALSE, overwrite=TRUE,save_images = TRUE)
  html_file = paste0(doc_dir,"/doc.html")
  my_pandoc(md_file,html_file,c("--wrap=preserve", "--mathjax", "--standalone"))
  #pandoc_convert(md_file,output=html_file, options = c("--wrap=preserve", "--mathjax", "--standalone"))

  #tab_html = cells_to_tabhtml(bind_rows(tab_df$cell_df))$tab_html
  tab_html = tab_df$tabhtml
  rai_write_all_tables_html(tab_df,file.path(doc_dir, "tabs.html"))
  #writeUtf8(tab_html, file.path(doc_dir, "tabs.html"))
  #rstudioapi::filesPaneNavigate(doc_dir)

  return(invisible(list(ocr=ocr, page_df=page_df, tab_df=tab_df)))
}

mocr_md_to_html_mono = function(doc_dir, ocr) {
  pages = ocr$pages
  page_marker = paste0("<p>§°pAGe°§</p>")
  page_marker_html = paste0("<p>\n§°pAGe°§\n</p>")
  md = paste0(page_marker, pages$markdown, collapse="\n")
  md_file = file.path(doc_dir, "doc_page_marker.md")
  writeUtf8(md, md_file)
  html_file = change_file_ext(md_file, "html")
  my_pandoc(md_file,html_file, args = c("--wrap=preserve","--mathjax"))
  html = merge.lines(readLines(html_file))
  pos_md = stri_locate_all_fixed(md, page_marker)[[1]]

  pos = stri_locate_all_fixed(html, page_marker_html)[[1]]
  start_positions <- pos[, 2] + 1
  end_positions   <- c(pos[-1, "start"] - 1, nchar(html))

  pages$html = stri_sub(html, from = start_positions, to = end_positions)
  pages

}

# This more robust function will be used if
# mocr_md_to_html_mono does not work
mocr_md_to_html_by_page = function(doc_dir, ocr) {
  restore.point("mocr_md_to_html_by_page")
  pages = ocr$pages
  pages_dir = file.path(doc_dir, "pages")
  if (!dir.exists(pages_dir)) dir.create(pages_dir)
  end_marker = paste0("\n<p><endofpage></endofpage></p>")
  end_marker_html = paste0("<p>[ \n]*<endofpage>")
  p = 1
  htmls = unlist(lapply(seq_len(NROW(pages)), function(p) {
    md = paste0(pages$markdown[p], end_marker)
    md_file = file.path(pages_dir, paste0("page",p,".md"))
    writeUtf8(md, md_file)
    html_file = change_file_ext(md_file, "html")
    my_pandoc(md_file,html_file, args = c("--wrap=preserve","--mathjax"))
    html = merge.lines(readLines(html_file))
    pos = stri_locate_all_regex(html, end_marker_html)[[1]]
    if (NROW(pos)>1) {
      msg = paste0("Multiple end page markers found when convering mocr markdown page ", p, " of ", doc_dir, " to html.")
      repbox_problem(msg, "mocr_md_html_multi_end",fail_action = "msg")
    }
    if (NROW(pos)>=1) {
      html = stri_sub(html, 1,pos[1,1]-1)
    }
    html
  }))
  pages$html = htmls
  pages
}


mocr_html_extract_tables = function(html) {
  restore.point("mocr_html_extract_tables")
  txt = html
  library(repboxArt)

  # Find table positions
  pattern <- "(?s)(<table\\b[^>]*>.*?</table>)"
  tab_pos = stringi::stri_locate_all_regex(txt, pattern,
                                           omit_no_match = TRUE, opts_regex = list(case_insensitive = TRUE)
  )[[1]]

  pattern <- "(^|[\n]|<p>)[ \t]*((Table)|(TABLE))"
  # Find all matches with their positions
  tit_pos = stringi::stri_locate_all_regex(txt, pattern, omit_no_match = TRUE)[[1]]

  # Candidates for table titles and table fragments
  pos_df = bind_rows(
    as.data.frame(tab_pos) %>% mutate(type="tab"),
    as.data.frame(tit_pos) %>% mutate(type="tit")
  ) %>%
    arrange(start) %>%
    filter(type == "tab" | type=="tit" & lead(type)=="tab") %>%
    # mutate(dist_to_next = lead(start)-end) %>%
    # title or panel titles should not be more than 1000 characters
    # filter(dist_to_next <= 1000) %>%
    mutate(
      merge_above = is.true(lag(type)=="tab"),
      title_start = case_when(
        type == "tab" & lag(type) == "tab" ~ lag(end)+1L,
        type == "tab" & lag(type) == "tit" ~ lag(start),
        TRUE ~ NA_integer_
      ),
      tabtitle = stri_sub(txt, title_start, start-1) %>%
        stri_replace_all_fixed("<p>","") %>%
        stri_replace_all_fixed("</p>",""),
      title_dist = start-title_start
    )


  tab_df = pos_df %>%
    filter(type=="tab") %>%
    mutate(
      tabname = tabtitle_to_tabname(tabtitle),
      tabid = tabname_to_tabid(tabname)
    ) %>%
    mutate(merge_above = merge_above |
      is.true(lag(tabid)==tabid) |
      (is.true(has.substr(tabtitle, "ontinue") | is.na(tabtitle)) & is.true(start-lag(end) < 300))
    ) %>%
    mutate(raw_tabhtml = stri_sub(txt, start, end) %>%
             stri_replace_all_fixed("\u2003", "")
    )

  tab_df = tab_df %>%
    mutate(tabhtml = sapply(seq_along(raw_tabhtml),function(i) {
      html_table_add_cellnum_row_col(raw_tabhtml[i], tabid=tabid[i])
    })) %>%
    mutate(parent_row = cumsum(1L-merge_above)) %>%
    mutate(cell_df = lapply(tabhtml, normalized_html_tab_to_cell_df)) %>%
    group_by(parent_row) %>%
    mutate(
      num_panels = n(),
      panel_pos = 1:n()
    ) %>%
    ungroup()

  # cat(substr(txt, tab_df$start[1], tab_df$end[1]))


  # Merge tables
  merge_df = tab_df %>%
    filter(num_panels > 1)

  if (NROW(merge_df)>0) {
    merge_df = merge_df %>%
      group_by(parent_row) %>%
      summarize(
        cell_df = list(cell_df_join(cell_df, tabtitle))
      )
    for (i in 1:NROW(merge_df)) {
      tab_df$cell_df[[merge_df$parent_row[i] ]] = merge_df$cell_df[[i]]
    }
  }
  tab_df = tab_df %>% filter(panel_pos==1) %>%
    ungroup() %>%
    mutate(
      tabname = tabtitle_to_tabname(tabtitle),
      tabid = tabname_to_tabid(tabname)
    )
  for (i in 1:NROW(tab_df$cell_df)) {
    tab_df$cell_df[[i]]$tabid = tab_df$tabid[[i]]
  }

  # Correct tables
  tab_df$cell_df = lapply(tab_df$cell_df, function(cell_df) {
    restore.point("shfjhsfk")
    cell_df$content = cell_df$inner_html
    row_df = split_html_latex_multiline(cell_df$content)
    cell_df = cell_df_split_rows_and_cols(cell_df, row_df)
    cell_df$text = cell_df$content
    cell_df = cells_add_cell_base(cell_df,add_tests = TRUE, split_multi_num = TRUE)
    cell_df
  })


  tab_df$tabhtml = sapply(tab_df$cell_df, cell_df_to_simple_tabhtml)

  num_na = sum(is.na(tab_df$tabid))
  if (num_na > 0) {
    msg = paste0("mocr table extraction ommited ", num_na, " tables for which no tabid could be identified. ", NROW(tab_df)-num_na, " tables successfully extracted.")
    repbox_problem(msg, type="mocr_na_tabid",fail_action = "msg")
    tab_df = tab_df %>% filter(!is.na(tabid))
  }

  #tab_df$tabhtml = sapply(tab_df$cell_df, cell_df_to_simple_tabhtml)
  tab_df
}


cell_df_join = function(cell_li, panel_titles = NULL) {
  if (length(cell_li)==1) return(cell_li[[1]])
  restore.point("cell_df_join")
  cell_df = cell_li[[1]]

  # Add panel titles as cell
  panel_inds = setdiff(seq_along(cell_li),1)
  if (!is.null(panel_titles)) {
    max_col = max(sapply(cell_li, function(cell_df) max(cell_df$col)))
    for (i in panel_inds) {
      text = panel_titles[i]
      cell_df = bind_rows(
        tibble(cellid="cell-0", row=0, col=1, colspan=max_col, rowspan=1,inner_html=text, text=text),
        cell_li[[i]]
      )
      cell_df$row = cell_df$row +1
      cell_li[[i]] = cell_df
    }
  }
  cell_df = cell_li[[1]]
  cell_df$panel_num = 1
  max_row = max(cell_df$row)
  for (i in panel_inds) {
    new_cell_df = cell_li[[i]]
    new_cell_df$panel_num = i
    new_cell_df$row = new_cell_df$row + max(cell_df$row)
    cell_df = bind_rows(cell_df, new_cell_df)
  }
  cell_df$cellid = paste0("cell-",1:NROW(cell_df))
  cell_df

}

my_pandoc = function(input, output, args, stdout = FALSE, stderr=FALSE) {
  restore.point("my_pandoc")
  all_args = c(input, "-o", output, args)
  system2("pandoc", args = all_args, stdout = stdout, stderr = stderr)
  #cat(paste0("\npandoc ", input, " -o ", output, " ", paste0(args, collapse=" ")))
}


mocr_parse_html_parts = function(page_df, journ=NULL) {
  restore.point("mocr_parse_html_parts")
  library(rvest)
  library(xml2)
  html_txt = paste0(page_df$html, collapse="\n")
  body = rvest::read_html(html_txt)

  nodes = body %>% rvest::html_elements("h1, h2, h3, p")

  df = tibble(tag = xml_name(nodes), xpath=xml_path(nodes), classes = html_attr(nodes,"class"),  text = html_text2(nodes), id = html_attr(nodes,"id"))

  df = df %>%
    mutate(
      type = case_when(
        startsWith(text, "Abstract") ~ "abs",
        tag == "p" ~ "p",
        tag == "h1" ~ "sec1",
        tag == "h2" ~ "sec2",
        tag == "h3" ~ "sec3",
        tag == "span" ~ "note",
        TRUE ~ "unknown"
      )
    )

  # Removes duplicated table entries
  df$.ROW = 1:NROW(df)
  dupl = duplicated(select(df, tag, type, text))
  df = df[!dupl,]
  df = html_text_part_df_standardize(df)
  df
}

example_mocr_make = function() {
  # Can run as an rstudio job

  library(repboxDoc)
  library(rmistral)
  library(repboxAI)
  project_dirs = list.dirs("~/repbox/projects_share",recursive = FALSE)

  project_dirs = list.dirs("~/repbox/projects_gha",recursive = FALSE)

  project_dirs = "/home/rstudio/repbox/projects_gha_new/aejapp_1_2_4"

  set_mistral_api_key(file = "~/repbox/gemini/mistral_api_key.txt")
  mod_df = rmistral::mistral_list_models()

  project_dir = project_dirs[1]
  for (project_dir in project_dirs) {
    doc_dirs = repbox_doc_dirs(project_dir, "pdf")
    doc_dir = doc_dirs[1]
    for (doc_dir in doc_dirs) {
      mocr_make_ocr(project_dir, doc_dir)
    }
  }



}

mocr_make_ocr = function(project_dir, doc_dir) {
  restore.point("mocr_make_ocr")
  artid = basename(project_dir)
  doc_type = rdoc_type(doc_dir)
  pdf_file = rdoc_pdf_file(doc_dir)
  #if (!isTRUE(file.exists(pdf_file))) return(NULL)

  tmp_pdf_file = "~/web/repbox_temp/temp_art.pdf"
  file.copy(pdf_file,tmp_pdf_file,overwrite = TRUE)
  url = "https://econ.mathematik.uni-ulm.de/repbox_temp/temp_art.pdf"
  cat(paste0("\nAttempt mistral ocr for ", artid," ", basename(doc_dir), " ... "))
  ocr = try(mistral_ocr(url,timeout_sec = 180,include_images = TRUE))
  if (is(ocr, "try-error")) return(NULL)
  if (mistral_is_ok(ocr) & isTRUE(NROW(ocr$pages)>0)) {
    outdir = file.path(dirname(doc_dir), paste0(doc_type, "_mocr"))
    if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)
    outfile = file.path(outdir, "ocr.Rds")
    saveRDS(ocr, outfile)
    cat("ok: ", NROW(ocr$pages), " pages.\n")
    mocr_copy_to_ejs(project_dir)
  } else {
    cat(" not ok.\n")
  }
}

mocr_copy_to_ejs = function(project_dir, overwrite=FALSE) {
  library(EconJournalScrap)
  doc_dirs = repbox_doc_dirs(project_dir, doc_form = "mocr", doc_type="art")
  if (length(doc_dirs)<1) return()

  artid = basename(project_dir)
  journ = ejs_artid_to_journ(artid)
  doc_dir = doc_dirs[1]
  for (doc_dir in doc_dirs) {

    dest_dir =  file.path("~/ejd_files", basename(doc_dir), journ)
    dest_file = file.path(dest_dir, paste0(artid, ".Rds"))
    if (file.exists(dest_file) & !overwrite) next
    if (!dir.exists(dest_dir)) dir.create(dest_dir)

    source_file = file.path(doc_dir, "ocr.Rds")
    file.copy(source_file, dest_file)
  }
}

repair_ejd_files_art_mocr = function() {
  files = list.files("~/ejd_files/art_mocr", glob2rx("*.Rds"),full.names = TRUE)
  artids = basename(files) %>% tools::file_path_sans_ext()
  journs = ejs_artid_to_journ(artids)
  new_files = file.path(dirname(files), journs, basename(files))
  dirs = unique(dirname(new_files))
  for (dir in dirs) {
    dir.create(dir)
  }
  for (i in seq_along(files)) {
    file.rename(files[i], new_files[i])
  }
}

