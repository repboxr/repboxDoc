example = function() {
  doc_dir = "~/repbox/projects_share/aeri_1_2_6/doc/art_pdf"
  doc_dir = "~/repbox/projects_share/jole_33_3_5/doc/art_html"
  doc_dir = "~/repbox/projects_share/aeri_1_2_6/doc/app1_pdf"
  doc_dir = "~/repbox/projects_share/aeri_1_2_6/doc/art_mocr"
  doc_dir = "~/repbox/projects_share/aeri_1_2_6/doc/app1_mocr"
  project_dir = "/home/rstudio/repbox/projects_share//pandp_108_1_111"

  project_dir = "~/repbox/projects_share/ecta_84_2_6"
  repbox_process_all_docs(project_dir, just_doc_form = "mocr")
  rstudioapi::filesPaneNavigate(project_dir)

  rdoc_process(doc_dir)
  rstudioapi::filesPaneNavigate(doc_dir)

  library(repboxDoc)
  project_dirs = list.dirs("~/repbox/projects_share/", recursive = FALSE)

  for (project_dir in project_dirs) {
    repbox_process_all_docs(project_dir, just_doc_form = "mocr")
  }

  # library(rmistral)
  # rmistral::set_mistral_api_key(file = "~/repbox/gemini/mistral_api_key.txt")
  # ocr = rmistral::mistral_ocr(file = "~/repbox/projects_share/aeri_1_2_6/doc/app1_pdf/pdf/10528.pdf")
  # saveRDS(ocr, "~/repbox/projects_share/aeri_1_2_6/doc/app1_mocr/ocr.Rds")


  dirs = list.dirs("~/repbox/projects_share")
  dirs = dirs[has.substr(dirs,"doc/app")]
  head(dirs)
}

repbox_process_all_docs = function(project_dir, steps= rdoc_steps_from(TRUE), opts = rdoc_options(), overwrite=TRUE, just_doc_type = NULL, just_doc_form=NULL, verbose=TRUE) {
  doc_dirs = repbox_doc_dirs(project_dir)
  for (doc_dir in doc_dirs) {
    if (!is.null(just_doc_type)) {
      doc_type = rdoc_type(doc_dir)
      if (!doc_type %in% just_doc_type) next
    }
    if (!is.null(just_doc_form)) {
      doc_form = rdoc_form(doc_dir)
      if (!doc_form %in% just_doc_form) next
    }
    if (verbose) {
      cat("\nproccess doc ", doc_dir,"\n")
    }
    rdoc_process(doc_dir, steps, opts, overwrite)
  }
}

rdoc_steps_from = function(pages = FALSE, tables = pages, parts = pages, sentences = parts, refs = sentences) {
  as.list(sys.frame(sys.parent(0)))
}

rdoc_process = function(doc_dir, steps= rdoc_steps_from(TRUE), opts = rdoc_options(), overwrite = TRUE, cache = new.env(parent=emptyenv())) {
  restore.point("rdoc_process")
  if (!overwrite) {
    if (rdoc_is_processed(doc_dir)) return(invisible())
  }
  doc_form = rdoc_form(doc_dir)
  if (doc_form == "pdf") {
    res = rdoc_pdf_process(doc_dir, steps, opts, cache=cache)
  } else if (doc_form == "html") {
    res = rdoc_html_process(doc_dir, steps, opts,cache=cache)
  } else if (doc_form == "mocr") {
    res = rdoc_mocr_process(doc_dir, steps, opts, cache=cache)
  } else {
    stop(paste0("Cannot process ", doc_dir,". Documents of form ", doc_form, " are not yet implemented."))
  }
  if (steps$sentences) {
    rdoc_sent_df(doc_dir, cache=cache)
  }
  if (steps$refs) {
    rdoc_tab_fig_refs(doc_dir, cache=cache)
  }

  invisible(res)
}

rdoc_is_processed = function(doc_dir) {
  doc_form = rdoc_form(doc_dir)
  if (doc_form %in% c("pdf","html")) {
    return(file.exists(file.path(doc_dir, "text_parts.Rds")))
  }
  restore.point("rdoc_is_processed")
  stop(paste0("Documents of form ", doc_form, " are not yet implemented."))

}

rdoc_opts = function() rdoc_options()


rdoc_options = function(stop_on_error = TRUE, ...) {
  as.list(sys.frame(sys.parent(0)))
}


rdoc_pdf_process = function(doc_dir, steps= rdoc_steps_from(TRUE), opts = rdoc_options(), cache=NULL) {
  restore.point("rdoc_pdf_process")
  project_dir = doc_dir_to_project_dir(doc_dir)
  repbox_set_problem_options(project_dir,fail_action = "msg")
  page_df = tab_df = NULL
  if (steps$pages) {
    page_df = rdoc_pdf_to_txt_pages(doc_dir)
    if (!is.null(cache)) cache$page_df = page_df
  }
  if (steps$tables) {
    if (opts$stop_on_error) {
      tab_df = rdoc_pdf_extract_tabs(doc_dir)
    } else {
      tab_df = try(rdoc_pdf_extract_tabs(doc_dir))
    }
    if (!is.null(cache)) cache$tab_df = tab_df
  }


  if (steps$parts) {
    if (opts$stop_on_error) {
      part_df = rdoc_pdf_pages_to_parts(doc_dir, opts=opts,page_df = page_df)
    } else {
      part_df = try(rdoc_pdf_pages_to_parts(doc_dir, opts=opts))
    }
    if (!is.null(cache)) cache$part_df = part_df

  }
}

rdoc_html_process = function(doc_dir,  steps= rdoc_steps_from(TRUE), opts = rdoc_options(), cache=NULL) {
  restore.point("rdoc_html_process")
  project_dir = doc_dir_to_project_dir(doc_dir)
  repbox_set_problem_options(project_dir,fail_action = "msg")
  if (opts$stop_on_error) {
    res = rdoc_html_to_parts(doc_dir, cache=cache)
  } else {
    res = try(rdoc_html_to_parts(doc_dir, cache=cache))
  }
}


rdoc_form = function(doc_dir) {
  base = basename(doc_dir)
  str.right.of(base,"_")
}

rdoc_type = function(doc_dir) {
  str.left.of(basename(doc_dir),"_")
}


rdoc_update_project = function(doc_dir, overwrite = FALSE, opts = repbox_rdoc_opts(overwrite=overwrite)) {
  restore.point("rdoc_update_project")
  opts$doc_dir = doc_dir
  repbox_set_current_doc_dir(doc_dir)
  overwrite = opts$overwrite
  rdoc_ensure_correct_dirs(doc_dir)
  art = rdoc_save_basic_info(doc_dir)

  parcels = list()

  existing_routes = NULL
  if (rdoc_has_pdf(doc_dir) & "pdf" %in% opts$create_routes) {
    cat("\n  1. Transform pdf and extract tables...")
    set_rdoc_route("pdf")
    existing_routes = c(existing_routes, "pdf")
    rdoc_pdf_to_txt_pages(doc_dir, overwrite = overwrite)
    parcels = rdoc_extract_pdf_tabs(doc_dir, overwrite = overwrite)
    rdoc_pdf_pages_to_parts(doc_dir, opts=opts)
  }
  if (rdoc_has_html(doc_dir) & "html" %in% opts$create_routes) {
    cat("\n  1. Transform html and extract tables...")
    set_rdoc_route("html")
    res = rdoc_html_to_parts(doc_dir)
    if (length(res)>0) {
      existing_routes = c("html", existing_routes)
    } else {
      cat("\nArticle HTML for ", doc_dir, " seems invalid.\n")
    }
  }
  if (!rdoc_has_pdf(doc_dir) & !rdoc_has_html(doc_dir)) {
    cat("\n No PDF or HTML file of article exists.")
    return(invisible(list()))
  }
  if (length(existing_routes)==0) {
    cat("\n No PDF or HTML file of article used.")
    return(invisible(list()))
  }

  route = intersect(opts$preferred_route,existing_routes)
  if (length(route)==0) {
    route = existing_routes[1]
  } else {
    route = route[1]
  }
  set_rdoc_route(route)

  cat("\n  2. Extract regression results from tables...")
  rdoc_extract_paren_type_from_tab_notes(doc_dir, route=route)

  parcels = rdoc_tabs_to_regs(doc_dir, opts=opts, parcels=parcels)
  cat("\n  3. Extract key phrases and map to references to figures, tables and columns...")
  rdoc_phrase_analysis(doc_dir)

  activate_rdoc_route(doc_dir,route)

  cat("\nDone with", doc_dir,"\n")
  invisible(parcels)
}

