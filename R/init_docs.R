example = function() {
  project_dirs = list.dirs("~/repbox/projects_share", recursive=FALSE)
  project_dir = project_dirs[2]
  for (project_dir in project_dirs) {
    rdoc_init_from_art(project_dir)
  }
  rstudioapi::filesPaneNavigate(project_dir)

}

rdoc_init_from_art = function(project_dir) {
  pdf_file = repboxArt::art_get_pdf_files(project_dir)
  if (length(pdf_file)==1) {
    dest_dir = file.path(project_dir, "doc", "art_pdf","pdf")
    if (!dir.exists(dest_dir)) dir.create(dest_dir, recursive = TRUE)
    file.copy(pdf_file, dest_dir)
  }

  html_file = repboxArt::art_get_html_files(project_dir)
  if (length(html_file)==1) {
    dest_dir = file.path(project_dir, "doc", "art_html","html")
    if (!dir.exists(dest_dir)) dir.create(dest_dir, recursive = TRUE)
    file.copy(html_file, dest_dir)
  }

}
