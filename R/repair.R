# repair old mistakes in doc directoris
#
# art.txt.md.to.doc = function() {
#   library(repboxDoc)
#   parent_dir = "~/repbox/projects_share"
#   project_dirs = repboxExplore::get_project_dirs(parent_dir)
#   doc_dirs = repbox_doc_dirs(project_dirs)
#
#   files = file.path(doc_dirs, "art.txt")
#   files = files[file.exists(files)]
#   to_files = file.path(dirname(files),"doc.txt")
#   file.rename(files, to_files)
#
#   files = file.path(doc_dirs, "art.md")
#   files = files[file.exists(files)]
#   to_files = file.path(dirname(files),"doc.md")
#   file.rename(files, to_files)
#
#   files = file.path(doc_dirs, "art.html")
#   files = files[file.exists(files)]
#   to_files = file.path(dirname(files),"doc.html")
#   file.rename(files, to_files)
#
#
# }

find_wrong_mocr = function() {
  library(stringtools)
  project_dirs = repboxExplore::get_project_dirs("~/repbox/projects_share")
  files = file.path(project_dirs, "doc", "art_mocr", "doc.md")
  f = files[1]
  for (f in files) {
    if (!file.exists(f)) next
    txt =readLines(f,warn = FALSE,n = 5)
    if (any((has.substr(txt,"US Consumer")))) cat("\n",f,"\n")

  }
}
