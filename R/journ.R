rdoc_guess_journ = function(doc_dir) {
  restore.point("rdoc_guess_journ")

  # Appendices don't follow a particular format
  doc_type = rdoc_type(doc_dir)
  if (doc_type != "art") return("unknown")


  artid = doc_dir_to_artid(doc_dir)
  journ = str.left.of(artid, "_")
  return(journ)

  guess_journ_from_artid(artid)
}



guess_journ_from_artid = function(artid) {
  restore.point("guess_jorun_from_artid")
  journ = str.left.of(artid,"_",not.found = "unknown")
  if (!journ %in% repbox_journ_list()) return("unknown")
  if (journ != "aer") return(journ)
  if (is_aer_pandp(artid)) return("aer_pandp")
  return(journ)
}

#' List of journals for which special detection heuristics (sections, footnotes etc.) are implemented.
repbox_journ_list = function() {
  c(
    "American Economic Review" = "aer",
    "aeri","jep",
    "American Economic Review P&P" = "aer_pandp",
    "AEJ Applied" = "aejapp",
    "AEJ Policy" = "aejpol",
    "AEJ Micro" = "aejmic",
    "AEJ Macro" = "aejmac",
    "Management Science" = "ms",
    "Quarterly Journal of Economics" = "qje"
  )
}

is_aer_pandp = function(artid) {
  journ = str.left.of(artid, "_")
  if (journ != "aer") return(FALSE)

  str = str.right.of(artid, "_")
  vol = str.left.of(str,"_") %>% as.integer()
  str = str.right.of(str, "_")
  issue = str.left.of(str,"_") %>% as.integer()
  # Only Papers and Proceeding Issues are a problem
  if (isTRUE((vol <= 103 & issue==3) | (vol>=104 & issue==5))) return(TRUE)
  return(FALSE)
}
