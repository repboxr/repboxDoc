example = function() {

  doc_dir = "~/repbox/projects_share/aeri_1_2_6/doc/art_pdf"
  doc_dir = "~/repbox/projects_share/aeri_1_2_6/doc/art_mocr"
  res = rdoc_tab_ref_text(doc_dir)
  rdoc_sent_df(doc_dir)
  rdoc_tab_fig_refs(doc_dir)
  rstudioapi::filesPaneNavigate(doc_dir)
}


rdoc_tab_ref_text = function(doc_dir,tabid=NULL, ref_li = rdoc_load_ref_li(doc_dir, cache), part_df=rdoc_load_part_df(doc_dir, cache=NULL), parts_pre=1, parts_post=2, sep_str = "\n[...]\n", cache=NULL) {
  restore.point("rdoc_tab_ref_text")
  tab_ref = ref_li$tab_ref
  if (is.null(tabid)) tabid = unique(tab_ref$tabid)
  if (length(tabid)>1) {
    res = bind_rows(lapply(tabid, function(tid) {
      rdoc_tab_ref_text(doc_dir,tid, ref_li, part_df, parts_pre, parts_post, sep_str, cache)
    }))
    return(res)
  }
  restore.point("rdoc_tab_ref_text_inner")

  partinds = org_partinds = tab_ref$partind[tab_ref$tabid == tabid]
  for (i in setdiff((-parts_pre):parts_post, 0)) {
    partinds = union(partinds, partinds+1)
  }
  partinds = sort(partinds)
  txt = part_df$text[partinds]
  add_sep = is.true(lead(partinds)==partinds+1)
  txt[add_sep] = paste0(txt[add_sep], sep_str)

  tibble(tabid=tabid, num_refs = length(org_partinds), num_parts = length(partinds), parts_pre = parts_pre, parts_post=parts_post, text =  paste0(txt, collapse="\n"))


}



rdoc_sent_df = function(doc_dir, part_df = rdoc_load_part_df(doc_dir, cache), save=TRUE, cache=NULL) {
  txt = merge.lines(part_df$text)

  sent_loc = locate_sentences_in_txt(txt)

  if (!has_col(part_df,"nchar")) {
    part_df$nchar = nchar(part_df$text)
  }
  parts_loc = text_parts_to_loc(part_df$nchar)
  sent_loc$partind = map_loc_to_parent_loc(sent_loc, parts_loc)
  sent_loc$parttype = part_df$type[sent_loc$partind]
  if (save) {
    saveRDS(sent_loc, file.path(doc_dir, "sent_df.Rds"))
  }
  if (!is.null(cache)) cache$sent_df = sent_loc

  invisible(sent_loc)
}


rdoc_tab_fig_refs = function(doc_dir, sent_df = rdoc_load_sent_df(doc_dir, cache), part_df = rdoc_load_part_df(doc_dir, cache), save=TRUE, cache=NULL) {
  restore.point("rdoc_tab_fig_refs")
  # References to table, figure or column
  txt = paste0(part_df$text, collapse = "\n")
  ref_li = rdoc_refs_analysis(txt, sent_loc=sent_df)
  if (save) {
    saveRDS(ref_li, file.path(doc_dir, "ref_li.Rds"))
  }
  if (!is.null(cache)) cache$ref_li = ref_li

  invisible(ref_li)
}


rdoc_refs_analysis = function(txt,refs_types = c("tab","fig","col"), sent_loc=locate_sentences_in_txt(txt), parts_loc=NULL) {
  restore.point("rdoc_refs_analysis")

  tab_fig_loc = locate_tab_fig_refs_in_txt(txt)
  col_loc = locate_col_refs_in_txt(txt)
  tab_fig_loc$sentence = map_loc_to_parent_loc(tab_fig_loc, sent_loc)
  col_loc$sentence = map_loc_to_parent_loc(col_loc, sent_loc)
  tab_fig_loc$col = NA_integer_





  loc = bind_rows(tab_fig_loc, col_loc) %>%
    select(start, end,reftype = type, id = typeid, col=col, sentence, str=str) %>%
    arrange(sentence, reftype!="tab", reftype != "figure", start)

  if (!is.null(parts_loc)) {
    loc$partind = map_loc_to_parent_loc(loc, parts_loc)
  } else if (has_col(sent_loc,"partind")) {
    loc$partind = sent_loc$partind[loc$sentence]
  } else {
    loc$partind = NA_integer_
  }

  # Let us find the closest preceding table reference
  loc$prev_tab_row = cumsum(loc$reftype=="tab")
  loc$prev_fig_row = cumsum(loc$reftype=="fig")
  loc = loc %>%
    group_by(prev_tab_row) %>%
    mutate(
      prev_tabid = ifelse(prev_tab_row == 0, NA_character_, first(id)),
      prev_tab_sentence = ifelse(prev_tab_row == 0, NA_integer_, first(sentence)),
      prev_tab_partind = ifelse(prev_tab_row == 0, NA_integer_, first(partind)),
      prev_tab_start = ifelse(prev_tab_row == 0, NA_integer_, first(start))
    ) %>%
    ungroup()

  tab_ref = filter(loc, reftype=="tab") %>%
    select(start, end,reftype, tabid = id, sentence, partind, str)

  fig_ref = filter(loc, reftype=="fig") %>%
    select(start, end,reftype, figid = id, sentence, partind, str)

  col_ref = filter(loc, reftype %in% c("col","cols")) %>%
    mutate(tab_sent_dist = abs(prev_tab_sentence - sentence), tab_char_dist = abs(start-prev_tab_start)) %>%
    select(start, end,reftype, col, tabid = prev_tabid,  sentence, partind,  tab_char_dist, tab_sent_dist, tab_sentence = prev_tab_sentence, tab_partind=prev_tab_partind, str)

  if (NROW(col_ref)==0) {
    col_ref$tabid = character(0)
  }

  list(tab_ref=tab_ref, fig_ref=fig_ref, col_ref=col_ref)
}


locate_tab_fig_refs_in_txt = function(txt) {
  restore.point("locate_tab_fig_refs_in_txt")
  tab_fig_loc = stri_locate_all_regex(txt,"(([Aa]ppendix )?(([tT]able)|([fF]igure)) (([ABCDEFGHIJKLM][0-9]*)|([1-9][0-9]*)))(([. ?\\:][0-9]*)|([-]?[a-zA-Z0-9][. ?\\:]))",omit_no_match = TRUE)[[1]]


  tab_fig_df = loc_to_df(txt, tab_fig_loc) %>%
    mutate(
      type = ifelse(has.substr(str, "able"),"tab","fig"),
      typeid = ifelse(type=="tab", str.right.of(str, "able "), str.right.of(str,"igure ")) %>% trimws() %>% gsub("\\.$","",.,fixed=FALSE)
    ) %>%
    mutate(
      typeid = ifelse(startsWith(tolower(str),"a"),paste0("App.", typeid),typeid)
    )

  # Otherwise we get errors as, class is not ste correctly
  tab_fig_df = ensure_empty_types(tab_fig_df, c("type","typeid","tabid"), "character")
  tab_fig_df
}



locate_sentences_in_txt = function(txt) {
  loc = stringi::stri_locate_all_boundaries(txt,type="sentence")[[1]]
  df = loc_to_df(txt,loc)

  df$trim_str = trimws(df$str)
  df$nchar = nchar(df$trim_str)


  # Adapt somewhat

  # 1. Remove empty sentences
  df = filter(df, trim_str != "")

  # 2. After some abbreviations where the next line does not
  #    end with a number there is often erroneously a
  #    cut. Like "Selten et al.\n(1995)" or [vol.\n5]
  df$merge_with_next = FALSE
  # et al. (1991), Vol. 1, p. 234
  abb_rx = " ((al)|(p)|([v][V]ol))\\.[ ]*$"
  ends_with_abb = which(grepl(abb_rx, df$trim_str))

  # 3. Let us now Generally merge very short sentences with less
  #    than 10 characters. They often refer to parts of titles,
  #    e.g. "Table 5.", "1.3"
  short_merge = which(df$nchar<10)

  merge_rows = setdiff(union(ends_with_abb, short_merge), NROW(df))
  df = sentences_merge_with_next(df, merge_rows)

  df$sentence = seq_len(NROW(df))
  df[,c("start","end","sentence","str")]
}

sentences_merge_with_next = function(df, merge_rows) {
  if (length(merge_rows)==0) return(df)

  # We use a loop becaue vectorized code
  # does not work nicely if there
  # are multiple merge rows directly after each other

  # It is important that merge_rows are sorted
  for (i in merge_rows) {
    df$str[i+1] = paste0(df$str[i],df$str[i+1])
    df$start[i+1] = df$start[i]
  }
  #df$str[merge_rows+1] = paste0(df$str[merge_rows],df$str[merge_rows+1])
  #df$start[merge_rows+1] = df$start[merge_rows]
  df = df[-merge_rows,]
  df
}

locate_col_refs_in_txt = function(txt) {
  col1_loc = stri_locate_all_regex(txt,"[Cc]olumn ((\\([0-9]+\\))|([0-9]+))",omit_no_match = TRUE)[[1]]

  col1_df = loc_to_df(txt,col1_loc) %>%
    mutate(
      type = "col",
      col =  str.right.of(str, "olumn ") %>%
        stri_replace_first_fixed("(","") %>%
        stri_replace_first_fixed(")","") %>%
        trimws()
    )


  col2_loc = stri_locate_all_regex(txt, "((first ),(second )|(third )|(fourth )|(fifth )|(sixth )|(seventh )|(eigth )|(ninth )|(tenth ))([Cc]olumn)[ .\\:?\t]",omit_no_match = TRUE)[[1]]

  pos_words = c("first","second","third","fourth","fifth","sixth","seventh","eight","ninth","tenth")

  col2_df = loc_to_df(txt,col2_loc) %>%
    mutate(
      type = "col",
      col = match(str.left.of(str, " "), pos_words) %>% as.character()
    )


  # columns 1–2
  # columns 1-2
  # columns 1 to 9
  # columns 1 and 2
  # columns 1, 3 and 6

  rx_num = "((\\([0-9]+\\))|([0-9]+))"
  rx_comb = "(([ ]?[.][ ]?)|( to )|([,]? and ))"
  # (rx_numrx_comb])+rx_num
  rx = paste0("[Cc]olumns (",rx_num,rx_comb,")+",rx_num)
  cols1_loc = stri_locate_all_regex(txt,rx,omit_no_match = TRUE)[[1]]
  cols1_df = loc_to_df(txt, cols1_loc) %>%
    mutate(
      type = "cols",
      col = extract_num_from_sequence_text(str.right.of(str," "))
    ) %>%
    unnest(col)


  #first-second column
  #{first to fifth column[s]}
  #{first and second column[s]}
  #{first, second[,] and third column[s]}
  rx_num = "((first),(second)|(third)|(fourth)|(fifth)|(sixth)|(seventh)|(eigth)|(ninth)|(tenth))"
  rx_comb = "(([ ]?[.][ ]?)|( to )|([,]? and ))"
  # (rx_numrx_comb])+rx_num
  rx = paste0("(",rx_num,rx_comb,")+",rx_num," [Cc]column")
  cols2_loc = stri_locate_all_regex(txt,rx,omit_no_match = TRUE)[[1]]
  cols2_df = loc_to_df(txt, cols2_loc) %>%
    mutate(
      type = "cols",
      col = extract_order_num_from_sequence_text(str.remove.ends(str,right = 8))
    ) %>%
    unnest(col)

  df = bind_rows(
    col1_df,
    col2_df,
    cols1_df,
    cols2_df
  ) %>%
    mutate(col = as.integer(col)) %>%
    arrange(start)
  df
}


extract_order_num_from_sequence_text = function(str, as.character=TRUE) {
  str = gsub("first","1",str, fixed=TRUE)
  str = gsub("second","2",str, fixed=TRUE)
  str = gsub("third","3",str, fixed=TRUE)
  str = gsub("fourth","4",str, fixed=TRUE)
  str = gsub("fifth","5",str, fixed=TRUE)
  str = gsub("sixth","6",str, fixed=TRUE)
  str = gsub("seventh","7",str, fixed=TRUE)
  str = gsub("eighth","8",str, fixed=TRUE)
  str = gsub("ninth","9",str, fixed=TRUE)
  str = gsub("tenth","10",str, fixed=TRUE)
  extract_num_from_sequence_text(str, as.character=as.character)
}


extract_num_from_sequence_text = function(str, as.character=TRUE) {
  nums = stri_extract_all_regex(str, "[0-9]+")
  has_and = has.substr(str, "and")
  len_nums = sapply(nums, length)

  range_rows = !has_and & len_nums == 2
  if (!as.character) {
    nums[range_rows] = lapply(nums[range_rows],function(nu) seq(nu[1], nu[2], by=1))
  } else {
    nums[range_rows] = lapply(nums[range_rows],function(nu) as.character(seq(nu[1], nu[2], by=1)))
  }
  nums
}


text_parts_tab_fig_references = function(text_df) {
  ma_li = stringi::stri_extract_all_regex(text_df$text,"((([tT]able)|([fF]igure)) (([ABCDEFGHIJKLM][0-9]* )|([1-9][0-9]* )))",omit_no_match = TRUE)
  ma_df = extract_all_to_index_df(ma_li)

  co_df = stringi::stri_extract_all_regex(text_df$text,"((first ),(second )|(third )|(fourth )|(fifth )|(sixth )|(seventh )|(eigth )|(ninth )|())([Cc]olumn)(( \\([0-9]+\\))|( [0-9]+)|( ))",omit_no_match = TRUE) %>%
    extract_all_to_index_df()

  ma_df = ma_df %>% mutate(
    type = ifelse(has.substr(str, "able"),"tab","fig"),
    typeid = ifelse(type=="tab", str.right.of(str, "able "), str.right.of(str,"igure ")) %>% trimws()
  )

  ref_df = bind_rows(ma_df, co_df)

  colnames(ma_df) = c("partind","str","type","typeid")
}

extract_all_to_index_df = function(res, ind.col=".ROW", unique=FALSE) {
  df =lapply(seq_len(NROW(res)), function(i) {
    if (NROW(res[[i]])==0) return(NULL)
    tibble(.ROW = i, str=res[[i]])
  }) %>% bind_rows()

  if (unique) {
    df = unique(df)
  }
  df
  # df = stri_list2matrix(tab_ma,byrow = TRUE)%>%as.data.frame()
  # df$.ROW = seq_len(NROW(res))
  # df = df[!is.na(df[,1]),]
  #
  # li = lapply(df)

}



loc_to_df = function(txt,loc, add.left=0, add.right=0) {
  if (NROW(loc)==0) {
    return(tibble(start=integer(0), end=integer(0), str=character(0)))
  }

  tibble(
    start = loc[,1],
    end = loc[,2],
    # stri_sub is faster
    str = stri_sub(txt, loc[,1]-add.left, loc[,2]+add.right)
    #str = substring(txt, loc[,1]-add.left, loc[,2]+add.right)
  )
}


map_loc_to_parent_loc = function(loc, ploc, just.start=TRUE) {
  restore.point("map_loc_to_parent_loc")
  if (just.start) {
    findInterval(loc$start, ploc$start)
  } else {
    start = findInterval(loc$start, ploc$start)
    end = findInterval(loc$end, ploc$start)
    return(tibble(start_ind=start, end_ind=end))
  }
}

