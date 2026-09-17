# Do the tests actually catch anything? ----
#
# Run with:  Rscript tests/verify-tests.R
#
# A green suite only means something if it can go red. This breaks the build
# script one defect at a time -- each of these is a real bug that has been in
# this file or was found in review -- and checks that tests/test-massmail-build.R
# notices. A mutation that survives is a hole in the suite, not a pass.

repo_root = local({
  args = commandArgs(trailingOnly = FALSE)
  here = sub("^--file=", "", grep("^--file=", args, value = TRUE))
  normalizePath(file.path(dirname(if (length(here)) here[1] else "tests/verify-tests.R"), ".."),
                mustWork = TRUE)
})

script_path = file.path(repo_root, "data-raw", "01-setup-mass-mail.R")
suite_path = Sys.getenv("MASSMAIL_SUITE",
                        unset = file.path(repo_root, "tests", "test-massmail-build.R"))
script = readLines(script_path, warn = FALSE)

# Each mutation is find/replace against the build script. `find` must match
# exactly once -- if the code moves on and an anchor stops matching, that is
# reported as an error rather than quietly counting as a pass.
mutations = list(
  list(
    name = "content attached by position instead of by url",
    find = "  content = massmail_read_email(massmail_meta_table$`url`, save_dir = save_dir)",
    replace = "  content = massmail_read_email(sort(list.files(save_dir)), save_dir = save_dir)"
  ),
  list(
    name = "rows the archive dropped are not carried over",
    find = "  bind_rows(refreshed, carried) %>%\n    massmail_arrange()",
    replace = "  refreshed %>%\n    massmail_arrange()"
  ),
  list(
    name = "the published row no longer fills a gap in the scrape",
    find = "  fresh[blank] = NA\n  dplyr::coalesce(fresh, published)",
    replace = "  fresh"
  ),
  list(
    name = "merge keyed on the whole url instead of the e-mail id",
    find = "  tools::file_path_sans_ext(basename(url))\n}",
    replace = "  url\n}"
  ),
  list(
    name = "the row-count floor is gone",
    find = "  if (nrow(massmail_data) < nrow(previous))",
    replace = "  if (FALSE)"
  ),
  list(
    name = "the earliest-date floor is gone",
    find = '  if (min(massmail_data$`date`, na.rm = TRUE) > min(previous$`date`, na.rm = TRUE))',
    replace = "  if (FALSE)"
  ),
  list(
    name = "the empty-content guard is gone",
    find = "  if (any(empty_content))",
    replace = "  if (FALSE)"
  ),
  list(
    name = "the unparseable-timestamp guard is gone",
    find = "  if (any(unparseable))",
    replace = "  if (FALSE)"
  ),
  list(
    name = "the duplicate e-mail id guard is gone",
    find = "  if (any(duplicated_id))",
    replace = "  if (FALSE)"
  ),
  list(
    name = "the empty-scrape guard is gone",
    find = "  if (nrow(scraped) == 0L)",
    replace = "  if (FALSE)"
  ),
  list(
    name = "time is written unpadded again",
    find = '           `time` = format(`datetime`, "%H:%M"))',
    replace = '           `time` = paste(lubridate::hour(`datetime`), lubridate::minute(`datetime`), sep = ":"))'
  ),
  list(
    name = "datetime is stamped UTC again",
    find = "           `datetime` = lubridate::force_tz(\n             lubridate::mdy_hm(`datetime`, quiet = TRUE),\n             massmail_tz, roll_dst = c(\"boundary\", \"post\")),",
    replace = "           `datetime` = lubridate::mdy_hm(`datetime`, quiet = TRUE),"
  ),
  list(
    name = "fields are zipped across the page instead of read per entry",
    find = '  subject = entries %>% html_element(".col4 a") %>% html_text()\n  subject[is.na(subject)] = entry_text(".col4")[is.na(subject)]',
    replace = '  subject = massmail_page %>% html_elements(".col4") %>% html_text()'
  ),
  list(
    name = "an entry with no link borrows the next entry's url",
    find = '    `url` = entries %>% html_element(".col4 a") %>% html_attr("href")',
    replace = '    `url` = massmail_page %>% html_elements(".col4 a") %>% html_attr("href")'
  ),
  list(
    name = "an empty published CSV is accepted as a baseline",
    find = '    if (nrow(published) == 0L) stop("it has no rows", call. = FALSE)',
    replace = "    NULL"
  ),
  list(
    name = "a published CSV that did not parse cleanly is accepted",
    find = "    if (nrow(parsing_problems) > 0L)",
    replace = "    if (FALSE)"
  ),
  list(
    name = "a newly listed e-mail with no readable body is published anyway",
    find = "  drop = unreadable & !published_before",
    replace = "  drop = rep(FALSE, nrow(scraped))"
  ),
  list(
    name = "the record of unreadable e-mails is never written",
    find = "  massmail_record_unparsed(scraped[drop, ], record_path, history = history)",
    replace = "  invisible(NULL)"
  ),
  list(
    name = "the record is not cleared once the parser catches up",
    find = "  massmail_record_unparsed(scraped[drop, ], record_path, history = history)",
    replace = "  if (any(drop)) massmail_record_unparsed(scraped[drop, ], record_path, history = history)"
  ),
  list(
    name = "a wholesale template change no longer fails the build",
    find = "  if (sum(drop) == new_emails && new_emails >= massmail_unparsed_alarm)",
    replace = "  if (FALSE)"
  ),
  list(
    name = "a single unreadable new e-mail fails the whole build",
    find = "massmail_unparsed_alarm = 3L",
    replace = "massmail_unparsed_alarm = 1L"
  ),
  list(
    name = "a missing baseline crashes instead of being handled",
    find = "  if (is.null(url)) return(character(0))\n",
    replace = ""
  ),
  list(
    name = "a cut-short download is left at the cache path",
    find = "    fetched = tryCatch({ download.file(url, staged, quiet = TRUE); TRUE },",
    replace = "    staged = file_loc\n    fetched = tryCatch({ download.file(url, staged, quiet = TRUE); TRUE },"
  ),
  list(
    name = "one unreachable e-mail stops the whole build again",
    find = "    fetched = tryCatch({ download.file(url, staged, quiet = TRUE); TRUE },",
    replace = "    fetched = ({ download.file(url, staged, quiet = TRUE); TRUE }) || tryCatch(TRUE,"
  ),
  list(
    name = "a missing cached copy errors instead of reporting no body",
    find = "    if (!file.exists(path)) return(NA_character_)\n",
    replace = ""
  ),
  list(
    name = "a bad cached copy is never fetched again",
    find = "  if (any(unreadable) && sum(unreadable) <= massmail_refetch_limit) {",
    replace = "  if (FALSE) {"
  ),
  list(
    name = "re-fetching is unbounded",
    find = "massmail_refetch_limit = 25L",
    replace = "massmail_refetch_limit = 100000L"
  ),
  list(
    name = "the same e-mail may be published under two identifiers",
    find = "  if (any(duplicated_email))",
    replace = "  if (FALSE)"
  ),
  list(
    name = "a known-unreadable e-mail is re-fetched every night again",
    find = "  unreadable = (is.na(content) | !nzchar(trimws(content))) &\n    massmail_refetch_due(massmail_meta_table$`url`, history)",
    replace = "  unreadable = is.na(content) | !nzchar(trimws(content))"
  ),
  list(
    name = "the backoff never expires, so one outage blacklists an e-mail",
    find = "  is.na(last) | as.Date(last) <= Sys.Date() - massmail_refetch_after",
    replace = "  is.na(last)"
  ),
  list(
    name = "the record forgets when it last tried",
    find = "    mutate(`first_seen` = as.Date(seen_before(\"first_seen\", Sys.Date())),",
    replace = "    mutate(`first_seen` = as.Date(Sys.Date()),"
  ),
  list(
    name = "an e-mail re-listed under a new url is published twice",
    find = "    carried = carried[!moved, ]",
    replace = "    carried = carried"
  ),
  list(
    name = "two different e-mails sent in the same minute are refused",
    find = "                                      massmail_data$`subject`,\n                                      massmail_data$`content`))",
    replace = "                                      massmail_data$`subject`))"
  ),
  list(
    name = "line endings are left to whatever the runner's parser does",
    find = '  gsub("\\r", "\\n", gsub("\\r\\n", "\\n", bodies, fixed = TRUE), fixed = TRUE)',
    replace = "  bodies"
  ),
  list(
    name = "only CRLF is folded, a lone CR still leaks through",
    find = '  gsub("\\r", "\\n", gsub("\\r\\n", "\\n", bodies, fixed = TRUE), fixed = TRUE)',
    replace = '  gsub("\\r\\n", "\\n", bodies, fixed = TRUE)'
  ),
  list(
    name = "a send-time-and-subject coincidence deletes an e-mail again",
    find = "  moved = paste(carried$`datetime`, carried$`subject`, carried$`content`) %in%\n    paste(scraped$`datetime`, scraped$`subject`, scraped$`content`)",
    replace = "  moved = paste(carried$`datetime`, carried$`subject`) %in%\n    paste(scraped$`datetime`, scraped$`subject`)"
  ),
  list(
    name = "a carried row falls back to the stored datetime again",
    find = "      `datetime` = lubridate::ymd_hm(paste(`date`, `time`),\n                                     tz = massmail_tz, quiet = TRUE),",
    replace = "      `datetime` = dplyr::coalesce(lubridate::ymd_hm(paste(`date`, `time`), tz = massmail_tz, quiet = TRUE), lubridate::force_tz(`datetime`, massmail_tz, roll_dst = c(\"boundary\", \"post\"))),"
  ),
  # Deliberately no mutation for removing the massmail_repair_entities() call
  # itself. On a libxml2 that already applies HTML5 legacy decoding the repair is
  # a no-op, so dropping it changes nothing observable here and the mutation
  # would survive on some machines and be caught on others. The regex below is
  # what the suite can pin everywhere.
  list(
    name = "the repair also rewrites properly terminated entities",
    find = "  gsub(\"&(amp|gt|lt|quot|copy|reg|nbsp)(?![a-zA-Z0-9#;])\", \"&\\\\1;\", html,",
    replace = "  gsub(\"&(amp|gt|lt|quot|copy|reg|nbsp);?\", \"&\\\\1;\", html,"
  ),
  list(
    name = "the published CSV goes back to a bare UTC timestamp",
    find = '    mutate(`datetime` = format(`datetime`, "%Y-%m-%dT%H:%M:%S%z")) %>%',
    replace = "    mutate(`datetime` = `datetime`) %>%"
  )
)

work_dir = file.path(tempdir(), "massmail-mutants")
dir.create(work_dir, showWarnings = FALSE, recursive = TRUE)

run_suite = function(against) {
  out = suppressWarnings(system2(
    file.path(R.home("bin"), "Rscript"), shQuote(suite_path),
    env = paste0("MASSMAIL_SCRIPT=", shQuote(against)),
    stdout = TRUE, stderr = TRUE
  ))
  status = attr(out, "status")
  list(failed = !is.null(status) && status != 0L,
       summary = grep("passed$", out, value = TRUE))
}

cat("Baseline: the suite must pass against the real build script.\n")
baseline = run_suite(script_path)
if (baseline$failed) {
  cat("  the suite is already failing -- fix that before trusting this report\n")
  quit(save = "no", status = 1L)
}
cat("  ok   ", if (length(baseline$summary)) tail(baseline$summary, 1) else "passed", "\n\n", sep = "")

cat("Now breaking the build script ", length(mutations), " ways.\n",
    "Every one of them should make the suite go red.\n\n", sep = "")

survived = character(0)
unmatched = character(0)

for (i in seq_along(mutations)) {
  m = mutations[[i]]
  source_text = paste(script, collapse = "\n")

  hits = length(gregexpr(m$find, source_text, fixed = TRUE)[[1]])
  if (!grepl(m$find, source_text, fixed = TRUE)) {
    unmatched = c(unmatched, m$name)
    cat("  ????  ", m$name, "\n        (anchor no longer matches the script -- update this mutation)\n", sep = "")
    next
  }
  if (hits > 1L) {
    unmatched = c(unmatched, m$name)
    cat("  ????  ", m$name, "\n        (anchor matches ", hits, " times -- make it unambiguous)\n", sep = "")
    next
  }

  mutant = file.path(work_dir, sprintf("mutant-%02d.R", i))
  writeLines(sub(m$find, m$replace, source_text, fixed = TRUE), mutant)

  result = run_suite(mutant)
  if (result$failed) {
    cat("  caught  ", m$name, "\n", sep = "")
  } else {
    survived = c(survived, m$name)
    cat("  MISSED  ", m$name, "\n", sep = "")
  }
}

unlink(work_dir, recursive = TRUE)

cat("\n", length(mutations) - length(survived) - length(unmatched), "/", length(mutations),
    " defects caught\n", sep = "")

if (length(unmatched)) {
  cat("\n", length(unmatched), " mutation(s) could not be applied:\n", sep = "")
  cat(paste0("  - ", unmatched, collapse = "\n"), "\n")
}
if (length(survived)) {
  cat("\n", length(survived), " defect(s) the suite does not catch:\n", sep = "")
  cat(paste0("  - ", survived, collapse = "\n"), "\n")
}

if (length(survived) || length(unmatched)) quit(save = "no", status = 1L)
