# Tests for the massmail build ----
#
# Run with:  Rscript tests/test-massmail-build.R
# Or check that these tests can actually fail:  Rscript tests/verify-tests.R
#
# Exits non-zero on the first failing expectation's summary, so CI can gate the
# daily build on it.

# Locate the repository regardless of where this was invoked from, so the test
# can be run from the project root, from tests/, or from a mutated copy.
massmail_repo_root = local({
  args = commandArgs(trailingOnly = FALSE)
  from_rscript = sub("^--file=", "", grep("^--file=", args, value = TRUE))
  from_source = tryCatch(sys.frame(1)$ofile, error = function(e) NULL)
  here = if (length(from_rscript)) from_rscript[1]
         else if (!is.null(from_source)) from_source
         else "tests/test-massmail-build.R"
  normalizePath(file.path(dirname(here), ".."), mustWork = TRUE)
})

# verify-tests.R points this at a deliberately broken copy of the build script
# to confirm the suite notices.
massmail_script = Sys.getenv(
  "MASSMAIL_SCRIPT",
  unset = file.path(massmail_repo_root, "data-raw", "01-setup-mass-mail.R")
)

# Sourcing the build script with `massmail.build = FALSE` loads its functions
# without touching the network or writing to data/.
options(massmail.build = FALSE)
setwd(massmail_repo_root)
source(massmail_script)

# Nothing here may touch the repository's own files. Redirecting the default
# record path covers every call that does not name one explicitly, including any
# added later -- the functions look this up when their defaults are evaluated.
massmail_unparsed_path = file.path(tempdir(), "massmail-unparsed-test.csv")
unlink(massmail_unparsed_path)

# Tiny harness ----
tests_run = 0L
tests_failed = 0L

expect = function(label, expr) {
  tests_run <<- tests_run + 1L
  ok = tryCatch(isTRUE(expr), error = function(e) structure(FALSE, msg = conditionMessage(e)))
  if (isTRUE(ok)) {
    cat("  ok   ", label, "\n", sep = "")
  } else {
    tests_failed <<- tests_failed + 1L
    cat("  FAIL ", label,
        if (!is.null(attr(ok, "msg"))) paste0("  <error: ", attr(ok, "msg"), ">") else "",
        "\n", sep = "")
  }
}

expect_error = function(label, expr, pattern) {
  tests_run <<- tests_run + 1L
  msg = tryCatch({ force(expr); NA_character_ }, error = function(e) conditionMessage(e))
  if (!is.na(msg) && grepl(pattern, msg)) {
    cat("  ok   ", label, "\n", sep = "")
  } else {
    tests_failed <<- tests_failed + 1L
    cat("  FAIL ", label, "  <expected error matching '", pattern, "', got ",
        if (is.na(msg)) "no error" else paste0("'", msg, "'"), ">\n", sep = "")
  }
}

# Fixtures ----
row_for = function(id, date, subject = paste("Subject", id), content = paste("Body", id),
                   sent = 1000, time = NULL) {
  dt = as.POSIXct(paste(date, "09:05"), tz = "America/Chicago")
  tibble::tibble(
    datetime = dt,
    date     = as.Date(dt, tz = "America/Chicago"),
    time     = if (is.null(time)) format(dt, "%H:%M") else time,
    sent     = sent,
    subject  = subject,
    url      = paste0("https://massmail.illinois.edu/massmail/", id, ".html"),
    content  = content
  )
}

published = dplyr::bind_rows(
  row_for("1001", "2013-04-01"),
  row_for("1002", "2016-08-15"),
  row_for("1003", "2021-02-09")
)

cat("\nmassmail_merge()\n")

# The whole point of the fix: a rolling cut-off on the archive must not delete
# rows we already published.
truncated = dplyr::bind_rows(
  row_for("1003", "2021-02-09"),
  row_for("1004", "2026-09-09")
)
merged = massmail_merge(truncated, published)

expect("keeps rows the fresh scrape no longer lists",
       setequal(merged$url, c(published$url, truncated$url)))

expect("does not duplicate a url present in both sources",
       !any(duplicated(merged$url)) && nrow(merged) == 4L)

expect("returns rows newest first",
       !is.unsorted(rev(merged$datetime)))

# A re-send updates the recipient count; the fresh page is authoritative.
restated = row_for("1003", "2021-02-09", subject = "Corrected subject", sent = 9999)
merged2 = massmail_merge(restated, published)
got = merged2[merged2$url == restated$url, ]

expect("fresh scrape wins for a url present in both",
       got$sent == 9999 && got$subject == "Corrected subject")

# A parser miss must not blank out a body we already have.
blanked = row_for("1003", "2021-02-09", content = NA_character_)
kept = massmail_merge(blanked, published)
kept_row = kept[kept$url == blanked$url, ]

expect("previous content fills in when the fresh content is NA",
       identical(kept_row$content, "Body 1003"))

empty = row_for("1003", "2021-02-09", content = "")
kept_empty = massmail_merge(empty, published)

expect("previous content fills in when the fresh content is empty",
       identical(kept_empty$content[kept_empty$url == empty$url], "Body 1003"))

expect("works when there is no previous table at all",
       nrow(massmail_merge(published, NULL)) == 3L)

expect("preserves the published column order",
       identical(names(merged),
                 c("datetime", "date", "time", "sent", "subject", "url", "content")))

cat("\nmassmail_validate()\n")

expect("passes a clean additive build",
       { massmail_validate(merged, published, truncated); TRUE })

expect_error("rejects an empty scrape",
             massmail_validate(published, published, published[0, ]),
             "returned no rows")

expect_error("rejects a build with fewer rows than the published data",
             massmail_validate(published[1:2, ], published, truncated),
             "fewer rows")

expect_error("rejects a build whose earliest date moved forward",
             massmail_validate(published[2:3, ] |> dplyr::bind_rows(row_for("1005", "2026-01-01")),
                               published, truncated),
             "earliest")

expect_error("rejects a build containing an empty content field",
             massmail_validate(dplyr::mutate(merged,
                                             content = replace(content, 1, NA_character_)),
                               published, truncated),
             "content")

expect_error("rejects a build containing a duplicate url",
             massmail_validate(dplyr::bind_rows(merged, merged[1, ]), published, truncated),
             "duplicate")

cat("\ncontent is keyed on url, never on position\n")

# This is the defect in the working-tree WIP: list.files() is lexicographic
# while the metadata table is date-descending.
cache = file.path(tempdir(), "massmail-emails-test")
dir.create(cache, showWarnings = FALSE)

for (id in c("9001", "100", "50000")) {
  writeLines(
    paste0('<html><body><table id="wrapper"><tbody><tr><td>x</td></tr>',
           '<tr><td>skip</td><td>skip</td><td>BODY-', id, '</td></tr></tbody></table></body></html>'),
    file.path(cache, paste0(id, ".html"))
  )
}

# Deliberately not in lexicographic order: sorting these filenames gives
# 100, 50000, 9001 -- a different order to the one the page lists them in.
urls = paste0("https://massmail.illinois.edu/massmail/", c("9001", "100", "50000"), ".html")
bodies = massmail_read_email(urls, save_dir = cache)

expect("each url gets its own body regardless of file order",
       identical(bodies, c("BODY-9001", "BODY-100", "BODY-50000")))

# The cache also holds e-mails the archive has stopped listing, so it is longer
# than the table being built. Attaching by position would scramble every row.
writeLines(
  paste0('<html><body><table id="wrapper"><tbody><tr><td>x</td></tr>',
         '<tr><td>skip</td><td>skip</td><td>BODY-ORPHAN</td></tr></tbody></table></body></html>'),
  file.path(cache, "1.html")
)

meta_rows = dplyr::bind_rows(
  row_for("9001", "2026-01-02"),
  row_for("100", "2024-05-05"),
  row_for("50000", "2022-07-07")
) %>% dplyr::select(-content)

attached = massmail_attach_content(meta_rows, save_dir = cache)

expect("content is attached by url even when the cache is longer than the table",
       identical(attached$content, c("BODY-9001", "BODY-100", "BODY-50000")))

expect("attaching content does not change the number of rows",
       nrow(attached) == 3L)

cat("\ncolumn formatting\n")

page = xml2::read_html(paste0(
  '<html><body><ul id="archive-list">',
  '<li><span class="col4"><a href="https://massmail.illinois.edu/massmail/7.html">Subj</a></span>',
  '<span class="col1">09-08-2026 &nbsp; 8:01 am</span><span class="col2">64,121</span></li>',
  '<li><span class="col4"><a href="https://massmail.illinois.edu/massmail/8.html">Subj2</a></span>',
  '<span class="col1">12-31-2025 &nbsp; 11:02 pm</span><span class="col2">1,003</span></li>',
  '</ul></body></html>'))
meta = massmail_meta_data(page)

expect("time is zero-padded to HH:MM",
       identical(meta$time, c("08:01", "23:02")))

expect("sent is parsed as a number",
       identical(meta$sent, c(64121, 1003)))

expect("datetime is anchored to America/Chicago, not UTC",
       identical(attr(meta$datetime, "tzone"), "America/Chicago") &&
         identical(format(meta$datetime[1], "%Y-%m-%d %H:%M"), "2026-09-08 08:01"))

expect("date is the Champaign calendar date, not the UTC one",
       identical(as.character(meta$date), c("2026-09-08", "2025-12-31")))

cat("\nmassmail_previous_data()\n")

tmp_csv = tempfile(fileext = ".csv")
massmail_write_csv(published, tmp_csv)

expect("reads the local published CSV when it exists",
       nrow(massmail_previous_data(tmp_csv, fallback_url = "http://invalid.invalid/x.csv")) == 3L)

# A header-only CSV must not be mistaken for a baseline: treating it as one
# would silently disable every floor in massmail_validate().
header_only = tempfile(fileext = ".csv")
readr::write_csv(published[0, ], header_only)

expect_error("falls through to the fallback when the local CSV has no rows",
             massmail_previous_data(header_only, fallback_url = "http://invalid.invalid/x.csv"),
             "could not be read")

# A CSV truncated mid-write usually leaves plenty of complete records, so a
# row count alone will not spot it. Taking it as the baseline would lower every
# floor in massmail_validate() to whatever survived.
truncated_csv = tempfile(fileext = ".csv")
writeLines(c(readLines(tmp_csv), substr(readLines(tmp_csv)[2], 1, 30)), truncated_csv)

expect("the truncated fixture really is short a field, not just short a row",
       nrow(readr::problems(suppressWarnings(readr::read_csv(
         truncated_csv, col_types = readr::cols(.default = readr::col_character()),
         progress = FALSE)))) > 0L)

expect_error("refuses a local CSV that parsed with problems",
             massmail_previous_data(truncated_csv, fallback_url = "http://invalid.invalid/x.csv"),
             "did not parse cleanly")

expect_error("refuses to continue when there is no local file and no usable fallback",
             massmail_previous_data(tempfile(fileext = ".csv"), fallback_url = NULL),
             "no published data")

cat("\nthe published data survives a key change\n")

# If the archive ever serves the same e-mails under a different path, keying on
# the whole url would match nothing and publish every e-mail twice.
moved = published %>%
  dplyr::mutate(url = sub("/massmail/", "/archive/massmail/", url))
merged_moved = massmail_merge(moved, published)

expect("a path change on the archive does not duplicate the published data",
       nrow(merged_moved) == 3L)

expect("a path change is taken as the new url for the e-mail",
       all(grepl("/archive/massmail/", merged_moved$url)))

expect_error("refuses to write when the scrape shares no e-mail with the published data",
             massmail_validate(dplyr::bind_rows(published, row_for("7777", "2026-02-02")),
                               published, row_for("7777", "2026-02-02")),
             "no e-mail in common")

cat("\ncarried-over rows are normalised\n")

# Rows published before this script parsed timestamps correctly stored the
# Champaign wall clock labelled Z, and an unpadded time. date + time are
# unambiguous in both conventions, so carried rows are rebuilt from those.
legacy = tibble::tibble(
  datetime = as.POSIXct("2020-02-03T22:08:00", tz = "UTC"),
  date     = as.Date("2020-02-03"),
  time     = "22:8",
  sent     = 50000,
  subject  = "Legacy row",
  url      = "https://massmail.illinois.edu/massmail/5555.html",
  content  = "Legacy body"
)
carried = massmail_merge(row_for("1004", "2026-09-09"), legacy)
carried_row = carried[carried$url == legacy$url, ]

expect("a carried row keeps the wall clock it was published with",
       identical(format(carried_row$datetime, "%Y-%m-%d %H:%M"), "2020-02-03 22:08"))

expect("a carried row is anchored to Champaign time",
       identical(attr(carried_row$datetime, "tzone"), "America/Chicago"))

expect("a carried row's legacy time is zero-padded",
       identical(carried_row$time, "22:08"))

cat("\nan unreadable new e-mail does not block the build\n")

unparsed = row_for("6001", "2026-09-10", content = NA_character_)
dropped = suppressWarnings(massmail_drop_unparsed(
  dplyr::bind_rows(unparsed, row_for("6002", "2026-09-11")), published))

expect("a new e-mail whose body cannot be parsed is dropped, not published",
       nrow(dropped) == 1L && dropped$url == row_for("6002", "2026-09-11")$url)

expect("dropping an unreadable new e-mail warns rather than failing silently",
       {
         w = tryCatch(
           massmail_drop_unparsed(unparsed, published),
           warning = function(x) conditionMessage(x))
         is.character(w) && grepl("6001", w)
       })

expect("an already published e-mail is never dropped for an unreadable body",
       nrow(massmail_drop_unparsed(
         dplyr::mutate(published, content = NA_character_), published)) == 3L)

# A warning in a build log is not a signal anyone sees. Leave a tracked record
# so the gap shows up in the commit diff instead.
record = tempfile(fileext = ".csv")
suppressWarnings(massmail_drop_unparsed(
  dplyr::bind_rows(unparsed, row_for("6002", "2026-09-11")), published,
  record_path = record))

expect("the e-mails it could not read are written to a tracked record",
       file.exists(record) &&
         identical(readr::read_csv(record, show_col_types = FALSE)$url, unparsed$url))

massmail_drop_unparsed(row_for("6003", "2026-09-12"), published, record_path = record)

expect("the record is emptied once everything parses again",
       nrow(readr::read_csv(record, show_col_types = FALSE)) == 0L)

# Every new e-mail failing at once is a template change, not an oddity: the
# dataset would otherwise stall indefinitely behind a green build.
all_unreadable = dplyr::bind_rows(
  row_for("7001", "2026-09-10", content = NA_character_),
  row_for("7002", "2026-09-11", content = NA_character_),
  row_for("7003", "2026-09-12", content = "")
)

expect_error("stops when no newly listed e-mail can be read at all",
             massmail_drop_unparsed(all_unreadable, published, record_path = record),
             "none of them")

expect("carries on when only some of the new e-mails are unreadable",
       nrow(suppressWarnings(massmail_drop_unparsed(
         dplyr::bind_rows(all_unreadable, row_for("7004", "2026-09-13")),
         published, record_path = record))) == 1L)

expect("one unreadable new e-mail on its own is not treated as a template change",
       nrow(suppressWarnings(massmail_drop_unparsed(
         unparsed, published, record_path = record))) == 0L)

# massmail_merge() accepts a missing baseline, so this has to as well.
expect("copes with there being no previous table at all",
       nrow(suppressWarnings(massmail_drop_unparsed(
         dplyr::bind_rows(unparsed, row_for("6002", "2026-09-11")), NULL,
         record_path = record))) == 1L)

cat("\nunparseable timestamps are reported clearly\n")

expect_error("names the offending row when a timestamp cannot be parsed",
             massmail_validate(
               dplyr::bind_rows(merged, dplyr::mutate(row_for("8888", "2026-03-03"),
                                                      datetime = as.POSIXct(NA),
                                                      date = as.Date(NA))),
               published, truncated),
             "unparseable")

cat("\nthe published CSV round-trips\n")

round_trip = tempfile(fileext = ".csv")
massmail_write_csv(merged, round_trip)

expect("datetime is written with an explicit offset, not a bare Z",
       any(grepl("T\\d{2}:\\d{2}:\\d{2}[+-]\\d{4}", readLines(round_trip)[2])))

expect("the CSV timestamp shows the same wall clock as the time column",
       {
         second = strsplit(readLines(round_trip)[2], ",")[[1]]
         identical(substr(second[1], 12, 16), merged$time[1])
       })

expect("reading the CSV back gives the same instants",
       isTRUE(all.equal(as.numeric(massmail_previous_data(round_trip, NULL)$datetime),
                        as.numeric(merged$datetime))))

cat("\nevery field stays pinned to its own archive entry\n")

archive_page = function(...) {
  xml2::read_html(paste0('<html><body><ul id="archive-list">', ...,
                         '</ul></body></html>'))
}
entry = function(id, stamp, sent, subject, extra = "") {
  paste0('<li><span class="col4"><a href="https://massmail.illinois.edu/massmail/',
         id, '.html">', subject, '</a>', extra, '</span><span class="col1">',
         stamp, '</span><span class="col2">', sent, '</span></li>')
}

# A stray second link in one entry and a missing link in another leave the
# document-wide counts equal, so zipping four separate sweeps by position would
# shift every row between them onto the wrong e-mail.
skewed = massmail_meta_data(archive_page(
  entry("11", "09-01-2026 &nbsp; 9:01 am", "100", "First",
        extra = ' <a href="https://example.org/flyer.pdf">(PDF)</a>'),
  entry("22", "09-02-2026 &nbsp; 9:02 am", "200", "Second"),
  '<li><span class="col4">Withdrawn</span><span class="col1">09-03-2026 &nbsp; 9:03 am</span><span class="col2">300</span></li>',
  entry("44", "09-04-2026 &nbsp; 9:04 am", "400", "Fourth")
))

expect("one row per archive entry, whatever the links do",
       nrow(skewed) == 4L)

expect("a stray extra link does not shift the following rows",
       identical(skewed$sent, c(100, 200, 300, 400)) &&
         identical(skewed$subject, c("First", "Second", "Withdrawn", "Fourth")))

expect("an entry with no link gets no url rather than borrowing the next one",
       is.na(skewed$url[3]) &&
         identical(basename(skewed$url[c(1, 2, 4)]),
                   c("11.html", "22.html", "44.html")))

# A template rollout that renames the recipient-count class on all but one
# entry would otherwise recycle that single value across every row.
lone = massmail_meta_data(archive_page(
  entry("11", "09-01-2026 &nbsp; 9:01 am", "100", "First"),
  '<li><span class="col4"><a href="https://massmail.illinois.edu/massmail/22.html">Second</a></span><span class="col1">09-02-2026 &nbsp; 9:02 am</span><span class="recipients">200</span></li>'
))

expect("a missing recipient count is NA, not the previous row's value",
       identical(lone$sent, c(100, NA_real_)))

expect("an e-mail sent in the spring-forward gap still gets a timestamp",
       {
         gap = massmail_meta_data(archive_page(
           entry("33", "03-08-2026 &nbsp; 2:30 am", "1", "Gap")))
         !is.na(gap$datetime) && identical(gap$time, "03:00")
       })

cat("\nduplicate e-mail ids are caught\n")

# A migration that lists each e-mail under both its old and its new path gives
# distinct urls but the same e-mail id.
both_paths = dplyr::bind_rows(
  published,
  dplyr::mutate(published, url = sub("/massmail/", "/v2/massmail/", url))
)

expect_error("refuses to write the same e-mail under two urls",
             massmail_validate(both_paths, published, both_paths),
             "appears twice under different")

expect_error("refuses a previously published table that already has duplicate ids",
             massmail_merge(published, both_paths),
             "twice|duplicate")

cat("\na corrupt local baseline stops the build\n")

expect_error("stops rather than quietly falling back to the pinned snapshot",
             massmail_previous_data(truncated_csv,
                                    fallback_url = massmail_fallback_url),
             "did not parse cleanly")

cat("\nevery e-mail template the archive has used still parses\n")

for (fixture in c("27144.html", "26844.html", "26839.html")) {
  path = file.path(massmail_cache_dir, fixture)
  if (!file.exists(path)) {
    cat("  skip  ", fixture, " (not in the local cache)\n", sep = "")
    next
  }
  expect(paste("the", fixture, "template yields a body"),
         {
           body = massmail_read_email(fixture)
           !is.na(body) && nchar(body) > 50L
         })
}

cat("\nmassmail_table() end to end\n")

e2e_cache = file.path(tempdir(), "massmail-e2e")
dir.create(e2e_cache, showWarnings = FALSE)
for (id in c("11", "22", "44")) {
  writeLines(
    paste0('<html><body><table id="wrapper"><tbody><tr><td>x</td></tr>',
           '<tr><td>s</td><td>s</td><td>Body of ', id, '</td></tr></tbody></table></body></html>'),
    file.path(e2e_cache, paste0(id, ".html"))
  )
}
e2e_csv = tempfile(fileext = ".csv")
e2e_rda = tempfile(fileext = ".rda")

# Two e-mails are still listed, one has dropped off the archive entirely.
e2e_previous = dplyr::bind_rows(
  row_for("11", "2026-09-01", subject = "Stale subject", content = "Body of 11"),
  row_for("99", "2014-05-05", subject = "Long gone", content = "Body of 99")
)

e2e = massmail_table(
  archive_page(entry("11", "09-01-2026 &nbsp; 9:01 am", "100", "First"),
               entry("22", "09-02-2026 &nbsp; 9:02 am", "200", "Second"),
               entry("44", "09-04-2026 &nbsp; 9:04 am", "400", "Fourth")),
  previous = e2e_previous, save_dir = e2e_cache,
  csv_path = e2e_csv, rda_path = e2e_rda)

expect("publishes the listed e-mails plus the one the archive dropped",
       nrow(e2e) == 4L &&
         setequal(basename(e2e$url), c("11.html", "22.html", "44.html", "99.html")))

expect("the e-mail the archive dropped keeps its body",
       identical(e2e$content[e2e$url == e2e_previous$url[2]], "Body of 99"))

expect("a re-listed e-mail takes the archive's current subject",
       identical(e2e$subject[basename(e2e$url) == "11.html"], "First"))

expect("every body belongs to its own e-mail",
       all(e2e$content == paste("Body of", tools::file_path_sans_ext(basename(e2e$url)))))

expect("the written CSV matches what was returned",
       nrow(massmail_previous_data(e2e_csv, NULL)) == 4L)

unlink(e2e_cache, recursive = TRUE)

cat("\na bad capture never becomes the cached copy\n")

# A small file:// "archive" so these exercise the real download path.
origin = file.path(tempdir(), "massmail-origin")
dl_cache = file.path(tempdir(), "massmail-dl-cache")
for (d in c(origin, dl_cache)) { unlink(d, recursive = TRUE); dir.create(d) }

email_html = function(txt) {
  paste0('<html><body><table id="wrapper"><tbody><tr><td>x</td></tr>',
         '<tr><td>s</td><td>s</td><td>', txt, '</td></tr></tbody></table></body></html>')
}
origin_url = function(id) paste0("file://", file.path(origin, paste0(id, ".html")))

writeLines(email_html("A perfectly good body."), file.path(origin, "5101.html"))

# An e-mail the archive cannot serve at all must not leave anything behind that
# a later run would mistake for a cached copy.
suppressWarnings(massmail_download_email(c(origin_url("5101"), origin_url("5199")),
                                         save_dir = dl_cache))

expect("a successful download is cached",
       file.exists(file.path(dl_cache, "5101.html")))

expect("a failed download leaves nothing at the cache path",
       !file.exists(file.path(dl_cache, "5199.html")))

expect("one unreachable e-mail does not stop the others being fetched",
       identical(sort(list.files(dl_cache)), "5101.html"))

expect("a missing cached copy reads as no body rather than erroring",
       is.na(massmail_read_email(origin_url("5199"), save_dir = dl_cache)))

# The real failure this guards: download.file leaves a truncated file at the
# destination and `if (file.exists()) next` makes it permanent.
writeLines("<html><body><p>Scheduled maintenance.</p></body>", file.path(dl_cache, "5102.html"))
writeLines(email_html("The body that was there all along."), file.path(origin, "5102.html"))

poisoned = tibble::tibble(url = c(origin_url("5101"), origin_url("5102")))
healed = massmail_attach_content(poisoned, save_dir = dl_cache)

expect("a cached copy with no readable body is fetched again rather than written off",
       identical(healed$content[2], "The body that was there all along."))

expect("re-fetching replaces the bad cached copy on disk",
       identical(massmail_read_email(origin_url("5102"), save_dir = dl_cache),
                 "The body that was there all along."))

# If nothing readable comes back the second time either, it is a template the
# parser does not know -- report no body and let massmail_drop_unparsed decide.
writeLines("<html><body><p>a fifth template</p></body></html>", file.path(origin, "5103.html"))
still_bad = suppressWarnings(massmail_attach_content(
  tibble::tibble(url = origin_url("5103")), save_dir = dl_cache))

expect("an e-mail the parser genuinely cannot read still reports no body",
       is.na(still_bad$content) || !nzchar(still_bad$content))

# The archive serves all of these perfectly well, so an uncapped retry would
# happily re-fetch every one of them -- which is the behaviour being ruled out.
many_ids = sprintf("52%02d", seq_len(40L))

expect("the cap is set low enough for this to exercise it",
       length(many_ids) > massmail_refetch_limit)

for (id in many_ids) {
  writeLines(email_html(paste("Body of", id)), file.path(origin, paste0(id, ".html")))
  writeLines("<html><body><p>stub</p></body></html>", file.path(dl_cache, paste0(id, ".html")))
}
capped = suppressWarnings(massmail_attach_content(
  tibble::tibble(url = origin_url(many_ids)), save_dir = dl_cache))

expect("re-fetching is capped so a site-wide change cannot hammer the archive",
       all(is.na(capped$content) | !nzchar(trimws(capped$content))))

expect("under the cap, the same e-mails are re-fetched and read",
       {
         few = many_ids[1:3]
         got = massmail_attach_content(tibble::tibble(url = origin_url(few)),
                                       save_dir = dl_cache)
         identical(got$content, paste("Body of", few))
       })

cat("\nthe same e-mail cannot be published under two identities\n")

# Half the archive moving to a new identifier form leaves enough ids matching
# that the no-overlap guard stays quiet, and publishes the moved half twice.
renamed = dplyr::mutate(published[1:2, ],
                        url = sub("/massmail/(\\d+)", "/massmail/m-\\1", url))
both_identities = dplyr::bind_rows(published, renamed)

expect("the fingerprint this relies on is unique in the published data",
       !any(duplicated(paste(massmail_previous_data()$datetime,
                             massmail_previous_data()$subject))))

expect_error("refuses when one e-mail appears under two identifiers",
             massmail_validate(both_identities, published,
                               dplyr::bind_rows(published[3, ], renamed)),
             "same e-mail")

expect("a normal build is not tripped up by the fingerprint check",
       { massmail_validate(merged, published, truncated); TRUE })

cat("\na known-unreadable e-mail is not re-fetched every night\n")

backoff_cache = file.path(tempdir(), "massmail-backoff")
unlink(backoff_cache, recursive = TRUE); dir.create(backoff_cache)
writeLines(email_html("Readable after all."), file.path(origin, "5301.html"))
writeLines("<html><body><p>stub</p></body></html>", file.path(backoff_cache, "5301.html"))
stub_mtime = function() file.mtime(file.path(backoff_cache, "5301.html"))

# No history: this is the first time we have seen it fail, so try again.
first_try = stub_mtime()
massmail_attach_content(tibble::tibble(url = origin_url("5301")),
                        save_dir = backoff_cache, history = NULL)

expect("an unreadable e-mail is re-fetched the first time it is seen",
       stub_mtime() != first_try)

# Put the stub back and say we already retried today.
writeLines("<html><body><p>stub</p></body></html>", file.path(backoff_cache, "5301.html"))
tried_today = tibble::tibble(url = origin_url("5301"), last_tried = Sys.Date())
before = stub_mtime()
again = massmail_attach_content(tibble::tibble(url = origin_url("5301")),
                                save_dir = backoff_cache, history = tried_today)

expect("an e-mail already retried today is left alone",
       identical(stub_mtime(), before))

expect("leaving it alone still reports that it has no body",
       is.na(again$content) || !nzchar(again$content))

# ... but not for ever: a transient outage must not blacklist an e-mail.
tried_long_ago = tibble::tibble(url = origin_url("5301"),
                                last_tried = Sys.Date() - massmail_refetch_after - 1L)
stale = massmail_attach_content(tibble::tibble(url = origin_url("5301")),
                                save_dir = backoff_cache, history = tried_long_ago)

expect("an e-mail last retried long ago is tried again",
       identical(stale$content, "Readable after all."))

expect("the record carries the bookkeeping the backoff needs",
       {
         r = tempfile(fileext = ".csv")
         suppressWarnings(massmail_drop_unparsed(
           row_for("5401", "2026-09-14", content = NA_character_), published,
           record_path = r))
         kept = readr::read_csv(r, show_col_types = FALSE)
         all(c("url", "first_seen", "last_tried", "attempts") %in% names(kept)) &&
           kept$attempts == 1L && kept$last_tried == Sys.Date()
       })

expect("a second failing run counts another attempt without moving first_seen",
       {
         r = tempfile(fileext = ".csv")
         row = row_for("5401", "2026-09-14", content = NA_character_)
         hist = tibble::tibble(url = row$url, first_seen = Sys.Date() - 30L,
                               last_tried = Sys.Date() - 30L, attempts = 4L)
         suppressWarnings(massmail_drop_unparsed(row, published, record_path = r,
                                                 history = hist))
         kept = readr::read_csv(r, show_col_types = FALSE)
         kept$attempts == 5L && kept$first_seen == Sys.Date() - 30L
       })

unlink(c(origin, dl_cache, backoff_cache), recursive = TRUE)

cat("\ntwo identities for one e-mail resolve themselves\n")

# The archive moves some e-mails to a new identifier form. The moved ones show
# up as a scraped row under the new id, while the old id is still carried.
moved_away = massmail_merge(
  dplyr::mutate(published[1:2, ],
                url = sub("/massmail/(\\d+)", "/massmail/m-\\1", url)),
  published)

expect("an e-mail that reappears under a new identifier is not published twice",
       nrow(moved_away) == 3L)

expect("the new identifier is the one that is kept",
       all(grepl("/massmail/m-", moved_away$url[moved_away$subject != "Subject 1003"])))

expect("an e-mail that simply stopped being listed is still carried",
       "Subject 1003" %in% moved_away$subject)

# The remaining fingerprint check has to be narrow enough that two genuinely
# different massmails sent in the same minute do not trip it.
same_minute = dplyr::bind_rows(
  published,
  dplyr::mutate(published[1, ], url = sub("1001", "1009", url),
                content = "A different message that happens to share a subject."))

expect("two different e-mails sharing a send time and subject are allowed",
       { massmail_validate(same_minute, published, same_minute); TRUE })

expect_error("the same e-mail published twice is still refused",
             massmail_validate(
               dplyr::bind_rows(published,
                                dplyr::mutate(published[1, ],
                                              url = sub("1001", "1009", url))),
               published, published),
             "same e-mail")

# Summary ----
unlink(cache, recursive = TRUE)
cat("\n", tests_run - tests_failed, "/", tests_run, " passed\n", sep = "")
if (tests_failed > 0L) quit(save = "no", status = 1L)
