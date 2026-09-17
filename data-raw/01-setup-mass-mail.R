# Load dependencies ----
library("rvest")
library("tidyverse")
library("lubridate")

# Where the published data lives ----
massmail_csv_path = "data/massmail_data.csv"
massmail_rda_path = "data/massmail_data.rda"
massmail_cache_dir = "data-raw/massmail-emails"

# E-mails the archive lists that no parser can read yet. Tracked and rewritten
# every run, so the gap shows up in the commit diff rather than only in a build
# log nobody reads.
massmail_unparsed_path = "data-raw/unparsed-emails.csv"

# Losing one new e-mail to an unfamiliar template is a nuisance. Losing every
# new e-mail means the template changed and the dataset has silently stopped
# growing, which is worth failing the build over.
massmail_unparsed_alarm = 3L

# How many unreadable cached copies are worth re-fetching in one run. A handful
# is a bad capture worth retrying; hundreds is the archive changing shape, and
# re-downloading the lot every night would only hammer it.
massmail_refetch_limit = 25L

# How long to leave a known-unreadable e-mail alone before trying it again. A
# stub the archive served once is worth one more fetch; a template the parser
# has never seen is not worth fetching nightly for ever. Long enough not to
# hammer, short enough that a transient outage is not a life sentence.
massmail_refetch_after = 7L

# The archive is served in Urbana-Champaign local time.
massmail_tz = "America/Chicago"

massmail_columns = c("datetime", "date", "time", "sent", "subject", "url", "content")

# A known-complete snapshot of the published data, used only when there is no
# local copy to merge against (a fresh clone, a wiped data/). Bump the SHA if a
# newer floor is ever needed.
massmail_fallback_url = paste0(
  "https://raw.githubusercontent.com/illinois-r/massmail/",
  "de92e339dd86f96c83389344e5f79cdf68864573/data/massmail_data.csv"
)

# Massmail contents retrieval and caching ----
#
# Fetch to a staging file and rename into place only once the transfer has
# finished. download.file() leaves a partial file at the destination when a
# transfer is cut short, and the file.exists() check below would then treat that
# fragment as this e-mail's cached copy for good.
massmail_download_email = function(massmail_email_url,
                                   save_dir = massmail_cache_dir) {
  dir.create(save_dir, showWarnings = FALSE)

  # More control
  for (url in massmail_email_url) {
    file_loc = file.path(save_dir, basename(url))

    if (file.exists(file_loc))
      next

    message("Downloading ... ", url)
    staged = tempfile(fileext = ".html")

    # One e-mail the archive will not serve should not cost us the other 1991.
    # Nothing is cached, so the next run simply tries again.
    fetched = tryCatch({ download.file(url, staged, quiet = TRUE); TRUE },
                       error = function(e) {
                         massmail_alert("Could not download ", url, ": ",
                                        conditionMessage(e))
                         FALSE
                       })

    # file.rename() only returns FALSE across a filesystem boundary, and
    # tempdir() need not share a mount with the repository, so fall back to a
    # copy rather than quietly failing to cache anything.
    if (fetched && file.exists(staged) &&
        !suppressWarnings(file.rename(staged, file_loc)))
      file.copy(staged, file_loc, overwrite = TRUE)

    unlink(staged)
  }
}

massmail_read_email = function(massmail_email_url,
                               save_dir = massmail_cache_dir) {

  file_loc = file.path(save_dir, basename(massmail_email_url))

  # There really are about 4 templates across a 10 year span.
  massmail_email_body = function(path) {
    # A download that never succeeded leaves no cached copy. Report that as no
    # body, so the retry below and the unparsed record deal with it instead of
    # the whole build stopping here.
    if (!file.exists(path)) return(NA_character_)

    email = read_html(path)

    message("Attempting approach one on ", path, " ...")
    approach_one = email %>%
      html_node("#wrapper > tbody > tr:nth-child(2) > td:nth-child(3)") %>%
      html_text(trim = TRUE)

    if(!is.na(approach_one) && !identical(approach_one, "")) return(approach_one)
    message("Attempting approach two on ", path, " ...")

    # Based off of 27144.html
    approach_two = email %>%
      html_node("#wrapper > tbody > tr:nth-child(2) > td:nth-child(2)") %>%
      html_text(trim = TRUE)

    if(!is.na(approach_two) && !identical(approach_two, "")) return(approach_two)

    message("Attempting approach three on ", path, " ...")

    # Based off of 26844.html
    approach_three = email %>%
      html_node("table:nth-child(2) > tr:nth-child(1) ") %>%
      html_text(trim = TRUE)

    if(!is.na(approach_three) && !identical(approach_three, "")) return(approach_three)

    message("Attempting approach four on ", path, " ...")

    # Based off of 26839.html
    approach_four = email %>%
      html_node("table:nth-child(3) tr:nth-child(1)") %>%
      html_text(trim = TRUE)

    approach_four

   }

  map_chr(file_loc, massmail_email_body)
}

# Attach e-mail bodies to their rows, keyed on url. The cache holds e-mails the
# archive has stopped listing, so it is both longer than and ordered differently
# to this table -- attaching bodies by position would scramble every row.
massmail_attach_content = function(massmail_meta_table,
                                   save_dir = massmail_cache_dir,
                                   history = massmail_unparsed_history()) {

  content = massmail_read_email(massmail_meta_table$`url`, save_dir = save_dir)

  # Only e-mails we have not already given a second chance to recently. Without
  # this, one e-mail in a template the parser cannot read is re-downloaded every
  # single night, for ever.
  unreadable = (is.na(content) | !nzchar(trimws(content))) &
    massmail_refetch_due(massmail_meta_table$`url`, history)

  # A cached copy yielding no body is more often a stub the archive served once
  # -- a maintenance page, a WAF interstitial, a truncated capture -- than a
  # template the parser has never seen. Throw it away and fetch once more before
  # concluding we cannot read the e-mail, otherwise the first bad capture is the
  # only one we ever keep.
  if (any(unreadable) && sum(unreadable) <= massmail_refetch_limit) {
    message("Re-fetching ", sum(unreadable),
            " e-mail(s) whose cached copy has no readable body ...")
    retry = massmail_meta_table$`url`[unreadable]
    unlink(file.path(save_dir, basename(retry)))
    massmail_download_email(retry, save_dir = save_dir)
    content[unreadable] = massmail_read_email(retry, save_dir = save_dir)
  } else if (any(unreadable)) {
    massmail_alert(sum(unreadable), " cached e-mail(s) have no readable body, ",
                   "which is more than the ", massmail_refetch_limit,
                   " this will re-fetch in one run. Leaving the cache alone.")
  }

  massmail_meta_table %>% mutate(`content` = content)
}


# Construct the massmail table ----
#
# Read each field from within its own <li>. Sweeping the whole document four
# times and zipping the results by position looks equivalent, but it only holds
# while every entry contributes exactly one of everything: one entry carrying a
# second link and another carrying none leaves the four counts equal and shifts
# every row between them onto the wrong e-mail, silently. html_element() always
# returns one value per entry -- the first match, or NA when there is none.
massmail_meta_data = function(massmail_page) {

  entries = massmail_page %>% html_elements("#archive-list li")

  entry_text = function(css) {
    entries %>% html_element(css) %>% html_text()
  }

  # The subject is the link's own text. Reading the whole .col4 span instead
  # would swallow anything else the archive puts beside it, so fall back to the
  # span only for an entry that carries no link at all.
  subject = entries %>% html_element(".col4 a") %>% html_text()
  subject[is.na(subject)] = entry_text(".col4")[is.na(subject)]

  tibble::tibble(
    `datetime` = entry_text(".col1"),
    `sent` = entry_text(".col2"),
    `subject` = subject,
    `url` = entries %>% html_element(".col4 a") %>% html_attr("href")
  ) %>%
    mutate(`sent` = readr::parse_number(`sent`),
           # mdy_hm() reads the archive's wall clock; force_tz() anchors it to
           # Champaign. roll_dst settles the hour that does not exist on the
           # spring-forward Sunday, which would otherwise come back NA and stop
           # the build every day for as long as the archive listed that e-mail.
           `datetime` = lubridate::force_tz(
             lubridate::mdy_hm(`datetime`, quiet = TRUE),
             massmail_tz, roll_dst = c("boundary", "post")),
           `date` = lubridate::date(`datetime`),
           `time` = format(`datetime`, "%H:%M"))
}


# Accumulate rather than overwrite ----
#
# The archive enforces a rolling cut-off and has truncated before: on
# 2022-03-10 it dropped everything older than five years, taking the published
# data from 1494 rows to 826 in a single green CI run. Rebuilding the table
# purely from today's listing is what let that happen. Everything below exists
# so that a future cut-off can only ever stop *adding* rows, never delete them.

# The stable identifier for an e-mail. Keying the merge on the whole url would
# mean a path change on the archive matched nothing and published every e-mail
# twice, once under each url.
massmail_email_id = function(url) {
  if (is.null(url)) return(character(0))
  tools::file_path_sans_ext(basename(url))
}

# Read what was published last time: the working copy if we have one, otherwise
# a known-complete snapshot so a fresh clone still starts from a full baseline.
# A baseline is not optional -- without one there is nothing to stop the build
# republishing a truncated listing -- so this stops rather than quietly
# carrying on with nothing to compare against.
massmail_previous_data = function(local_path = massmail_csv_path,
                                  fallback_url = massmail_fallback_url) {

  read_published = function(source) {
    published = readr::read_csv(
      source,
      col_types = readr::cols(
        `datetime` = readr::col_datetime(),
        `date` = readr::col_date(),
        `time` = readr::col_character(),
        `sent` = readr::col_double(),
        `subject` = readr::col_character(),
        `url` = readr::col_character(),
        `content` = readr::col_character()
      ),
      progress = FALSE
    )

    # An empty or half-written table is not a baseline. Saying so here stops a
    # header-only or truncated CSV from quietly lowering every floor in
    # massmail_validate to whatever happened to survive. A mid-write truncation
    # usually leaves plenty of complete records, so the row count alone will not
    # catch it -- readr reports the ragged tail as a parsing problem.
    if (nrow(published) == 0L) stop("it has no rows", call. = FALSE)

    parsing_problems = readr::problems(published)
    if (nrow(parsing_problems) > 0L)
      stop("it did not parse cleanly (", nrow(parsing_problems),
           " problem(s), first on row ", parsing_problems$row[1], ")",
           call. = FALSE)

    published
  }

  if (!is.null(local_path) && file.exists(local_path)) {
    message("Reading previously published data from ", local_path, " ...")
    published = tryCatch(read_published(local_path), error = function(e) {
      # An empty file is the ordinary fresh-clone case, so fall through to the
      # pinned snapshot. A file with rows in it that will not parse is not:
      # falling back would measure every floor against a snapshot that predates
      # whatever the corrupt file was holding, and quietly drop it. Stop and let
      # someone look.
      if (!grepl("no rows", conditionMessage(e), fixed = TRUE))
        stop("Refusing to build: ", local_path, " ", conditionMessage(e),
             ". Restore it from git rather than letting the build fall back to ",
             "an older snapshot.", call. = FALSE)
      message("Could not use ", local_path, ": ", conditionMessage(e))
      NULL
    })
    if (!is.null(published)) return(published)
  }

  if (is.null(fallback_url))
    stop("Refusing to build: there is no published data to merge against.",
         call. = FALSE)

  message("Falling back to ", fallback_url, " ...")
  tryCatch(
    read_published(fallback_url),
    error = function(e)
      stop("Refusing to build: the fallback snapshot at ", fallback_url,
           " could not be read (", conditionMessage(e),
           "), leaving no published data to merge against.", call. = FALSE)
  )
}

# Take the freshly scraped value unless the scrape came back empty for it.
massmail_prefer = function(fresh, published) {
  blank = !is.na(fresh) & !nzchar(trimws(format(fresh)))
  fresh[blank] = NA
  dplyr::coalesce(fresh, published)
}

massmail_arrange = function(massmail_data) {
  massmail_data %>%
    arrange(desc(`datetime`)) %>%
    select(all_of(massmail_columns))
}

# An e-mail the archive has only just listed, whose body none of the four
# templates above can read, has nothing to fall back on. Leave it unpublished
# rather than failing the build and holding up every other new e-mail until
# someone teaches the parser a fifth template -- but record it, because a
# warning inside a green build is not a signal anyone will see.
massmail_drop_unparsed = function(scraped, previous,
                                  record_path = massmail_unparsed_path,
                                  history = massmail_unparsed_history(record_path)) {

  unreadable = is.na(scraped$`content`) | !nzchar(trimws(scraped$`content`))
  published_before = massmail_email_id(scraped$`url`) %in%
    massmail_email_id(previous$`url`)

  drop = unreadable & !published_before
  massmail_record_unparsed(scraped[drop, ], record_path, history = history)

  if (!any(drop)) return(scraped)

  # Every new e-mail failing at once is a template change, not an oddity. Left
  # to the warning alone the job stays green while the dataset stops growing.
  new_emails = sum(!published_before)
  if (sum(drop) == new_emails && new_emails >= massmail_unparsed_alarm)
    stop("Refusing to build: the archive listed ", new_emails,
         " new e-mail(s) and none of them could be read, which usually means ",
         "the e-mail template changed. See ", record_path, ".", call. = FALSE)

  massmail_alert("Could not read the body of ", sum(drop),
                 " newly listed e-mail(s); leaving them unpublished until the ",
                 "parser handles them: ",
                 paste(scraped$`url`[drop], collapse = ", "))

  scraped[!drop, ]
}

# What the last run could not read, and when it last tried.
massmail_unparsed_history = function(record_path = massmail_unparsed_path) {
  if (is.null(record_path) || !file.exists(record_path)) return(NULL)
  tryCatch(
    readr::read_csv(record_path, show_col_types = FALSE, progress = FALSE),
    error = function(e) NULL
  )
}

# Is this e-mail due another go? Never seen it fail before, or we last tried
# long enough ago that the archive may well have sorted itself out.
massmail_refetch_due = function(url, history) {
  if (is.null(history) || !nrow(history) || is.null(history$`last_tried`))
    return(rep(TRUE, length(url)))
  
  last = history$`last_tried`[match(massmail_email_id(url),
                                    massmail_email_id(history$`url`))]
  is.na(last) | as.Date(last) <= Sys.Date() - massmail_refetch_after
}

# Rewritten every run, so a fixed parser shows up as the file emptying out.
# first_seen and attempts carry across runs; last_tried is what the backoff
# above reads on the next build.
massmail_record_unparsed = function(unparsed, record_path, history = NULL) {
  if (is.null(record_path)) return(invisible(NULL))
  
  seen_before = function(column, default) {
    if (is.null(history) || !nrow(history) || is.null(history[[column]]))
      return(default)
    carried = history[[column]][match(massmail_email_id(unparsed$`url`),
                                      massmail_email_id(history$`url`))]
    dplyr::coalesce(carried, default)
  }
  
  tried_today = massmail_refetch_due(unparsed$`url`, history)
  
  unparsed %>%
    select(any_of(c("datetime", "date", "subject", "url"))) %>%
    mutate(`first_seen` = as.Date(seen_before("first_seen", Sys.Date())),
           `last_tried` = as.Date(dplyr::if_else(
             tried_today, Sys.Date(), as.Date(seen_before("last_tried", Sys.Date())))),
           `attempts` = as.integer(seen_before("attempts", 0L)) + as.integer(tried_today)) %>%
    readr::write_csv(file = record_path)
}

# warning() alone leaves a green tick on a run nobody opens. On GitHub Actions
# the same text as a workflow command becomes an annotation on the run itself.
massmail_alert = function(...) {
  text = paste0(...)
  if (identical(Sys.getenv("GITHUB_ACTIONS"), "true"))
    cat("::warning title=Unreadable massmail::", gsub("\n", " ", text), "\n", sep = "")
  warning(text, call. = FALSE)
}

# Union the fresh scrape with the published data, keyed on the e-mail id. The
# scrape wins field by field, the published row fills any gap it leaves, and a
# row the archive has stopped listing is carried over.
massmail_merge = function(scraped, previous) {

  scraped = scraped %>% mutate(`.id` = massmail_email_id(`url`))

  if (is.null(previous) || nrow(previous) == 0L) return(massmail_arrange(scraped))

  # Rows published before this script parsed timestamps correctly stored the
  # Champaign wall clock labelled Z, and an unpadded time. date and time are
  # the local reading under both the old and the new convention, so rebuild
  # datetime from those rather than trusting the stored offset.
  previous = previous %>%
    mutate(
      `.id` = massmail_email_id(`url`),
      `datetime` = dplyr::coalesce(
        lubridate::ymd_hm(paste(`date`, `time`), tz = massmail_tz, quiet = TRUE),
        lubridate::force_tz(`datetime`, massmail_tz,
                            roll_dst = c("boundary", "post"))
      ),
      `time` = format(`datetime`, "%H:%M")
    )

  if (any(duplicated(previous$`.id`)) || any(duplicated(scraped$`.id`)))
    stop("Refusing to merge: the same e-mail appears twice under different ",
         "urls, so the join would fan out.", call. = FALSE)

  carried = previous %>%
    filter(!`.id` %in% scraped$`.id`)

  # An e-mail the archive has re-listed under a new identifier looks like two
  # e-mails: a carried row under the old id and a scraped row under the new one.
  # No two massmails have ever shared a send time and a subject, and a carried
  # row is by definition not in today's listing, so a match means the e-mail
  # moved. Keep the scraped row -- it has the identifier that still works.
  moved = paste(carried$`datetime`, carried$`subject`) %in%
    paste(scraped$`datetime`, scraped$`subject`)
  if (any(moved)) {
    message("Re-identifying ", sum(moved),
            " e-mail(s) the archive now lists under a different url.")
    carried = carried[!moved, ]
  }

  if (nrow(carried) > 0L) {
    message("Carrying over ", nrow(carried),
            " e-mail(s) the archive no longer lists.")
  }

  refreshed = scraped %>%
    left_join(previous, by = ".id", suffix = c("", ".published"),
              relationship = "one-to-one")

  for (column in massmail_columns) {
    refreshed[[column]] = massmail_prefer(refreshed[[column]],
                                          refreshed[[paste0(column, ".published")]])
  }

  bind_rows(refreshed, carried) %>%
    massmail_arrange()
}

# Refuse to publish a table that is worse than the one we already have. These
# run before either write, so a regression fails the build instead of being
# committed.
massmail_validate = function(massmail_data, previous, scraped) {

  if (nrow(scraped) == 0L)
    stop("Refusing to write: the archive scrape returned no rows.", call. = FALSE)

  duplicated_url = duplicated(massmail_data$`url`)
  if (any(duplicated_url))
    stop("Refusing to write: duplicate url in the merged table, e.g. ",
         massmail_data$`url`[which(duplicated_url)[1]], ".", call. = FALSE)

  # A migration that lists an e-mail under both its old and its new path gives
  # two distinct urls for one e-mail, which the check above cannot see.
  duplicated_id = duplicated(massmail_email_id(massmail_data$`url`))
  if (any(duplicated_id))
    stop("Refusing to write: the same e-mail appears twice under different ",
         "urls, e.g. ", massmail_data$`url`[which(duplicated_id)[1]], ".",
         call. = FALSE)

  # A backstop for one e-mail reaching the table twice under different urls,
  # which massmail_merge resolves for the case it can see. Send time and subject
  # alone would also match two genuinely different massmails that happen to go
  # out in the same minute under the same subject, so the body has to match as
  # well -- at which point they are the same e-mail whatever the urls say.
  duplicated_email = duplicated(paste(massmail_data$`datetime`,
                                      massmail_data$`subject`,
                                      massmail_data$`content`))
  if (any(duplicated_email))
    stop("Refusing to write: the same e-mail is present twice under different ",
         "identifiers, e.g. ", massmail_data$`url`[which(duplicated_email)[1]],
         " -- the archive has probably renamed part of its listing.",
         call. = FALSE)

  empty_content = is.na(massmail_data$`content`) |
    !nzchar(trimws(massmail_data$`content`))
  if (any(empty_content))
    stop("Refusing to write: ", sum(empty_content),
         " row(s) have empty content, e.g. ",
         massmail_data$`url`[which(empty_content)[1]], ".", call. = FALSE)

  unparseable = is.na(massmail_data$`datetime`) | is.na(massmail_data$`date`)
  if (any(unparseable))
    stop("Refusing to write: ", sum(unparseable),
         " row(s) have an unparseable date/time, e.g. ",
         massmail_data$`url`[which(unparseable)[1]], ".", call. = FALSE)

  if (is.null(previous) || nrow(previous) == 0L) return(invisible(TRUE))

  if (!any(massmail_email_id(scraped$`url`) %in% massmail_email_id(previous$`url`)))
    stop("Refusing to write: the scrape has no e-mail in common with the ",
         "published data, so the archive's identifiers have changed and ",
         "nothing would merge.", call. = FALSE)

  if (nrow(massmail_data) < nrow(previous))
    stop("Refusing to write: merged table has fewer rows (", nrow(massmail_data),
         ") than the published data (", nrow(previous), ").", call. = FALSE)

  if (min(massmail_data$`date`, na.rm = TRUE) > min(previous$`date`, na.rm = TRUE))
    stop("Refusing to write: earliest date moved forward (",
         min(previous$`date`, na.rm = TRUE), " -> ",
         min(massmail_data$`date`, na.rm = TRUE),
         "); the archive must never lose history.", call. = FALSE)

  invisible(TRUE)
}

# readr writes a POSIXct in UTC, which would leave the published timestamp
# reading 5-6 hours away from the date and time columns beside it. Write the
# offset instead: unambiguous, and it reads back as the same instant.
massmail_write_csv = function(massmail_data, path) {
  massmail_data %>%
    mutate(`datetime` = format(`datetime`, "%Y-%m-%dT%H:%M:%S%z")) %>%
    readr::write_csv(file = path)
}


# Handle non-table semantics
massmail_table = function(massmail_page,
                          previous = massmail_previous_data(),
                          save_dir = massmail_cache_dir,
                          csv_path = massmail_csv_path,
                          rda_path = massmail_rda_path,
                          record_path = massmail_unparsed_path) {

  # Form overview of data
  massmail_meta_table = massmail_meta_data(massmail_page)

  # A maintenance window serves a page with no listing. That is not a failure
  # worth alerting on -- leave the published data alone and stop here.
  if (nrow(massmail_meta_table) == 0L) {
    message("Archive listed no e-mails; leaving the published data untouched.")
    return(invisible(NULL))
  }

  # Download individual data cells
  massmail_meta_table %>%
    pull(url) %>%
    massmail_download_email(save_dir = save_dir)

  # Pull e-mail contents from the local cache
  scraped = massmail_meta_table %>%
    massmail_attach_content(save_dir = save_dir) %>%
    select(all_of(massmail_columns)) %>%
    massmail_drop_unparsed(previous, record_path = record_path)

  # Release table with emails attached
  massmail_data = massmail_merge(scraped, previous)
  massmail_validate(massmail_data, previous, scraped)

  # Export data ----
  #usethis::use_data(massmail_data, overwrite = TRUE)
  # compress = FALSE looks wrong for a file that is committed daily, and is not.
  # save() otherwise writes a gzip stream, and a gzip stream is a fresh opaque
  # blob every run that git cannot delta against yesterday's: measured over 15
  # consecutive builds that costs ~1.4 MB of history per build (~343 MB a year)
  # against ~100 KB uncompressed (~25 MB a year). The working file grows from
  # 1.4 MB to 4.9 MB; the repository stops growing by a third of a gigabyte a
  # year. (xz lands in between, at ~189 MB a year -- still an opaque blob.)
  save(massmail_data, file = rda_path, compress = FALSE)
  massmail_write_csv(massmail_data, csv_path)

  invisible(massmail_data)
}


# Build the massmail data table ----
# Skipped when the script is sourced by data-raw/test-01-setup-mass-mail.R.
if (!identical(getOption("massmail.build"), FALSE)) {

  # Retrieve the massmail page. read_html() stops on a non-200 response, so an
  # unreachable archive fails the build before anything is written.
  massmail_page = read_html(httr::GET("https://massmail.illinois.edu/massmailArchive"))

  # Build an updated version of the massmail archive
  massmail_data = massmail_table(massmail_page)

  # Preview data inside of RStudio
  # View(massmail_data)
}
