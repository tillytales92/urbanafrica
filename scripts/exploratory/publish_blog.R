# Copy the blog post in blog/ to the local clone of the website repo
# (tillmeissner.github.io), ready to commit and push. Not part of the pipeline.
#
#   blog/YYYY-MM-DD-<title>.md  ->  <SITE_DIR>/_posts/
#   blog/*.js                   ->  <SITE_DIR>/assets/js/
#   blog/*.json                 ->  <SITE_DIR>/assets/json/
#
# SITE_DIR is the local clone of the website repo. Set it once in ~/.Renviron,
# e.g.  SITE_DIR="C:/Users/ADMIN/Documents/GitHub/tillmeissner.github.io"
# (usethis::edit_r_environ() opens the file; restart R afterwards).
#
# After running: open the website repo, commit and push. The site rebuilds in a
# few minutes.
pacman::p_load(here)

site_dir <- Sys.getenv("SITE_DIR")
if (site_dir == "" || !dir.exists(file.path(site_dir, "_posts"))) {
  stop("SITE_DIR is not set or does not point to the website repo. ",
       "Add SITE_DIR=\"<path to tillmeissner.github.io>\" to ~/.Renviron and restart R.")
}

blog_dir <- here("blog")
posts <- list.files(blog_dir, pattern = "^\\d{4}-\\d{2}-\\d{2}-.+\\.md$", full.names = TRUE)
js    <- list.files(blog_dir, pattern = "\\.js$", full.names = TRUE)
json  <- list.files(blog_dir, pattern = "\\.json$", full.names = TRUE)
if (length(posts) == 0) stop("No post found in ", blog_dir, " (expected YYYY-MM-DD-title.md)")

# Jekyll skips posts dated in the future, so the post would silently not appear.
for (p in posts) {
  date_line <- grep("^date:", readLines(p, n = 20, warn = FALSE), value = TRUE)[1]
  post_time <- as.POSIXct(sub("^date:\\s*\"?([^\"]+)\"?\\s*$", "\\1", date_line), tz = "UTC")
  if (!is.na(post_time) && post_time > Sys.time()) {
    warning(basename(p), " is dated ", format(post_time), " UTC, which is in the future: ",
            "the website will not show it until then. Set an earlier date: line.")
  }
}

copy_to <- function(files, sub_dir) {
  dest <- file.path(site_dir, sub_dir)
  dir.create(dest, recursive = TRUE, showWarnings = FALSE)
  for (f in files) {
    target  <- file.path(dest, basename(f))
    changed <- !file.exists(target) || !identical(unname(tools::md5sum(f)), unname(tools::md5sum(target)))
    file.copy(f, target, overwrite = TRUE)
    cat(if (changed) "updated  " else "unchanged", file.path(sub_dir, basename(f)), "\n")
  }
}
copy_to(posts, "_posts")
copy_to(js, "assets/js")
copy_to(json, "assets/json")

cat("\nCopied to", site_dir, "\nNext: commit and push the website repo to publish.\n")
