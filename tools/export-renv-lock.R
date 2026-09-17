# Export an renv-format lock file from the uvr-managed library (decision D9).
#
#   source tools/uvr-env.sh && Rscript tools/export-renv-lock.R
#
# uvr stays the working environment manager (uvr.toml / uvr.lock are the source
# of truth). renv.lock is a convenience for reviewers and collaborators who use
# renv: `renv::restore()` against it rebuilds the same package versions. It is
# written from the DESCRIPTION files of the packages actually installed, so it
# records what ran, not what was requested.
lib <- .libPaths()[1]
ip <- as.data.frame(installed.packages(lib.loc = lib, fields = c(
  "Repository", "RemoteType", "RemoteHost", "RemoteRepo", "RemoteUsername",
  "RemoteRef", "RemoteSha", "RemoteSubdir")), stringsAsFactors = FALSE)
ip <- ip[is.na(ip$Priority) | ip$Priority != "base", ]
clean <- function(x) x[!vapply(x, function(v) is.null(v) || is.na(v), TRUE)]

# uvr does not write Remote* fields into DESCRIPTION, so git-pinned packages
# (mlr3extralearners and the mlr-org dependencies it drags in) are read from
# uvr.lock, which records the exact commit.
ul <- readLines("uvr.lock")
starts <- grep("^\\[\\[package\\]\\]", ul)
gh <- list()
for (i in seq_along(starts)) {
  blk <- ul[starts[i]:(if (i < length(starts)) starts[i + 1] - 1 else length(ul))]
  if (!any(grepl('^source = "github"', blk))) next
  nm  <- sub('^name = "(.*)"$', "\\1", grep("^name = ", blk, value = TRUE))
  url <- sub('^url = "(.*)"$', "\\1", grep("^url = ", blk, value = TRUE))
  m <- regmatches(url, regexec("repos/([^/]+)/([^/]+)/tarball/([0-9a-f]+)", url))[[1]]
  gh[[nm]] <- list(user = m[2], repo = m[3], sha = m[4])
}
pkgs <- lapply(seq_len(nrow(ip)), function(i) {
  r <- ip[i, ]
  if (!is.null(gh[[r$Package]])) {
    g <- gh[[r$Package]]
    list(Package = r$Package, Version = r$Version, Source = "GitHub", RemoteType = "github",
         RemoteHost = "api.github.com", RemoteUsername = g$user, RemoteRepo = g$repo,
         RemoteRef = g$sha, RemoteSha = g$sha)
  } else if (!is.na(r$RemoteType) && r$RemoteType %in% c("github", "git2r", "gitlab")) {
    clean(list(Package = r$Package, Version = r$Version, Source = "GitHub",
               RemoteType = r$RemoteType, RemoteHost = r$RemoteHost, RemoteRepo = r$RemoteRepo,
               RemoteUsername = r$RemoteUsername, RemoteRef = r$RemoteRef, RemoteSha = r$RemoteSha,
               RemoteSubdir = r$RemoteSubdir))
  } else {
    clean(list(Package = r$Package, Version = r$Version, Source = "Repository",
               Repository = if (is.na(r$Repository)) "CRAN" else r$Repository))
  }
})
names(pkgs) <- ip$Package
lock <- list(
  R = list(Version = paste(R.version$major, R.version$minor, sep = "."),
           Repositories = list(list(Name = "CRAN", URL = "https://cloud.r-project.org"))),
  Packages = pkgs[order(names(pkgs))]
)
jsonlite::write_json(lock, "renv.lock", auto_unbox = TRUE, pretty = TRUE)
cat(sprintf("renv.lock written: R %s, %d packages (%d from GitHub) from %s\n",
            lock$R$Version, length(pkgs), sum(vapply(pkgs, function(p) identical(p$Source, "GitHub"), TRUE)), lib))
