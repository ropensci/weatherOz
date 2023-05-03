#' Capitalise the First Letters of Words in a String
#'
#' Capitalize the first letter of each element of the string vector.
#'
#' @param x `string` to be capitalized.
#' @param method one out of "first" (default), "word", "title". "first" will
#'  only capitalize the first character of a string. "word" will capitalize all
#'  found words and "title" will also capitalize wordwise, but leave out: a,
#'  an, the, at, by, for, in, of, on, to, up, and, as, but, s, or and nor.)
#'
#' @note Taken from \CRANpkg{Hmisc}
#'
#' @author Charles Dupont
#' @noRd

.strcap <- function(x, method = c("first", "word", "title")) {
  .cap <- function(x) {
    capped <- grep('^[^A-Z]*', x, perl = TRUE)

    substr(x[capped], 1, 1) <- toupper(substr(x[capped], 1, 1))
    return(x)
  }

  na <- is.na(x)

  switch(
    match.arg(method),
    first = {
      res <- .cap(x)
    },
    word = {
      res <-
        unlist(lapply(lapply(
          strsplit(x, split = "\\b\\W+\\b"), .cap
        ), paste, collapse = " "))
    },
    title = {
      z <- strsplit(tolower(x), split = "\\b\\W+\\b")
      low <-
        c(
          "a",
          "an",
          "the",
          "at",
          "by",
          "for",
          "in",
          "of",
          "on",
          "to",
          "up",
          "and",
          "as",
          "but",
          "or",
          "nor",
          "s"
        )
      z <- lapply(z, function(y) {
        y[y %nin% low] <- StrCap(y[y %nin% low])
        y[y %in% low] <- tolower(y[y %in% low])
        y
      })

      nn <- strsplit(x, split = "\\w+")

      res <- unlist(lapply(1:length(z), function(i) {
        if (length(nn[[i]]) != length(z[[i]])) {
          if (z[[i]][1] == "") {
            z[[i]] <- z[[i]][-1]
          } else {
            z[[i]] <- c(z[[i]], "")
          }
        } else {
          if (z[[i]][1] == "" & length(z[[i]]) > 1)
            z[[i]] <- VecRot(z[[i]], -1)
        }
        do.call(paste, list(nn[[i]], z[[i]], sep = "", collapse = ""))
      }))

    }
  )

  res[na] <- NA
  return(res)
}
