# Console prompts used by the wizard. All input goes through .read_input(), so
# answers can be scripted with options(copernicopia.input = function(prompt) ...),
# which is how the tests drive the wizard.

.read_input <- function(prompt) {
  f <- getOption("copernicopia.input")
  if (is.function(f)) {
    cat(prompt)
    answer <- f(prompt)
    cat(answer, "\n", sep = "")
    return(trimws(answer))
  }
  if (!interactive()) {
    stop("This step needs an answer from you; run it in an interactive R session.",
         call. = FALSE)
  }
  trimws(readline(prompt))
}

.ask_text <- function(prompt, default = NULL) {
  shown <- if (is.null(default)) prompt else sprintf("%s[%s] ", prompt, default)
  repeat {
    answer <- .read_input(shown)
    if (nzchar(answer)) return(answer)
    if (!is.null(default)) return(default)
  }
}

.ask_password <- function(prompt) {
  if (is.function(getOption("copernicopia.input"))) return(.read_input(prompt))
  askpass::askpass(prompt)
}

.ask_yes_no <- function(prompt, default = TRUE) {
  hint <- if (default) "[Y/n] " else "[y/N] "
  repeat {
    answer <- tolower(.read_input(paste0(prompt, " ", hint)))
    if (!nzchar(answer)) return(default)
    if (answer %in% c("y", "yes")) return(TRUE)
    if (answer %in% c("n", "no")) return(FALSE)
    cat("Please answer y or n.\n")
  }
}

# Parse "1, 3 5-7" into c(1, 3, 5, 6, 7); NULL if the text is not a number list.
.parse_numbers <- function(text) {
  text <- trimws(text)
  if (!grepl("^[0-9][0-9 ,;-]*$", text)) return(NULL)
  pieces <- strsplit(text, "[ ,;]+")[[1]]
  pieces <- pieces[nzchar(pieces)]
  out <- unlist(lapply(pieces, function(p) {
    if (grepl("^[0-9]+-[0-9]+$", p)) {
      r <- as.integer(strsplit(p, "-")[[1]])
      seq(r[1], r[2])
    } else if (grepl("^[0-9]+$", p)) {
      as.integer(p)
    } else {
      NA_integer_
    }
  }))
  if (anyNA(out)) return(NULL)
  unique(out)
}

.ask_numbers <- function(prompt, n, default = NULL, multiple = TRUE) {
  repeat {
    answer <- .read_input(prompt)
    if (!nzchar(answer) && !is.null(default)) return(default)
    picks <- .parse_numbers(answer)
    if (!is.null(picks) && length(picks) > 0 && all(picks >= 1 & picks <= n) &&
        (multiple || length(picks) == 1)) {
      return(picks)
    }
    cat(sprintf("Please enter %s between 1 and %d.\n",
                if (multiple) "one or more numbers" else "a number", n))
  }
}

.ask_number <- function(prompt, default = NULL, min = -Inf, max = Inf) {
  shown <- if (is.null(default)) prompt else sprintf("%s[%s] ", prompt, format(default))
  repeat {
    answer <- .read_input(shown)
    if (!nzchar(answer) && !is.null(default)) return(default)
    value <- suppressWarnings(as.numeric(answer))
    if (length(value) == 1 && !is.na(value) && value >= min && value <= max) {
      return(value)
    }
    cat(sprintf("Please enter a number between %s and %s.\n", min, max))
  }
}

.ask_date <- function(prompt, default = NULL) {
  shown <- if (is.null(default)) prompt else sprintf("%s[%s] ", prompt, format(default))
  repeat {
    answer <- .read_input(shown)
    if (!nzchar(answer) && !is.null(default)) return(as.Date(default))
    value <- tryCatch(as.Date(answer, tryFormats = c("%Y-%m-%d", "%Y/%m/%d")),
                      error = function(e) NA)
    if (!is.na(value)) return(value)
    cat("Please enter a date as YYYY-MM-DD.\n")
  }
}

.rule <- function(title) {
  cat("\n", title, "\n", strrep("-", nchar(title)), "\n", sep = "")
}
