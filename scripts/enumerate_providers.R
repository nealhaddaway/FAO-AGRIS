#!/usr/bin/env Rscript
suppressPackageStartupMessages({
  library(xml2)
  library(curl)
})
dir.create("outputs", recursive = TRUE, showWarnings = FALSE)
catalogue_url <- "https://agris.fao.org/ods/AGRIS.ODS.xml"
tmp <- tempfile(fileext = ".xml")
on.exit(unlink(tmp), add = TRUE)
curl::curl_download(catalogue_url, tmp, quiet = TRUE)
catalogue_bytes <- file.info(tmp)$size
doc <- xml2::read_xml(tmp)
# Match provider filenames in all attributes and text nodes, regardless of namespaces.
txt <- as.character(doc)
filenames <- unique(regmatches(txt, gregexpr("AGRIS\\.ODS\\.[A-Za-z0-9_-]+\\.xml", txt, perl = TRUE))[[1L]])
filenames <- sort(filenames)
if (!length(filenames)) stop("No provider filenames found in catalogue; inspect XML structure.")
providers <- sub("^AGRIS\\.ODS\\.", "", sub("\\.xml$", "", filenames))
urls <- paste0("https://agris.fao.org/ods/", filenames)
cat("Catalogue bytes:", catalogue_bytes, "Provider files:", length(urls), "\n")
probe <- function(i) {
  handle <- curl::new_handle(nobody = TRUE, followlocation = TRUE,
    timeout = 30, connecttimeout = 10, useragent = "AGRIS-ODS-size-inventory/0.1")
  result <- tryCatch(curl::curl_fetch_memory(urls[[i]], handle = handle), error = identity)
  if (inherits(result, "error")) {
    return(data.frame(provider = providers[[i]], file = filenames[[i]], url = urls[[i]],
      status = NA_integer_, content_length_bytes = NA_real_, content_type = "",
      error = conditionMessage(result)))
  }
  hdr <- curl::parse_headers_list(result$headers)
  len <- suppressWarnings(as.numeric(hdr[["content-length"]]))
  if (!length(len) || !is.finite(len)) len <- NA_real_
  type <- hdr[["content-type"]]
  if (is.null(type)) type <- ""
  data.frame(provider = providers[[i]], file = filenames[[i]], url = urls[[i]],
    status = as.integer(result$status_code), content_length_bytes = len,
    content_type = as.character(type), error = "")
}
workers <- min(10L, max(1L, parallel::detectCores() - 1L))
cat("Checking HEAD metadata using", workers, "workers\n")
batch_size <- 100L
result_parts <- vector("list", length(urls))
for (start in seq.int(1L, length(urls), by = batch_size)) {
  ids <- start:min(length(urls), start + batch_size - 1L)
  result_parts[ids] <- parallel::mclapply(ids, probe, mc.cores = workers)
  cat("Completed", max(ids), "of", length(urls), "\n")
}
inventory <- do.call(rbind, result_parts)
write.csv(inventory, "outputs/agris_provider_inventory.csv", row.names = FALSE, na = "")
known <- inventory$status >= 200L & inventory$status < 300L &
  is.finite(inventory$content_length_bytes)
known[is.na(known)] <- FALSE
summary <- data.frame(
  catalogue_bytes = catalogue_bytes,
  provider_files = nrow(inventory),
  known_sizes = sum(known),
  unknown_sizes = sum(!known),
  total_known_bytes = sum(inventory$content_length_bytes[known]),
  total_known_gib = sum(inventory$content_length_bytes[known]) / 1024^3,
  largest_known_gib = if (any(known)) max(inventory$content_length_bytes[known]) / 1024^3 else NA_real_,
  stringsAsFactors = FALSE)
write.csv(summary, "outputs/agris_provider_summary.csv", row.names = FALSE)
print(summary)
cat("HTTP statuses:\n")
print(table(inventory$status, useNA = "ifany"))
cat("Largest 20:\n")
print(head(inventory[order(-inventory$content_length_bytes),
  c("provider", "content_length_bytes", "status")], 20), row.names = FALSE)
