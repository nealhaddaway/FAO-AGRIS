#!/usr/bin/env Rscript
suppressPackageStartupMessages(library(curl))
dir.create("outputs", recursive=TRUE, showWarnings=FALSE)
# Deliberately tiny, sequential sample to avoid throttling the AGRIS service.
providers <- c("BG3","BRI","R26","BRY","TRA")
rows <- list()
for (code in providers) {
  url <- sprintf("https://agris.fao.org/ods/AGRIS.ODS.%s.xml", code)
  h <- curl::new_handle(followlocation=TRUE,timeout=40,connecttimeout=15,
    useragent="FAO-AGRIS-research-size-probe/0.2",
    httpheader=c("Range: bytes=0-1023", "Accept: application/xml"))
  result <- tryCatch(curl::curl_fetch_memory(url,handle=h),error=identity)
  if(inherits(result,"error")) {
    rows[[length(rows)+1L]] <- data.frame(provider=code,status=NA_integer_,
      content_length=NA_character_,content_range=NA_character_,content_type=NA_character_,
      received_bytes=NA_real_,error=conditionMessage(result))
  } else {
    hdr <- curl::parse_headers_list(result$headers)
    get <- function(k) if(is.null(hdr[[k]])) "" else as.character(hdr[[k]])
    rows[[length(rows)+1L]] <- data.frame(provider=code,status=as.integer(result$status_code),
      content_length=get("content-length"),content_range=get("content-range"),
      content_type=get("content-type"), received_bytes=length(result$content),error="")
  }
  print(tail(rows,1)[[1]])
  # Intentionally slow, because 429 responses were observed in the first run.
  Sys.sleep(5)
}
tab <- do.call(rbind,rows)
write.csv(tab,"outputs/agris_range_probe.csv",row.names=FALSE)
print(tab,row.names=FALSE)
