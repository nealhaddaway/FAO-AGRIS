#!/usr/bin/env Rscript
suppressPackageStartupMessages(library(httr2))
urls <- c(
 "https://data.apps.fao.org/catalog/dcat/agris",
 "https://agris.fao.org/agris_ods/",
 "https://www.fao.org/agris/agris-ods"
)
probe <- function(u, method = "HEAD") {
  out <- tryCatch({
    req <- request(u) |> req_user_agent("AGRIS-ODS-feasibility-check/0.1 (metadata-only)") |>
      req_timeout(35) |> req_error(is_error=function(resp) FALSE)
    if(method=="HEAD") req <- req_method(req,"HEAD")
    else req <- req_headers(req,Range="bytes=0-65535")
    resp <- req_perform(req)
    headers <- resp_headers(resp)
    list(method=method,url=u,status=resp_status(resp),
         final_url=resp_url(resp),
         content_type=headers[["content-type"]] %||% NA_character_,
         content_length=headers[["content-length"]] %||% NA_character_,
         content_range=headers[["content-range"]] %||% NA_character_,
         data=if(method=="GET")resp_body_string(resp) else "")
  },error=function(e) list(method=method,url=u,error=conditionMessage(e)))
  out
}
`%||%` <- function(a,b) if(is.null(a)) b else a
results <- list()
for(u in urls) {
  for(method in c("HEAD","GET")) {
    x <- probe(u,method)
    results[[length(results)+1L]] <- x
    cat(sprintf("%s %s : status=%s content_length=%s content_range=%s type=%s final_url=%s error=%s\\n",
      method,u,x$status %||% NA,x$content_length %||% NA,x$content_range %||% NA,
      x$content_type %||% NA,x$final_url %||% NA,x$error %||% "none"))
    if(!is.null(x$data) && nzchar(x$data)) {
      cat("body_excerpt: ", substr(gsub("[\\r\\n]+"," ",x$data),1L,200L),"\\n")
    }
  }
}
dir.create("outputs",showWarnings=FALSE)
saveRDS(results,"outputs/probe_results.rds")

# Inspect link targets in the HTML pages and negotiate machine-readable catalogue data.
for(u in urls[1:2]) {
  rr <- tryCatch(request(u) |> req_user_agent("AGRIS-ODS-feasibility-check/0.1") |>
    req_headers(Accept="application/rdf+xml, application/xml;q=0.9, text/xml;q=0.8") |>
    req_timeout(30) |> req_error(is_error=function(x) FALSE) |> req_perform(),error=identity)
  if(inherits(rr,"error")) {cat("NEGOTIATE ERROR ",u,": ",conditionMessage(rr),"\\n"); next}
  typ <- resp_headers(rr)[["content-type"]] %||% ""
  body <- tryCatch(resp_body_string(rr),error=function(e)"")
  cat(sprintf("NEGOTIATE %s type=%s response_bytes=%d\\n",u,typ,length(resp_body_raw(rr))))
  urls_found <- regmatches(body,gregexpr('(?:href|src)=[\\x22\\x27][^\\x22\\x27]+[\\x22\\x27]',body,perl=TRUE))[[1L]]
  urls_found <- unique(urls_found[grepl("xml|rdf|dcat|zip|download|catalog|agris",urls_found,ignore.case=TRUE)])
  cat("LINKS ",u,": ",paste(head(urls_found,35L),collapse=" ; "),"\\n")
  cat("DOCUMENT_PREFIX ",u,": ",substr(gsub("[\\r\\n]+"," ",body),1L,250L),"\\n")
}
