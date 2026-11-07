#!/usr/bin/env Rscript
suppressPackageStartupMessages(library(xml2))
# Inspection of selected small FAO AGRIS provider XML files.
# Strictly cap each attempted download at 4 MB and delete temporary files.
providers <- c("BG3","BRI","BRY","R26","TRA","TR8","IN8","GB8","IRC","AS3","USF","BRU")
max_bytes <- 4000000L
get_tags <- function(node) {
  kids <- xml_children(node)
  unique(xml_name(kids))
}
field_text <- function(node, pattern) {
  nodes <- xml_find_all(node, sprintf(".//*[contains(translate(local-name(),'ABCDEFGHIJKLMNOPQRSTUVWXYZ','abcdefghijklmnopqrstuvwxyz'),'%s')]",pattern))
  v <- trimws(xml_text(nodes))
  v <- v[nzchar(v)]
  if(length(v)) paste(unique(v), collapse=" | ") else ""
}
ok <- 0L
for (provider in providers) {
  u <- sprintf("https://agris.fao.org/ods/AGRIS.ODS.%s.xml",provider)
  dest <- tempfile(fileext=".xml")
  args <- c("--location","--silent","--show-error","--max-time","35",
            "--max-filesize",as.character(max_bytes),
            "--output",dest,"--write-out","HTTP=%{http_code} BYTES=%{size_download}",u)
  out <- suppressWarnings(system2("curl",args,stdout=TRUE,stderr=TRUE))
  status <- attr(out,"status")
  if(is.null(status)) status <- 0L
  bytes <- if(file.exists(dest)) file.info(dest)$size else 0
  cat(sprintf("PROVIDER %s curl_status=%d bytes=%s result=%s\n",provider,status,bytes,paste(out,collapse=" ")))
  if(status!=0L||bytes==0L||bytes>max_bytes) {unlink(dest);next}
  xml <- tryCatch(read_xml(dest),error=identity)
  unlink(dest)
  if(inherits(xml,"error")) {
    cat(sprintf("PROVIDER_PARSE_ERROR %s %s\n",provider,conditionMessage(xml)))
    next
  }
  root <- xml_root(xml)
  children <- xml_children(root)
  cat(sprintf("PROVIDER_XML %s root=%s immediate_children=%d child_tags=%s all_element_names=%s\n",
     provider,xml_name(root),length(children),paste(head(get_tags(root),20L),collapse=","),
     paste(head(unique(xml_name(xml_find_all(root,".//*"))),35L),collapse=",")))
  # AGRIS AP typically contains a repeated record element. Prefer named records.
  recs <- xml_find_all(root,".//*[local-name()='record' or local-name()='Record' or local-name()='resource' or local-name()='Resource']")
  if(!length(recs) && length(children)>1L) recs <- children
  if(!length(recs)) recs <- root
  count <- length(recs)
  coverage <- c(title=0L,description=0L,subject=0L,date=0L,identifier=0L)
  for(r in recs) {
    for(k in names(coverage)) {
      value <- field_text(r,k)
      if(nzchar(value)) coverage[[k]] <- coverage[[k]]+1L
    }
  }
  cat(sprintf("PROVIDER_COVERAGE %s records=%d title=%d description=%d subject=%d date=%d identifier=%d\n",
    provider,count,coverage[["title"]],coverage[["description"]],coverage[["subject"]],coverage[["date"]],coverage[["identifier"]]))
  if(count>0L) {
    r <- recs[[1L]]
    cat(sprintf("PROVIDER_SAMPLE %s first_record_tag=%s fields=%s title=%s abstract=%s subjects=%s\n",
      provider,xml_name(r),paste(head(get_tags(r),40L),collapse=","),
      substr(gsub("[\r\n]+"," ",field_text(r,"title")),1L,120L),
      substr(gsub("[\r\n]+"," ",field_text(r,"description")),1L,160L),
      substr(gsub("[\r\n]+"," ",field_text(r,"subject")),1L,160L)))
  }
  ok <- ok+1L
  if(ok>=3L)break
}
cat(sprintf("PROVIDER_PROBE_COMPLETE successful=%d attempted=%d max_download_bytes_each=%d\n",ok,length(providers),max_bytes))
if(ok==0L)stop("No valid provider XML files within download cap")
