#!/usr/bin/env Rscript
suppressPackageStartupMessages({library(xml2); library(curl)})
dir.create("outputs", recursive=TRUE, showWarnings=FALSE)
caturl <- "https://agris.fao.org/ods/AGRIS.ODS.xml"
tmp <- tempfile(fileext=".xml")
curl::curl_download(caturl,tmp,quiet=TRUE)
doc <- xml2::read_xml(tmp); unlink(tmp)
src <- as.character(doc)
files <- sort(unique(regmatches(src,gregexpr("AGRIS\\.ODS\\.[A-Za-z0-9_-]+\\.xml",src,perl=TRUE))[[1]]))
stopifnot(length(files)>0)
# Systematic sample across the complete alphabetically sorted list, not a random cluster.
n <- min(50L,length(files))
idx <- unique(as.integer(round(seq(1,length(files),length.out=n))))
chosen <- files[idx]
cap <- 250 * 1024^2 # 250 MiB per provider, no oversized files stored
rows <- vector("list", length(chosen))
for (i in seq_along(chosen)) {
  file <- chosen[[i]]
  url <- paste0("https://agris.fao.org/ods/",file)
  dest <- tempfile(fileext=".xml")
  handle <- curl::new_handle(followlocation=TRUE,timeout=75,connecttimeout=15,
    useragent="FAO-AGRIS-size-sample/0.3")
  # curl's progress callback aborts when download grows beyond the safety cap.
  # Failures are reported; files are deleted on every iteration.
  exceeded <- FALSE
  cb <- function(down,total,up,uptotal) {
    if (down > cap || (total > cap && total > 0)) {exceeded <<- TRUE;return(1L)}
    0L
  }
  curl::handle_setopt(handle, noprogress=FALSE, progressfunction=cb)
  out <- tryCatch(curl::curl_fetch_disk(url,dest,handle=handle),error=identity)
  bytes <- if(file.exists(dest)) as.numeric(file.info(dest)$size) else NA_real_
  unlink(dest)
  if(inherits(out,"error")) {
    rows[[i]] <- data.frame(file=file,status=NA_integer_,bytes=bytes,complete=FALSE,
      error=if(exceeded) "over_250_MiB_cap" else conditionMessage(out))
  } else {
    rows[[i]] <- data.frame(file=file,status=as.integer(out$status_code),bytes=bytes,
      complete=out$status_code==200L,error="")
  }
  cat(i,"/",length(chosen),file,"bytes",rows[[i]]$bytes,"status",rows[[i]]$status,
      rows[[i]]$error,"\n")
  Sys.sleep(2)
}
out <- do.call(rbind, rows)
write.csv(out,"outputs/agris_provider_size_sample.csv",row.names=FALSE)
good <- out$complete & is.finite(out$bytes)
if(any(good)) {
  sizes <- out$bytes[good]
  stats <- data.frame(total_providers=length(files),sampled=nrow(out),
    successful=sum(good),failed=sum(!good),median_MiB=median(sizes)/1024^2,
    p90_MiB=unname(quantile(sizes,.9))/1024^2,max_MiB=max(sizes)/1024^2,
    sampled_complete_GiB=sum(sizes)/1024^3,
    naive_extrapolated_GiB=mean(sizes)*length(files)/1024^3)
  write.csv(stats,"outputs/agris_provider_size_sample_summary.csv",row.names=FALSE)
  print(stats,row.names=FALSE)
}
