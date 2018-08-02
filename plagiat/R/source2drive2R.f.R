source2drive2R.f <- function(
	d.app='239436372790-rtpllkbjtdkrdbfe1p39fdllauq53dso.apps.googleusercontent.com'
	,d.secret='Y0SnjioP18eOLP4sfqNddO7U'
	,id
	,gid=warning('Consider specifying google id of subsheet')
	,title.search = warning('Consider specifying title of google sheet')
	,filter.extension = ''
	,title.prefix='.*'
	,title.suffix='.*'
	,makedt=T
	,ignore.trash=T
)
{
	mis<-c(id=missing(id),gid=missing(gid),title.search=missing(title.search))
	if(all(mis)) stop('Specify id or title.search')
	if(all(!mis[-2])) {warning('Both id and title.search provided, only id used.')}
	library(RGoogleDrive)
	library(httr)
	library(data.table)
	library(magrittr)
	options(
		drive.app=d.app
		,drive.secret=d.secret
	)
	drive.auth()
	ret <- list()
	if(mis[1]){
		raw.list <- GET("https://www.googleapis.com/drive/v2/files?maxResults=100000",config(token = getOption("drive.auth")))
		parsed.list <- httr::content(raw.list, as = "parsed")
		all.files <- sapply(parsed.list$items, function(x) x$title, simplify = F)
		if(ignore.trash) all.files<-all.files[!sapply(parsed.list$items, function(x) x$labels$trashed, simplify = T)]
		query<-paste("^",title.prefix,title.search, title.suffix,filter.extension,"$", sep = "")
		cat('\nSearch expression:',query)
		target.files <- sapply(
			query
			, function(x) grep(x, all.files, value = T)
			, simplify = F
		)
		if(!sapply(target.files,length) %>% all) {stop('No files found.',call. = F)} else {print(target.files)}
		target.w <- sapply(
			query
			, function(x) grep(x,all.files, value = F)
			, simplify = F)
		for (i in names(target.w)) for (j in 1:length(target.files[[i]])) {
			filesize <- paste(round(as.integer(parsed.list$items[[target.w[[i]][j]]]$fileSize)/(2^20),3), "MB")
			cat(c("\nDownloading ", filesize, " \"", target.files[[i]][j], "\""),
					sep = "")
			dl<-parsed.list$items[[target.w[[i]][j]]]$exportLinks$`text/csv`
			t0 <- proc.time()
			ret[[i]][target.files[[i]][j]] <- list(httr::content(GET(dl,config(token = getOption("drive.auth"))), as = "text"))
			t1 <- proc.time()
			cat("\nDownloaded in", round((t1 - t0)[3]/60, 2), "minutes.")
			if (makedt) {
				cat("\nConverting target text file to data.table.")
				ret[[i]][target.files[[i]][j]] <- list(fread(ret[[i]][[target.files[[i]][j]]],verbose = T))
				cat(" Done.")
			}
			attr(ret[[i]][[target.files[[i]][j]]], "source.file.size") <- filesize
		}
	} else {
		dl<-paste0('https://docs.google.com/spreadsheets/export?id=',id,'&exportFormat=csv',ifelse(mis[2],'',paste0('&gid=',gid)))
		t0 <- proc.time()
		ret <- httr::content(GET(dl,config(token = getOption("drive.auth"))), as = "text")
		t1 <- proc.time()
		cat("\nDownloaded in", round((t1 - t0)[3]/60, 2), "minutes.")
		if (makedt) {
			cat("\nConverting target text file to data.table.")
			ret <- fread(ret,verbose = T)
			cat(" Done.")
		}
	}
	ret
}
