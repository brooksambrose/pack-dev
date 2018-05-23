#' Browse ICPSR records using either speech or speaker IDs.
#' @param icpsr A CTAWG list of 3 data frames named according to ICPSR plain text tables and containing the "GPOspeech", "SpeakerID", and "GPOdescr" substrings.
#' @param speechID The name of the field containing the speech ID. First three digits indicate the nth Congress, next 4 indicate the year, following 7 are the numerical index in order of appearance.
#' @param speakerID Speaker ID, which the function links via the "GPOdesc" table to merge speaker and speech data.
browse.icpsr<-function(icpsr,speechID=NULL,speakerID=NULL,print.max=50){
	require(data.table,quietly = T)
	if(is.null(speechID)&is.null(speakerID)) stop('Must specify either speechID or speakerID.')

	icpsr<-icpsr.cong107
	for(i in grep('GPOspeech',names(icpsr))) icpsr[[i]]$speechID<-as.character(icpsr[[i]]$speechID)
	GPOspeech<-rbindlist(icpsr[grep('GPOspeech',names(icpsr))])
	icpsr[grep('GPOspeech',names(icpsr))]<-NULL
	setkey(GPOspeech,speechID)

	for(i in grep('SpeakerID',names(icpsr))) icpsr[[i]]$id<-as.character(icpsr[[i]]$id)
	SpeakerID<-rbindlist(icpsr[grep('SpeakerID',names(icpsr))])
	icpsr[grep('SpeakerID',names(icpsr))]<-NULL
	setnames(SpeakerID,'id','speakerID')
	setkey(SpeakerID,speakerID)

	for(i in grep('GPOdesc',names(icpsr))) {
		icpsr[[i]]$speechID<-as.character(icpsr[[i]]$speechID)
		icpsr[[i]]$speakerID<-as.character(icpsr[[i]]$speakerID)
	}
	GPOdesc<-rbindlist(icpsr[grep('GPOdesc',names(icpsr))])
	icpsr[grep('GPOdesc',names(icpsr))]<-NULL

	if(!is.null(speechID)){
		speechID<-as.character(speechID)
		setkey(GPOdesc,speechID)
		ret<-merge(GPOdesc,GPOspeech[speechID])
		setkey(ret,speakerID)
		ret<-merge(SpeakerID,ret)
		if(nrow(ret)<print.max) {cat(paste('Printing first',floor(print.max),'documents.\n'));for(i in 1:ifelse(nrow(ret)>print.max,print.max,nrow(ret))) {print(t(ret[i]));cat('\n')}}
		return(ret)
	}
	if(!is.null(speakerID)){
		speakerID<-as.character(speakerID)
		setkey(GPOdesc,speakerID)
		ret<-merge(GPOdesc,SpeakerID[speakerID])
		setkey(GPOdesc,speechID)
		setkey(ret,speechID)
		ret<-merge(ret,GPOspeech)
		cat(paste('Printing first',floor(print.max),'documents.'))
		if(nrow(ret)<print.max) {cat(paste('Printing first',floor(print.max),'documents.\n'));for(i in 1:ifelse(nrow(ret)>print.max,print.max,nrow(ret))) {print(t(ret[i]));cat('\n')}}
		return(ret)
	}
}
