typ <- function(){
	hier<-rev(c('sub','par','super'))
	d<-lapply(list(
		relevance=list(closure=c('include','tend','exclude'),expectation=c('irregular','regular'))
		,relation=list(cast=hier,catch=hier)
		,meaning=list(affect=c('attracted','neutral','repulsed'),codification=c('explicit','implicit','cryptic'))
	),function(x) array(rep('',prod(sapply(x,length))),dim=sapply(x,length),dimnames=x))
	d$relevance['exclude','regular']<-'abject'
	d$relevance['exclude','irregular']<-'reject'
	d$relevance['tend',]<-'object'
	d$relevance['include',]<-'retroject'

	d$relation[throw='super',catch='sub']<-'subject' # stable
	d$relation[throw='par',catch='par']<-'citizen' # stable
	d$relation[throw='sub',catch='super']<-'subject' # stable

	d$relation[throw='super',catch='super']<-'contest' # strained
	d$relation[throw='super',catch='par']<-'challenge' # strained
	d$relation[throw='par',catch='super']<-'challenge' # strained
	d$relation[throw='par',catch='sub']<-''  # strained
	d$relation[throw='sub',catch='par']<-'' # strained
	d$relation[throw='sub',catch='sub']<-'genteel' # strained

	d$process<-data.frame(matrix(letters[1:4],nrow=2,ncol=2,dimnames=list(Scale=c('Micro','Macro'),Mechanism=c('Cultural','Social'))))

	d$stages<-data.frame(
		Stage=c('Prototyping','Assemblage','Facilitation','Accumulation')
		,Sensemaking=c('Obsession','Recognition','Mastery','Competition')
		,Control=c('Private','Peer','Provider','Professional')
	)

		#should be an "editing" nuance where throw is negotiated, a catch is refused while throw is appealed
	d
}
