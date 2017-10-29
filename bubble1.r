#I'm going to write my R code like C code so it's easy to follow. For this iteration, I'm only going to recommend 1 keyword.  This is obviously not correct. 


# Load the text mining libraries.
library("tm")
library("SnowballC")
library("wordcloud")
library("spacyr")


#Read in the raw tags, turn it into a 'corpus' (like a book) and take out all the 'stopwords' (filler words)
text <- readLines("tag_sets.csv")
docs <- Corpus(VectorSource(text))
docs <- tm_map(docs, removeWords, stopwords("english"))


#Find the most common words in the corpus

dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
top_words=as.character(d$word[c(1:100)])

#Set the correlation limit to an arbitrary value. The lower the corlimit, the bigger your 'network' will be. I set it small
#so that the network would encompass most of the user tags. The higher the limit, the more accurate the matches you do find, but there's a higher likelihood you won't get a match in the first place.

corlimits=rep(0.1,length(top_words))

#make the correlation model by finding the correlation coefficient between the 100 top words and all other tags
model=findAssocs(dtm, terms = top_words,corlimit=corlimits)


#put the model in a searchable format
list_of_names=names(unlist(model))
c=as.data.frame(unlist(model))
list_of_corvalues=c[,1]
new_keywords=c(1:length(text))*0

#loop over all the sets of artist tags to find a recommended keyword.

for(i in c(1:length(text))){
	a=unlist(strsplit(text[i],","))
	keyword_length=length(a)
	toMatch=paste(a,collapse="|")
    
	#I'm going to take the most common keyword here for the matching. This is NOT the best way of doing this.
    if(nchar(text[i]) > 2000){ #this statement gets rid of super long keys
      a=unlist(strsplit(text[i]," "))[1:100]  
      print(a)
    }
    
    a.topword=top_words[which(top_words %in% a)][1] #get the top word
	
#if topword exists, get the keyword that correlates the strongest with topword	
	if(!is.na(a.topword)){	
		first_try=list_of_names[which(grepl(a.topword,list_of_names)=="TRUE")]
		x=strsplit(first_try,"\\.")
		x=sapply(x,"[",2)
		list_of_possiblities=x[!x %in% a]
		if(length( list_of_possiblities ) >0){
			new_keywords[i]=list_of_possiblities[1]
		}else{
			new_keywords[i]=sample(top_words,1) #hack
		}
#if there isn't a top word, find the best top word
	} else if(length(grep(toMatch,list_of_names) > 0)){ 

		matches=list_of_names[grep(toMatch,list_of_names)]
		x=strsplit(matches,"\\.")
		x=sapply(x,"[",1)
		n <- length(unique(x))

        if(n ==1){
            new_keywords[i]=x[1]
        }else{
            count_most_common=sort(table(x),partial=n)[[n]]		
            count_second=sort(table(x),partial=n-1)[[n-1]]			
		
            if(count_most_common - count_second > 4){		
                new_keywords[i]=names(sort(table(x),decreasing=TRUE)[1])
            } else {
                new_keywords[i]=x[order(match(x,top_words))][1]
            }    
        }
        
	}else{

	
		new_keywords[i]=sample(top_words,1)


	} 
		
}

output=data.frame(new_keywords)
write.table(output,"bubble1.txt",sep=",",quote = FALSE,col.names = FALSE,row.names = FALSE)

