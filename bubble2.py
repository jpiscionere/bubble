import os
import sys
import spacy
en_nlp = spacy.load('en')
import csv
import numpy as np
import pandas


data=pandas.read_table(sys.stdin)
data=np.array(data)
user_keys=data[0:np.shape(data)[0],1]



data=pandas.read_table("tag_sets.csv")
data=np.array(data)
artwork=data[0:np.shape(data)[0],0]



works_index = np.empty(len(user_keys), dtype=str)
new_works = np.empty(len(user_keys), dtype=str)


for i in range(0,len(user_keys)):
	max_spacy_score=0.0
	print(i)
    	try:
        	user2= en_nlp(unicode(user_keys[i]))
    	except ValueError:
		new_works[i]="error"        	
		break
    	for j in range(0,len(artwork)):
		try:
            	art2 = en_nlp(unicode(artwork[j]))
            	spacy_score=art2.similarity(user2)
            	if(spacy_score > max_spacy_score):
                	print(spacy_score)
                	max_spacy_score=spacy_score
                	new_works[i]=artwork[j]
                	works_index[i]=j
        	except ValueError:
            	pass

output=np.stack((data[0:np.shape(data)[0],0],works_index,new_works))
np.savetxt("test.out",output,delimiter=",")
