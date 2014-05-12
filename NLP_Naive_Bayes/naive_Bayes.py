#!/usr/bin/python
from collections import Counter,defaultdict
import sys,re,math,csv
import random
class_col=4
title_col=3
body_col=2
def tokenize(text):
    """breaking text into words"""
    return re.findall('[A-Za-z0-9]+',text.lower())
def tokenize_title_body(title,body):
  """ breaking text into title and body the do not overlap"""
  return ["title: "+ t for t in tokenize(title)] + ["body: " + b for b in tokenize(body)] 

def read_training_file(filename):
    global class_col,title_col,body_col
    priors=Counter()
    likelihood = defaultdict(Counter)
    
    with open(filename) as f:
        reader=csv.reader(f,delimiter=";")
        for line in reader:
            parts = line#.split('\t')
            priors[parts[class_col]] +=1 
            for  word in tokenize_title_body(parts[title_col],parts[body_col]):
                likelihood[parts[class_col]][word.lower()]+=1
    f.close()
    return priors,likelihood
######################BEGINING of CLASSIFIERS
def classify_random (line,priors,likelihood):
    """ return a random category"""
    categories=priors.keys()
    return categories[int(random.random()*len(categories))]

def classify_max_prior(line,priors,likelihood):
    """Return the biggest category """
    return max(priors, key=lambda x: priors[x])

def classify_bayesian(line,priors,likelihood):
  """Return the maximum posterior"""
  global title_col,body_col
  max_class=(-1E6,'')
  for c in priors.keys():
    p=math.log(priors[c])
    n=float(sum(likelihood[c].values()))
    for word in tokenize_title_body(line[title_col],line[body_col]):
      p = p + math.log(max(1E-6,likelihood[c][word.lower()] / n))
    if p >max_class[0]:
      max_class=(p,c)
  return max_class[1]

######################END of CLASSIFIERS  
def read_testing_file(filename):
    f=open (filename)    
    return csv.reader(f,delimiter=";")

def main():
    training_file=sys.argv[1]
    testing_file=sys.argv[2]
    
    (priors,likelihood)=read_training_file(training_file)
    lines=read_testing_file(testing_file)
    num_correct=0
    #####options:classify_bayesian,classify_random,classify_max_prior
    classify=classify_bayesian
    count=0
    for line in lines:
        count+=1
        if classify(line,priors,likelihood)==line[class_col]: ##subject of webpage  
           num_correct+=1

    print "Classified %d correctly out of %d for an accuracy of %f" %(num_correct,count,float(num_correct)/count)

if __name__=='__main__':
    main()
    
