import json
import re
import os
import numpy as np
import pandas as pd

### Create cik_to_tickers dictionary

file_name = "company_tickers.json"
cik_to_tickers_dict = {}

f = open(file_name)
file = json.load(f)
for company in file.values():
    if cik_to_tickers_dict.get(str(company["cik_str"]), None):
        cik_to_tickers_dict[str(company["cik_str"])].append(company["ticker"])
    else:
        cik_to_tickers_dict[str(company["cik_str"])] = [company["ticker"]]
    
f.close()

### Construct vocabulary that has w2v embeddings

with open("edgar_w2v_200.txt") as f:
    lines = f.readlines()[1:]

w2v_vocab = [line.split()[0] for line in lines]

### Load EDGAR-CORPUS data

def tf_dict(corpus, vocab):
    corpus_exp = re.findall(r'\w+', corpus.lower())
    res = {}
    for word in corpus_exp:
        if word in vocab:
            if word in res.keys():
                res[word] += 1
            else:
                res[word] = 1
    return res

for year in range(2010, 2020):
    for filename in os.listdir("./{}".format(year)):
        temp = {}
        with open("./{}/{}".format(year, filename), 'r') as f:
            corpus = json.load(f)
        tf_7A = tf_dict(corpus["section_7A"], w2v_vocab) # only include terms that are in w2v_vocab
        temp = {"year": year, "cik": corpus["cik"], "section_7A": tf_7A}
        with open("./tf/{}_{}.json".format(corpus["cik"], str(year)), 'w') as outfile:
            json.dump(temp, outfile)

### Construct tf-idf

term_doc_freq_7A = {}
N = 0

for filename in os.listdir("./tf"):
    with open("./tf/{}".format(filename)) as f:
        tf = json.load(f)
        # tf only contains tokens that are in w2v_vocab
    if len(tf["section_7A"]) > 15: # only conisder documents with more than 15 distinct tokens
        N += 1
        for word in tf["section_7A"]:
            if str(word) in term_doc_freq_7A:
                term_doc_freq_7A[str(word)] += 1
            else:
                term_doc_freq_7A[str(word)] = 1

top_7A = {}

for filename in os.listdir("./tf"):
    with open("./tf/{}".format(filename)) as f:
        tf = json.load(f)
    if len(tf["section_7A"]) > 15: # only count documents with more than 15 distinct tokens
        temp = {}
        tot_words_7A = sum(tf["section_7A"].values())
        for word in tf["section_7A"]:
            if str(word) not in temp: 
                temp[str(word)] = ((tf["section_7A"][str(word)]) / tot_words_7A) * np.log(N / term_doc_freq_7A[str(word)])
            else:
                print("duplicate word found: {} in {}, Section 7A".format(word, filename))
        if temp:
            top_words = sorted(temp, key = temp.get, reverse = True)[:30]
            top_7A[(tf["cik"], tf["year"])] = [(word, temp[word]) for word in top_words]

### Extract tfidf-weighted word embeddings for top words

cik_span = {}
for cik, year in top_7A:
    if cik in cik_span:
        cik_span[str(cik)].append(year)
    else:
        cik_span[str(cik)] = [year]
        
cik_span_10to19 = [cik for cik, year in cik_span.items() if set(range(2010, 2020)).issubset(set(year))]

with open("edgar_w2v_200.txt") as f:
    lines = f.readlines()[1:]
    w2v_embeddings = {str(line.split()[0]): [float(x) for x in line.split()[1:]] for line in lines}

for i in range(15):
    final_data = []
    for cik in cik_span_10to19:
        if cik in cik_to_tickers_dict:
            for year in range(2010, 2020):
                word = top_7A[(cik, year)][i][0]
                tfidf = top_7A[(cik, year)][i][1]
                denom = np.sum([top_7A[(cik, year)][j][1] for j in range(15)])
                datum = [cik, cik_to_tickers_dict[cik][0], year] + [v * tfidf / denom for v in w2v_embeddings[word]]
                final_data.append(datum)
    df = pd.DataFrame(final_data)
    df.to_csv("./top_w2v/top_{}.csv".format(i), index = False, header = False)



