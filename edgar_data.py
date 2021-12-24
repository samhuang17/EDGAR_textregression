import json
import re
import os
import numpy as np 
import pandas as pd 

### Create cik_to_tickers_dict dictionary

file_name = "company_tickers.json" # downloaded from https://www.sec.gov/files/company_tickers.json
cik_to_tickers_dict = {}

f = open(file_name)
file = json.load(f)
for company in file.values():
    if cik_to_tickers_dict.get(str(company["cik_str"]), None):
        cik_to_tickers_dict[str(company["cik_str"])].append(company["ticker"])
    else:
        cik_to_tickers_dict[str(company["cik_str"])] = [company["ticker"]]
f.close()

### Create vocabulary that has w2v embedding

with open("edgar_w2v_200.txt") as f:
    lines = f.readlines()[1:]

w2v_vocab = [line.split()[0] for line in lines]

### Construct tf-idf
term_doc_freq_1A = {}
term_doc_freq_7 = {}
N = 0

for filename in os.listdir("./tf"):
    N += 1
    with open("./tf/{}".format(filename)) as f:
        tf = json.load(f)
    for word in tf["section_1A"]:
        if str(word) in term_doc_freq_1A:
            term_doc_freq_1A[str(word)] += 1
        else:
            term_doc_freq_1A[str(word)] = 1
    for word in tf["section_7"]:
        if str(word) in term_doc_freq_7:
            term_doc_freq_7[str(word)] += 1
        else:
            term_doc_freq_7[str(word)] = 1

top_1A = {}
top_7 = {}

for filename in os.listdir("./tf"):
    with open("./tf/{}".format(filename)) as f:
        tf = json.load(f)
    temp1 = {}
    temp2 = {}
    tot_words_1A = sum(tf["section_1A"].values())
    tot_words_7 = sum(tf["section_7"].values())
    for word in tf["section_1A"]:
        if term_doc_freq_1A[str(word)] < 30:
            temp1[str(word)] = 0
        else:
            temp1[str(word)] = ((tf["section_1A"][str(word)]) / tot_words_1A) * np.log(N / term_doc_freq_1A[str(word)])
    if temp1:
        top_1A[(tf["cik"], tf["year"])] = sorted(temp1, key = temp1.get, reverse = True)[:20]

    for word in tf["section_7"]:
        if term_doc_freq_7[str(word)] < 30:
            temp2[str(word)] = 0
        else:
            temp2[str(word)] = ((tf["section_7"][str(word)]) / tot_words_7) * np.log(N / term_doc_freq_7[str(word)])
    if temp2:
        top_7[(tf["cik"], tf["year"])] = sorted(temp2, key = temp2.get, reverse = True)[:20]


### Extract word embeddings for top words

cik_span = {}
for cik, year in top_7:
    if len(top_7[(cik, year)]) >= 15:
        if cik in cik_span:
            cik_span[cik].append(year)
        else:
            cik_span[cik] = [year]

cik_span_10to19 = [cik for cik, year in cik_span.items() if set(range(2010, 2020)).issubset(set(year))]

with open("edgar_w2v_200.txt") as f:
    lines = f.readlines()[1:]
    w2v_embeddings = {str(line.split()[0]): [float(x) for x in line.split()[1:]] for line in lines}

for i in range(15):
    final_data = []
    for cik in cik_span_10to19:
        if cik in cik_to_tickers_dict.keys():
            for year in range(2010, 2020):
                word = top_7[(cik, year)][i]
                datum = [cik, cik_to_tickers_dict[cik][0], year] + w2v_embeddings[word]
                final_data.append(datum)
    df = pd.DataFrame(final_data)
    df.to_csv("./top_w2v/top_{}.csv".format(i), index = False, header = False)
































