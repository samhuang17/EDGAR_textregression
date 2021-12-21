import json
import re
import os

# Create vocabulary that has w2v embedding

with open("edgar_w2v_200.txt") as f:
	lines = f.readlines()[1:]

w2v_vocab = [line.split()[0] for line in lines]

# Load EDGAR-CORPUS data and calculate term-frequency

def tf_dict(corpus, vocab):
	corpus_exp = re.findall(r'w\+', corpus.lower())
	res = {}
	for word in corpus_exp:
		if word in vocab:
			if word in res.keys():
				res[word] += 1
			else:
				res[word] = 1
	return res

for year in range(2010, 2021):
	for filename in os.listdir("./{}".format(year)):
		temp = {}
		with open("./{}/{}".format(year, filename), 'r') as f:
			corpus = json.load(f)
		tf_1A = tf_dict(corpus["section_1A"], w2v_vocab)
		tf_7 = tf_dict(corpus["section_7"], w2v_vocab)
		temp = {"year": year, "cik": corpus["cik"], "section_1A": tf_1A, "section_7": tf_7}
		with open("./tf/{}_{}.json".format(corpus["cik"], str(year)), 'w') as outfile:
			json.dump(temp, outfile)

# Create cik_to_tickers_dict dictionary

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

# to do