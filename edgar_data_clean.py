import json

# Prepare cik_to_tickers_dict dictionary

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

# Load EDGAR-CORPUS data

edgar_corpus_data = {}

cik = 1750
corpus_name = "./2020/{}_2020.json".format(cik)

f = open(corpus_name)
corpus = json.load(f)

###
# One might need to tokenize the text data in the below.
###
edgar_corpus_data[str(cik)] = [cik_to_tickers_dict[str(cik)][0], \
                               corpus["section_1A"], \
                               corpus["section_7"]]


f.close()