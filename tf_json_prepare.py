### Load EDGAR-CORPUS data and calculate term-frequency

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

