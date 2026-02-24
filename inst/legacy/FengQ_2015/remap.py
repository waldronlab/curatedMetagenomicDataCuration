import pandas as pd
"""
d = {}

with open('mapping') as mp:
	for line in mp:
		line = line.rstrip().split()
		d[line[0]] = line[1]

print(d)


df = pd.read_table('ZellerG_2014_metadata.txt', sep='\t', header=0)

cp = df['DiseaseLevel'].tolist()

dic = dict([(s,d) for s,d in zip(df.StudySampleId.tolist(),df.DiseaseLevel.tolist())])

cp2 = []

for s in df.StudySampleId.tolist():
	try:
		cp2.append((dic[s] if d[s]!="Small" else "SmallAdenoma"))
	except:
		cp2.append('Na')

 
#cp2 = [(dic[s] if d[s]!="Small" else "SmallAdenoma") for s in df.StudySampleId.tolist()]#[(c if d[c]!='Small' else 'SmallAdenoma') for c in cp]

#print(cp2, len(cp2))
"""

df = pd.read_table('FengQ_2015_metadata.txt', sep='\t', header=0)

#df['DiseaseLevel'] = cp2
 
disease = []

n = 0
for d,f in zip(df.Disease.tolist(), df.DiseaseLevel.tolist()):
	if f=='Carcinoma': disease.append('CRC')
	elif f in ['LargeAdenoma', 'SmallAdenoma', 'AdvancedAdenoma', 'Adenoma']: disease.append('Adenoma')
	else: disease.append(df.Disease.iloc[n])

	n += 1
	#elif f=='Na': disease.append('N')
	#else: print('cazzo di probellma hai con: ', f)

print (disease, len(disease))
#disease = [(c if df.DiseaseLevel.tolist()[df['Disease']]) for c in df.Disease]

df['Disease'] = disease

df.to_csv('new', sep="\t", header=True, index=False)
