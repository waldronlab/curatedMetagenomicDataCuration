
import pandas as pd

df = pd.read_table('KarlssonFH_2013_metadata.txt', sep="\t", header=0)
d = {}

with open('disease') as D:
	for line in D.readlines():
		line = line.rstrip().split()
		name = 'S' + line[0][3:]
		dise = line[1]
		d[name] = dise

#print(d)

for k in d:
	if d[k] == 'NGT': d[k] = 'N'

df['Disease'] = [d[k] for k in df.StudySampleId.tolist()]
df.to_csv('tmp_2', sep="\t", header=True, index=False)

#print(df)
