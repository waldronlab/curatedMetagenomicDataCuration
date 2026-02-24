d = dict()
with open("QinN_RUNS.txt") as xml:
	for line in xml.readlines():
		line = line.rstrip().split()
		name = line[2].split("_")[0]
		if name not in d: d[name] = []
		for runcode in line[4:-2]:
			d[name].append(runcode)			

for k in d: d[k] = ",".join(d[k])
#for k in d: print(k, d[k])
#print ("tot elemen : ", len(list(d.keys())))
 
import pandas as pd 
df = pd.read_table("QinN_2014_metadata.txt", sep="\t", header=0)
runCODE = []
for s in df.StudySampleId.tolist():
	runCODE.append(d[s])

df["RUN_CODE"] = runCODE

df.to_csv("tmp", sep="\t", header=True, index=False)


#ùprint(df)
