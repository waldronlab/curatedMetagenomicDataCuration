import pandas as pd
df = pd.read_table("Obregon-TitoAJ_2015_metadata.txt", sep="\t", header=0)
d = {}
with open("runs.txt") as runs:
	for line in runs.readlines():
		line = line.rstrip().split()
		sample = line[2]
		runs = line[4:-2]
		d[sample] = ",".join(runs)

#print(df.StudySampleId.tolist())
#print ( d)

run = []
for c in df.StudySampleId.tolist():
	run.append(d[c])
#print (run)

df["RUN_CODE"] = run
#print(df)


df.to_csv("tmp", sep="\t", header=True, index=False)
