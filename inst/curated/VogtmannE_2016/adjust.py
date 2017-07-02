
import pandas as pd

df = pd.read_table('VogtmannE_2016_metadata.txt', sep='\t', header=0)

cp = df.StudySampleId.tolist()
cp1 = df.Subj_Id.tolist()

for el in range(len(cp)):
	cp[el] += "-27-0-0"

for el in range(len(cp1)):
	cp1[el] += "-27-0-0"

df["StudySampleId"] = cp
df["Subj_Id"] = cp
#-27-0-0

del df["RUN_CODE"]

df.to_csv("tmp", sep="\t", header=True, index=False)
