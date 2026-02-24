import pandas as pd

df = pd.read_table("LeChatelierE_2013_metadata.txt", sep="\t", header=0)

cp1,cp2 = [["MetaHIT-"+c for c in df.StudySampleId.tolist()] for i in range(2)] 

del df["StudySampleId"]
del df["Subj_Id"]

df.insert(0, "StudySampleId", cp1)
df.insert(1, "Subj_Id", cp2)
df.to_csv("tmp", sep="\t", header=True, index=False)


