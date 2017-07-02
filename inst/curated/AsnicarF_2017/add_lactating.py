import pandas as pd
df = pd.read_table("AsnicarF_2017_metadata.txt", sep="\t", header=0)

df.insert(19, "pregnant", ["no" for s in df.sampleID.tolist()])#[("yes" if s[5]=="M" else "no") for s in df.sampleID.tolist()])
df.insert(20, "lactating", [("yes" if s[5]=="M" else "no") for s in df.sampleID.tolist()])

df.to_csv("new", sep="\t", header=True, index=False)
#print(df)
