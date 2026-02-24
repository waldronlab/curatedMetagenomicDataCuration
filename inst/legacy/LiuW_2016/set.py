

import pandas as pd

df = pd.read_table("LiuW_2016_metadata.txt", sep="\t", header=0)

df["PMID"] = ["27708392" for t in range(df.shape[0])]

df["RUN_CODE"] = [df["StudySampleId"].iloc[l] for l in range(df.shape[0])]

print("\t".join(df.columns))
for i in range(df.shape[0]):
    row = "\t".join(list(map(str, [df[c].iloc[i] for c in df.columns])))
    print(row)
