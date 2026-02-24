import pandas as pd 

df = pd.read_table('HMP_2012_metadata.txt', sep='\t', header=0)

cp = [('HMP_2012_'+str(c)) for c in df.subjectID.tolist()]

df["subjectID"] = cp

df.to_csv("new", sep="\t", header=True, index=False)
