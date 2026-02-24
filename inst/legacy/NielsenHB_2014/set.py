

import pandas as pd
d = {}
d2 = {}
with open("mapping.txt") as mapping:
	for line in mapping.readlines():
	
		line = line.rstrip().split()
		sample = line[2]
		if not sample.startswith("M"):
			realsample = sample.replace(".", "_")
			realsample = realsample.replace("-", "_")
			reads = line[4:-2]
			d[realsample] = sample
			d2[sample] = ",".join(reads)		
#print(d, d2)

df = pd.read_table("NielsenHB_2014_metadata.txt", sep="\t", header=0)

S = df.StudySampleId.tolist()
R = df.RUN_CODE.tolist()

N = []

for i,j in zip(S, R):
	#print (i,j)
	if j != "Na": N.append(j)
	else:
		N.append(d2[d[i]])

df["RUN_CODE"] = N

df.to_csv("tmp", sep="\t", header=True, index=False)

#for h,l in zip(S, N):
#	print(h,l)
#print (N)




"""
cp0, cp1 = ["" for t in range(df.shape[0])], ["" for j in range(df.shape[0])]
con = {"StudySampleId":cp0,"Subj_Id":cp1}


for column in ["StudySampleId","Subj_Id"]:
    for y in range(df.shape[0]):
        dot = False
        sample = [letter for letter in df[column].iloc[y]]
        for le in range(len(sample)):
            if sample[le]=="_" and (not dot): 
                sample[le] = "."        
                dot = True
                #print(sample)
            if sample[le] =="_" and dot:
                sample[le] = "-"
                dot = False
                #print(sample)
        #cp0[y] = "".join(sample)
        #print("".join(sample))
        #cp1[y] = "".join("".join(sample).split("-")[0])

        #for letter in sample:
        #     if (letter=="_" and (not dot)): 
         #        sample=sample.replace("_",".")
         #        dot=True
             #if (letter=="." and dot): 
                 #sample=sample.replace("_","-")
              #   dot=False
         #    con[column][y] = sample


print(cp0)
print()
print(cp1)

df["StudySampleId"] = cp0
df["Subj_Id"] = cp1
"""         
#print(df)
