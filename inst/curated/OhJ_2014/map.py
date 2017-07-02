import pandas as pd

s = set()
m = {}
with open('mapping.txt') as mpp:
	for line in mpp:
		line = line.split()
		name = line[2]
		s.add(name)
		if (not name in m): m[name] = []
		List = line[4:-2]
		if len(List)>len(m[name]): m[name] = List


#for k in m: print(k, m[k])

#print(len(s))

#print(len(list(m.keys())))		

#for j,jj in enumerate(sorted(m.keys())):
#	print(j,jj)

mm = {}
df = pd.read_table('OhJ_2014_metadata.txt', sep='\t', header=0)

ncbi = []

for s in df.StudySampleId.tolist():
	ncbi.append(",".join(m[s]))

df['RUN_CODE'] = ncbi

used = set(df.StudySampleId.tolist())

others = []

for s in m.keys(): 
	if s not in used: others.append(s)

df.to_csv('new', sep='\t', header=True, index=False)
	
print(len(others))


