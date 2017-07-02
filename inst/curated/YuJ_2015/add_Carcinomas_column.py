
import pandas as pd

df = pd.read_table('YuJ_2015_metadata.txt', sep='\t', header=0)
c = {'N': 'Na', 'CRC': 'Carcinoma'}

dis = df.Disease.tolist()
col = [c[el] for el in dis]

#print(dis, col)
df.insert(7, 'DiseaseLevel', col)

#print(df)
df.to_csv('tmp', sep='\t', header=True, index=False)


