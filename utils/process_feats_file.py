import glob


folder = "../data_processed/eclipse/"
extension = ".dat"
filepaths = glob.glob(("%s/*" + extension) % (folder))


feat_string = []

for i in filepaths:
    f = open(i,'r')
    feats = map(lambda x:x.split(" ")[1].replace('\n',""),f.readlines())
    f.close()
    feats.append(i)
    feat_string.append(feats)





fileout = "../data_processed/eclipse1.dat"
g = open(fileout,'w')

f = open(i,'r')
feats_names = map(lambda x:x.split(" ")[0].replace('\n',""),f.readlines())
f.close()

g.write(' '.join(feats_names) + " filename\n")

for i in feat_string:
    g.write(' '.join(i) + "\n")

g.close()
