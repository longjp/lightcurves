import glob

def compile_feats_file(folder,extension,fileout):
    filepaths = glob.glob(("%s/*" + extension) % (folder))
    feat_string = []
    for i in filepaths:
        f = open(i,'r')
        feats = map(lambda x:x.split(" ")[1].replace('\n',""),f.readlines())
        f.close()
        feats.append(i)
        feat_string.append(feats)


    g = open(fileout,'w')
    f = open(i,'r')
    feats_names = map(lambda x:x.split(" ")[0].replace('\n',""),f.readlines())
    f.close()

    g.write(' '.join(feats_names) + " filename\n")

    for i in feat_string:
        g.write(' '.join(i) + "\n")

    g.close()

if __name__ == "__main__":
    ## process original sources
    print "processing rr_orig files"
    folder = "../data_processed/rr_orig/"
    extension = ".dat"
    fileout = "../data_processed/rr_orig.dat"
    compile_feats_file(folder,extension,fileout)

    ## process residual sources
    print "processing rr_residual files"
    folder = "../data_processed/rr_residual/"
    extension = ".dat"
    fileout = "../data_processed/rr_residual.dat"
    compile_feats_file(folder,extension,fileout)
