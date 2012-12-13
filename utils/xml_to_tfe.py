## convert an .xml vosource file into a file with just tfe

import glob
import re

folder = "../data/asas_ACVS_50k_new_aper_20120221"
out_folder = "../data/asas_tfe/"
extension = ".xml"
filepaths = glob.glob(("%s/*" + extension) % (folder))

for i in filepaths:
    print i
    f = open(i,'r')
    data = f.read()
    f.close()
    data = data.split("<TABLEDATA>")[1].split("</TABLEDATA>")[0][1:]
    data = re.sub("<TD>","",data)
    data = re.sub("</TD>"," ",data)
    data = re.sub("</TR>","",data)
    data = re.sub("<TR row='\w{1,}'>","",data)
    data = re.sub("\t","",data)
    g = open(out_folder + i.split("/")[-1],'w')
    g.write(data)
    g.close()


