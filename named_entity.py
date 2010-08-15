# Nicolas Della Penna me@nikete.com
# 15/8/2010
# Strips the sentences in the textual entries in afg.csv and puts them one per line for SENNA to input

import csv, os
f = csv.reader(open('afg.csv','r'))
#"ReportKey","DateOccurred","Type","Category","TrackingNumber","Title","Summary","Region","AttackOn","ComplexAttack","ReportingUnit","UnitName","TypeOfUnit","FriendlyWIA","FriendlyKIA","HostNationWIA","HostNationKIA","CivilianWIA","CivilianKIA","EnemyWIA","EnemyKIA","EnemyDetained","MGRS","Latitude","Longitude","OriginatorGroup","UpdatedByGroup","CCIR","Sigact","Affiliation","DColor","Classification"

w = open('afg.text','w')
index = open('afg.textReportKey','w')
for l in f:
   for s in l[6].strip('\n').strip('\t').split('.'):
     w.write(s+'\n')
     index.write(l[0])
w.close()
index.close()

os.system("./senna-osx -ner < afg.text > afg.ner")
