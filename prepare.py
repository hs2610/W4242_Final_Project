import re

f = open('../Data/train.tsv', 'r')
content = f.read()
f.close()
lines = content.split("\r")
for i in range(len(lines)):
  if i==0:
    continue
  pieces = lines[i].split("\t")
  f = open("../Data/train/%s" % pieces[0], "w")
  f.write(re.sub('"', '', pieces[2]))
  f.write("\n")
  f.close()

f = open('../Data/test.tsv', 'r')
content = f.read()
f.close()
lines = content.split("\r")
for i in range(len(lines)):
  if i==0:
    continue
  pieces = lines[i].split("\t")
  f = open("../Data/test/%s" % pieces[0], "w")
  f.write(re.sub('"', '', pieces[2]))
  f.write("\n")
  f.close()
