import sys
import os
import re
import shutil

hfilename = os.path.normpath(sys.argv[1])
print "Get exports from: " + hfilename

projectbasename = os.path.splitext(os.path.split(hfilename)[1])[0]
projectbasedir = os.path.split(hfilename)[0] + "\\"

exportregex = re.compile("__cdecl\s+(\w+)\(")

deffilename = projectbasedir + projectbasename + ".def"

deffile = open(deffilename, "w");

print >> deffile, "LIBRARY " + projectbasename
print >> deffile, "EXPORTS"

hlines = open(hfilename, "r").readlines()
for line in hlines:
	match = exportregex.search(line)
	if match:
		#print >> deffile, "\t_" + match.group(1) + "=" + match.group(1)
		print >> deffile, "\t" + match.group(1)