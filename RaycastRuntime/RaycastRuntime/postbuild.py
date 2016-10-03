import sys
import os
import re
import shutil
from subprocess import call

# python postbuild.py path\to\dll\project_name.dll path\to\file\copy_file_1 path\to\file\copy_file_2 ... path\to\dir\copy_to

dllfilename = os.path.normpath(sys.argv[1])
outputdir = os.path.normpath(sys.argv[len(sys.argv) - 1]) + "\\"

print "Get exports from: " + dllfilename
for i in range(2, len(sys.argv) - 1):
	print "Copy " + sys.argv[i]
print "Writing def to: " + outputdir

projectbasename = os.path.splitext(os.path.split(dllfilename)[1])[0]

shutil.copy(dllfilename, outputdir)
for i in range(2, len(sys.argv) - 1):
	shutil.copy(sys.argv[i], outputdir)

data = os.popen("dumpbin /exports \"" + sys.argv[1] + "\"").readlines()

exportregex = re.compile("\s*\d+\s+\d+\s+[A-Z0-9]+\s+(\w+)\s+=")

deffilename = outputdir + projectbasename + ".def"

deffile = open(deffilename, "w");

print >> deffile, "LIBRARY " + projectbasename
print >> deffile, "EXPORTS"

for line in data:
	match = exportregex.match(line)
	if match:
		print >> deffile, "\t" + match.group(1)

deffile.close()		
		
os.chdir(outputdir)
args = ["dlltool", "-d", projectbasename + ".def", "-l", "lib" + projectbasename + ".a"]

res = call(args)

sys.exit(res)
		
# "%MINGWHOME%\bin\dlltool" -d "$(ProjectDir)$(ProjectName).def" -l "$(ProjectDir)..\..\..\haskell\lib$(ProjectName).a"		
# copy "$(TargetDir)$(ProjectName).dll" "$(ProjectDir)..\..\..\haskell\$(ProjectName).dll";python "$(ProjectDir)..\..\..\gendef.py" "$(ProjectDir)..\..\..\$(ProjectName).dll" > "($ProjectDir)..\..\..\$(ProjectName).def"