import os
import re
import sys

fileList = []
rootDir = "timingRuns"
originalRuntime = 0.0
mpiRuns = {}

for root, subFolders, files in os.walk(rootDir):
    for filename in files:
        if filename.lower().endswith(".txt"):
            fileList.append(os.path.join(root, filename))

for path in fileList:
    runLog = open(path, "r")
    filename = path.split("/")[-1]
    runtime = 0.0
    processCount = 1
    processes = re.findall("\d+", filename)
    for i in processes:
        processCount *= int(i)
    for line in runLog:
        if re.match(" Total time:", line):
            floats = re.search("\d+.\d+", line)
            runtime = max(runtime, float(floats.group()))
    if len(path.split("/")) == 4:
        if originalRuntime != 0.0:
            print("Error, two original runs found")
            sys.exit(-1)
        originalRuntime = runtime
    else:
        try:
            mpiRuns[processCount] = min(mpiRuns[processCount], runtime)
        except KeyError:
            mpiRuns[processCount] = runtime

print("Original single threaded code executed in " + str(originalRuntime) + " seconds")

for key in mpiRuns.keys():
    print(str(key) + ", " + str(mpiRuns[key]))
