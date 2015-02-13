import os
import re
import sys

fileList = []
rootDir = "timingRuns/"
originalRuntime = 0.0
originalIFBF0Runtime = 0.0
mpiRuns = {}
mpiExpandingAreaRuns = {}
mpiExactCornerRuns = {}
mpiExactCornerExpandingAreaRuns = {}

for root, subFolders, files in os.walk(rootDir):
    for filename in files:
        if filename.lower().endswith(".txt") and not filename.lower().endswith("_summary.txt"):
            fileList.append(os.path.join(root, filename))

for path in fileList:
    runLog = open(path, "r")
    summaryLogPath = path[:-4] + "_summary.txt"
    if os.path.isfile(summaryLogPath):
        print(summaryLogPath + " already exists")
        continue
    summaryLog = open(path[:-4] + "_summary.txt", "w")
    filename = path.split("/")[-1]
    for line in runLog:
        if re.match(" Total time:", line):
            summaryLog.writelines(line)
    runLog.close()
    summaryLog.close()
