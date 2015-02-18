import os
import re
import sys

# Known Limitations:
# 1. Doesn't deal with multiple hosts
# 2. Doesn't deal with multiple GMCF Versions on a single host
# 3. Only supports les_main.txt, les_main_ifbf=0.txt, SharedMemory/*.txt, SharedMemoryExpandingArea/*.txt,
#    SharedMemoryExactCorners/*.txt, and SharedMemoryExactCornersExpandingArea/*.txt

if len(sys.argv) != 2:
    print("Please run this script with a starting directory as a command line argument.")

fileList = []
rootDir = sys.argv[1] # For example, timingRuns/<hostname>/<gmcf-configuration>
gmcfRuns = {}
gmcfExpandingAreaRuns = {}
gmcfExactCornerRuns = {}
gmcfExactCornerExpandingAreaRuns = {}

for root, subFolders, files in os.walk(rootDir):
    for filename in files:
        if filename.lower().endswith("_summary.txt"):
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
    runLog.close()
    if runtime == 0.0:
        continue
    # Found a runtime for the GMCF code where total area is 150x150x90
    if "/SharedMemory/" in path:
        try:
            gmcfRuns[processCount] = min(gmcfRuns[processCount], runtime)
        except KeyError:
            gmcfRuns[processCount] = runtime
    # Found a runtime for the GMCF code where each process has its own 150x150x90 area
    elif "/SharedMemoryExpandingArea/" in path:
        try:
            gmcfExpandingAreaRuns[processCount] = min(gmcfExpandingAreaRuns[processCount], runtime)
        except KeyError:
            gmcfExpandingAreaRuns[processCount] = runtime
    # Found a runtime for the GMCF code where total area is 150x150x90 with exact corner code
    elif "/SharedMemoryExactCorners/" in path:
        try:
            gmcfExactCornerRuns[processCount] = min(gmcfExactCornerRuns[processCount], runtime)
        except KeyError:
            gmcfExactCornerRuns[processCount] = runtime
    # Found a runtime for the GMCF code where each process has its own 150x150x90 area with exact corner code
    elif "/SharedMemoryExactCornersExpandingArea/" in path:
        try:
            gmcfExactCornerExpandingAreaRuns[processCount] = min(gmcfExactCornerExpandingAreaRuns[processCount], runtime)
        except KeyError:
            gmcfExactCornerExpandingAreaRuns[processCount] = runtime
    else:
        print(path + " is unexpected.")

print("\nSharedMemory Runs")
for key in gmcfRuns.keys():
    print(str(key) + ", " + str(gmcfRuns[key]))

print("\nSharedMemoryExpandingArea Runs")
for key in gmcfExpandingAreaRuns.keys():
    print(str(key) + ", " + str(gmcfExpandingAreaRuns[key]))

print("\nSharedMemoryExactCorners Runs")
for key in gmcfExactCornerRuns.keys():
    print(str(key) + ", " + str(gmcfExactCornerRuns[key]))

print("\nSharedMemoryExactCornersExpandingArea Runs")
for key in gmcfExactCornerExpandingAreaRuns.keys():
    print(str(key) + ", " + str(gmcfExactCornerExpandingAreaRuns[key]))
