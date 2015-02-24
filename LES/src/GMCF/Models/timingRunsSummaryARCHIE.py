import os
import re
import sys

if len(sys.argv) != 2:
    print("Please run this script with a starting directory as a command line argument.")

fileList = []
rootDir = sys.argv[1] # For example, timingRuns/archie/<mpi-configuration>
originalRuntime = 0.0
originalIFBF0Runtime = 0.0
archieRuns = {}
archieExpandingAreaRuns = {}
archieExactCornerRuns = {}
archieExactCornerExpandingAreaRuns = {}

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
    if path.endswith("les_main_summary.txt"):
        if originalRuntime != 0.0:
            print("Error, two original runs found")
            sys.exit(-1)
        originalRuntime = runtime
    # Found the runtime for the original code but with IFBF=0
    elif path.endswith("les_main_IFBF0_summary.txt"):
        if originalIFBF0Runtime != 0.0:
            print("Error, two ifbf=0 runs found")
            sys.exit(-1)
        originalIFBF0Runtime = runtime
    # Found a runtime for the GMCF code where total area is 150x150x90
    elif "/MPI_SharedMemory/" in path:
        try:
            archieRuns[processCount] = min(archieRuns[processCount], runtime)
        except KeyError:
            archieRuns[processCount] = runtime
    # Found a runtime for the GMCF code where each process has its own 150x150x90 area
    elif "/MPI_SharedMemoryExpandingArea/" in path:
        try:
            archieExpandingAreaRuns[processCount] = min(archieExpandingAreaRuns[processCount], runtime)
        except KeyError:
            archieExpandingAreaRuns[processCount] = runtime
    # Found a runtime for the GMCF code where total area is 150x150x90 with exact corner code
    elif "/MPI_SharedMemoryExactCorners/" in path:
        try:
            archieExactCornerRuns[processCount] = min(archieExactCornerRuns[processCount], runtime)
        except KeyError:
            archieExactCornerRuns[processCount] = runtime
    # Found a runtime for the GMCF code where each process has its own 150x150x90 area with exact corner code
    elif "/MPI_SharedMemoryExactCornersExpandingArea/" in path:
        try:
            archieExactCornerExpandingAreaRuns[processCount] = min(archieExactCornerExpandingAreaRuns[processCount], runtime)
        except KeyError:
            archieExactCornerExpandingAreaRuns[processCount] = runtime
    else:
        print(path + " is unexpected.")

if originalRuntime != 0.0:
    print("Original single threaded code, " + str(originalRuntime))

if originalIFBF0Runtime != 0.0:
    print("\nOriginal single threaded ifbf=0 code, " + str(originalIFBF0Runtime))

print("\nSharedMemory Runs")
for key in archieRuns.keys():
    print(str(key) + ", " + str(archieRuns[key]))

print("\nSharedMemoryExpandingArea Runs")
for key in archieExpandingAreaRuns.keys():
    print(str(key) + ", " + str(archieExpandingAreaRuns[key]))

print("\nSharedMemoryExactCorners Runs")
for key in archieExactCornerRuns.keys():
    print(str(key) + ", " + str(archieExactCornerRuns[key]))

print("\nSharedMemoryExactCornersExpandingArea Runs")
for key in archieExactCornerExpandingAreaRuns.keys():
    print(str(key) + ", " + str(archieExactCornerExpandingAreaRuns[key]))
