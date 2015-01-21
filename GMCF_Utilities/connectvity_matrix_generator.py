import sys


def is_top_row(processes_per_row, rank):
    return rank < processes_per_row


def is_bottom_row(processes_per_row, rank):
    return rank >= (processes - processes_per_row)


def is_leftmost_column(processes_per_row, rank):
    return rank % processes_per_row == 0


def is_rightmost_column(processes_per_row, rank):
    return rank % processes_per_row == processesPerRow - 1

if len(sys.argv) != 3:
    print("Please enter two numbers in command line, procPerRow and procPerCol")

processesPerRow = int(sys.argv[1])
processesPerCol = int(sys.argv[2])
processes = processesPerRow * processesPerCol

matrix = [[0 for x in range(processes)] for y in range(processes)]

print("Generating connectivity matrix for " + str(processesPerRow) + " processes per row and " +
      str(processesPerCol) + " processes per column")

for i in range(0,processes):
    if not is_top_row(processesPerRow, i):
        # i-processesPerRow is the top neighbour
        # print(str(i) + " has top neighbour " + str(i-processesPerRow))
        matrix[i][i-processesPerRow] = 1
    if not is_bottom_row(processesPerRow, i):
        # i+processesPerRow is the bottom neighbour
        # print(str(i) + " has bottom neighbour " + str(i+processesPerRow))
        matrix[i][i+processesPerRow] = 1
    if not is_leftmost_column(processesPerRow, i):
        # i-1 is the left neighbour
        # print(str(i) + " has left neighbour " + str(i-1))
        matrix[i][i-1] = 1
    if not is_rightmost_column(processesPerRow, i):
        # i+1 is the right neighbour
        # print(str(i) + " has right neighbour " + str(i+1))
        matrix[i][i+1] = 1

for i in range(0, processes):
    print(matrix[i])
