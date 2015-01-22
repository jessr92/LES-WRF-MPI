from indentation_helper import *
import sys


def is_top_row(processes_per_row, rank):
    return rank < processes_per_row


def is_bottom_row(processes_per_row, rank):
    return rank >= (processes - processes_per_row)


def is_leftmost_column(processes_per_row, rank):
    return rank % processes_per_row == 0


def is_rightmost_column(processes_per_row, rank):
    return rank % processes_per_row == processesPerRow - 1


def generate_matrix():
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


def output_matrix():
    indented_output("module gmcfConfiguration")
    indentation_increase()
    indented_output("implicit none")
    indented_output("integer, dimension(NMODELS, NMODELS) :: gmcfConnectivityMatrix = reshape( / &")
    output_matrix_string()
    indented_output(" /), shape(gmcfConnectivityMatrix) )")
    indentation_decrease()
    indented_output("end module gmcfConfiguration")
    indented_output("")


def output_matrix_string():
    indentation_increase()
    matrix_string = ""
    for i in range(0, processes):
        for j in range(0, processes):
            matrix_string += str(matrix[i][j])
            if i != processes - 1 or j != processes - 1:
                matrix_string += ", "
            else:
                matrix_string += " "
        matrix_string += "&"
        indented_output(matrix_string)
        matrix_string = ""
    indentation_decrease()

if len(sys.argv) != 3:
    print("Please enter two numbers in command line for the number of processes per row and per column")
    return

processesPerRow = int(sys.argv[1])
processesPerCol = int(sys.argv[2])
processes = processesPerRow * processesPerCol

matrix = [[0 for x in range(processes)] for y in range(processes)]

generate_matrix()

output_matrix()
