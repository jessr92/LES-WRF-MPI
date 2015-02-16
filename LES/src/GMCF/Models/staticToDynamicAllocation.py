import re

# Limitations:
# 1. Assumes real(kind=4)

class ArrayInfo:
    def __init__(self, name):
        self.name = name
        self.dimensionBounds = []

arrays = []

main_file = open("main.f95", "r")
for line in main_file:
    if "dimension(" in line:
        array_name = line.split("::")[1].strip()
        array_info = ArrayInfo(array_name)
        dimension_info = re.search('dimension\((.+?)\)', line).group(1).split(",")
        for dimension in dimension_info:
            dimension_split = dimension.split(":")
            if len(dimension_split) == 1:
                lowerBound = "1"
                upperBound = dimension_split[0]
            else:
                lowerBound = dimension_split[0]
                upperBound = dimension_split[1]
            array_info.dimensionBounds.append((lowerBound, upperBound))
        dimension_count = len(dimension_info)
        array_info.dimension_count = dimension_count
        arrays.append(array_info)
main_file.close()

for array in arrays:
    array_definition = "real(kind=4), dimension(:"
    for i in range(1, array.dimension_count):
        array_definition += ",:"
    array_definition += ("), allocatable :: " + array.name)
    print(array_definition)

for array in arrays:
    array_allocation = "allocate(" + array.name + "("
    for i in range(0, array.dimension_count):
        array_allocation += "(" + str(array.dimensionBounds[i][1]) + ")-(" + str(array.dimensionBounds[i][0]) + ")+1"
        if i != array.dimension_count - 1:
            array_allocation += ","
    array_allocation += "))"
    print(array_allocation)
    print("call zero" + str(array.dimension_count) + "DReal4Array(" + array.name + ")")

for array in arrays:
    print("deallocate(" + array.name + ")")