from indentation_helper import *
import sys

if len(sys.argv) != 3:
    sys.exit("We need two arguments, the number of model instances required then the model instance name.")

modelCountRequired = int(sys.argv[1])
modelName = sys.argv[2]

indented_output("; " + modelName + ".yml")
indented_output("")
indented_output("(begin")
indentation_increase()
for i in range(1, modelCountRequired + 1):
    indented_output("(" + modelName + " '" + str(i) + ")")
indentation_decrease()
indented_output(")")
indented_output("")
