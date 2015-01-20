import sys

INDENTATION_STEPPING = 4
indentationLevel = 0


def indented_output(message):
    print((" " * indentationLevel) + message)


def indentation_increase():
    global indentationLevel
    indentationLevel += INDENTATION_STEPPING


def indentation_decrease():
    global indentationLevel
    indentationLevel -= INDENTATION_STEPPING
    if indentationLevel < 0:
        indentationLevel = 0

if len(sys.argv) != 3:
    print("We need two arguments, the number of model instances required then the model instance name.")

modelCountRequired = int(sys.argv[1])
modelName = sys.argv[2]

indented_output("; GMCF.yml")
indented_output("")
indented_output("(begin")
indentation_increase()
for i in range(1, modelCountRequired + 1):
    indented_output("(" + modelName + " '" + str(i) + ")")
indentation_decrease()
indented_output(")")
indented_output("")
