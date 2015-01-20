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


def output_system_config(model_count):
    indented_output("System:")
    indentation_increase()
    indented_output("Libraries: [GMCF, CoreServices]")
    indented_output("NServiceNodes: " + str(model_count + 1))
    output_service_nodes(model_count)
    indented_output("")
    output_aliases_nodes(model_count)
    indented_output("")
    indented_output("ModelLibPaths: ['libles.a']")
    indented_output("")
    output_models(model_count)
    indentation_decrease()


def output_service_nodes(model_count):
    indented_output("ServiceNodes:")
    indentation_increase()
    for i in range(1, model_count + 1):
        indented_output("mn" + str(i) + ": [ " + str(i) + " , [GMCF.GMCF] ]")
    indented_output("ctrl: [ " + str(model_count + 1) + ", [CoreServices.BEGIN] ]")
    indentation_decrease()


def output_aliases_nodes(model_count):
    indented_output("Aliases")
    indentation_increase()
    indented_output("begin: ctrl.CoreServices.BEGIN.begin")
    for i in range(1, model_count + 1):
        indented_output("model" + str(i) + ": mn" + str(i) + ".GMCF.GMCF.run_model" + str(i))
    indentation_decrease()


def output_models(model_count):
    indented_output("Models:")
    for i in range(1, model_count + 1):
        indentation_increase()
        output_model(i)
        indentation_decrease()


def output_model(i):
    indented_output("model" + str(i))
    indentation_increase()
    indented_output("ModelId: " + str(i))
    indentation_decrease()

if len(sys.argv) != 2:
    print("We need one argument, the number of LES models required.")

lesCountRequired = int(sys.argv[1])

output_system_config(lesCountRequired)
