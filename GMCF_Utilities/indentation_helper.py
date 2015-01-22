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

