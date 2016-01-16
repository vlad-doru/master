def read_int(var_name):
    print("Introduceti variabila {0}:".format(var_name))
    line = raw_input()
    # We allow comments.
    value = int(line.split('#')[0])
    print("Variabila {0} = {1}".format(var_name, value))
    return value
