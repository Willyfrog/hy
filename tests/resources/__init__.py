

def kwtest(*args, **kwargs):
    return kwargs

def startest(*args, **kwargs):
    return list(args)

def both_args_test(*args, **kwargs):
    return (list(args), kwargs)

def function_with_a_dash():
    pass
