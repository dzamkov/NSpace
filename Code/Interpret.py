from Lexer import *;

"""
Interprets an string showing an nspace script.
"""
def Interpret(Script):

    # Lexify the script
    l = Lexer()
    l.Write(Script)
    return l.Finish()
    


# Test script
n = Interpret("""
string main = "hello world";
""")
