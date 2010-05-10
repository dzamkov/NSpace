from Lexer import *
from Statement import *

"""
Interprets an string showing an nspace script. Scope is the mutable scope
to start the script with.
"""
def Interpret(Script, Scope):

    # Lexify the script
    l = Lexer()
    l.Write(Script)
    n = l.Finish()

    # Run
    RunStatement(n, Scope, [])
    return Scope

# Test script
try:
    n = Interpret("""
    x = false
    y = true
    if(y) {
        x = true
        y = false
    }

    if(y) {
        x = false
    } else {
        if(x) {
            y = true
        }
    }""", Scope())
# Expect x = true, y = true
except LexerException as lx:
    print(lx.Message)
    print(lx.String)
