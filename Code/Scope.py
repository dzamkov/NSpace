"""
A set of variable in a scope.
"""
class Scope:

    """
    Variables in the set.
    """
    __Vars = None

    def __init__(self):
        self.__Vars = dict()
        self.SetSpecialVariables()

    """
    Adds the special variables into this scope.
    """
    def SetSpecialVariables(self):
        self.__Vars["string"] = str
        self.__Vars["bool"] = bool
        self.__Vars["true"] = True
        self.__Vars["false"] = False
        self.__Vars["null"] = None

    """
    Converts a script value to the specified type.
    """
    def Convert(self, Value, Type):
        if isinstance(Value, Type):
            return Value
        else:
            return None
        
    """
    Assigns a variable with a value.
    """
    def Assign(self, Variable, Value):
        self.__Vars[Variable] = Value

    """
    Gets the value of the specified variable.
    """
    def Value(self, Variable):
        try:
            return self.__Vars[Variable]
        except KeyError:
            return None
        
