from Lexer import *;

"""
Interprets an string showing an nspace script. Variables is the set of variables
to start the script with. It must be a mutable map and will change if the script
adds or removes variables.
"""
def Interpret(Script, Variables):

    # Lexify the script
    l = Lexer()
    l.Write(Script)
    n = l.Finish()

    # Add some variables
    Variables["string"] = str
    Variables["bool"] = bool
    Variables["true"] = True
    Variables["false"] = False

    # Run
    RunStatement(n, Variables, [])
    return Variables

"""
Error given when the variable in a declaration statement can not be assigned
because of insufficent arguments.
"""
class UnAssignedArgumentException(Exception):
    pass

"""
Parses a type node and returns an object that represents its type.
"""
def ParseType(Type, Variables):

    pass

"""
Evaluates an expression node and returns its value.
"""
def EvaluateExpression(ExpressionNode, Variables):
    if isinstance(ExpressionNode, WordExpressionNode):
        try:
            return Variables[ExpressionNode.Value]
        except KeyError:
            return None
    if isinstance(ExpressionNode, StringLiteralNode):
        return ExpressionNode.Value

"""
Converts a script value to the specified type.
"""
def Convert(Value, Type):
    if isinstance(Value, Type):
        return Value
    else:
        return None

"""
Runs a statement, using the particular variable name to values map. This will
return the return value of the statement if it has one. FillValues is a list
of arguments given. When a declaration statement is given, a fill value is
popped and assigned to it.
"""
def RunStatement(StatementNode, Variables, FillValues):
    if isinstance(StatementNode, BlockNode):
        for s in StatementNode.Statements:
            ret = RunStatement(s, Variables, FillValues)
            if ret != None:
                return ret
    if isinstance(StatementNode, DeclarationStatementNode):
        obj = Convert(FillValues.pop(), EvaluateExpression(StatementNode.Type, Variables))
        Variables[StatementNode.Variable.Value] = obj
    if isinstance(StatementNode, DefinitionStatementNode):
        val = EvaluateExpression(StatementNode.Value, Variables)
        typ = EvaluateExpression(StatementNode.Type, Variables)
        Variables[StatementNode.Variable.Value] = Convert(val, typ)
    if isinstance(StatementNode, AssignmentStatementNode):
        val = EvaluateExpression(StatementNode.Value, Variables)
        Variables[StatementNode.Variable.Value] = val
    if isinstance(StatementNode, IfStatementNode):
        if Convert(EvaluateExpression(StatementNode.Condition, Variables), bool):
            return RunStatement(StatementNode.TrueStatement, Variables, FillValues)
        else:
            return RunStatement(StatementNode.FalseStatement, Variables, FillValues)
    pass

# Test script
n = Interpret("""
bool x = false;
if(x) {
	string meh = "MEH";
} else {
	string unmeh = "UNMEH";
};
""", dict())
