from Scope import *
from Expression import *
from Lexer import *

"""
Runs a statement, in the specified scope. This will
return the return value of the statement if it has one. FillValues is a list
of arguments given. When a declaration statement is given, a fill value is
popped and assigned to it.
"""
def RunStatement(StatementNode, Scope, FillValues):
    if StatementNode != None:
        if isinstance(StatementNode, BlockNode):
            return RunBlockStatement(StatementNode, Scope, FillValues)
        if isinstance(StatementNode, AssignmentStatementNode):
            return RunAssignmentStatement(StatementNode, Scope, FillValues)
        if isinstance(StatementNode, IfStatementNode):
            return RunIfStatement(StatementNode, Scope, FillValues)
    return None

"""
Runs a block statement, which is an ordered list of other statements.
"""
def RunBlockStatement(StatementNode, Scope, FillValues):
    for s in StatementNode.Statements:
        ret = RunStatement(s, Scope, FillValues)
        if ret != None:
            return ret
    pass

"""
Runs an assignment statement, which simply assigns a value to a variable.
"""
def RunAssignmentStatement(StatementNode, Scope, FillValues):
    Scope.Assign(
        StatementNode.Variable.Value,
        Evaluate(StatementNode.Value, Scope))
    pass


"""
Runs an if statement, 
"""
def RunIfStatement(StatementNode, Scope, FillValues):
    if Scope.Convert(Evaluate(StatementNode.Condition, Scope), bool):
        return RunStatement(StatementNode.TrueStatement, Scope, FillValues)
    else:
        return RunStatement(StatementNode.FalseStatement, Scope, FillValues)
