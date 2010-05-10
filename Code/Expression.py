from Lexer import *
from Scope import *

"""
Evaluates an expression node.
"""
def Evaluate(ExpressionNode, Scope):
    if isinstance(ExpressionNode, WordExpressionNode):
        return Scope.Value(ExpressionNode.Value)
    if isinstance(ExpressionNode, StringLiteralNode):
        return ExpressionNode.Value
