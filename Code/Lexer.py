AssignmentChars = set(["=", "~"])
StringLiteralDelimiter = set(["\"", "'"])
EscapeChar = "\\"
EscapedCharTranslation = { "n" : "\n", '"' : '"', '\\' : '\\'  }
WhiteSpaceChars = set([" ", "\n", "\t"])
IsWordChar = lambda C: type(C).__name__ == "str" and C.isalpha() or C == "_"
KeyWords = set(["function", "new"])
StatementEnd = ";"

"""
Error given when the lexer receieves bad input (syntax error is script)
"""
class LexerError:

    """
    A message that describes what the bad input is.
    """
    Message = None

"""
Takes input characters defining a script and outputs a node-like representation
of the script.
"""
class Lexer:

    """
    Represents a string of characters and nodes that represent the working set
    that is yet to be classified as nodes.
    """
    __CurrentString = None

    def __init__(self):
        self.__CurrentString = []

    """
    Writes a set of characters to the lexer for processing, the set does not
    need to finish an particular structure such as a statement or variable, it
    can have any amount of chars as long as they are in order.
    """
    def Write(self, Chars):
        for c in Chars:
            self.__CurrentString.append(c)

    """
    Processes the current string, creating larger node representations of the
    characters in it.
    """
    def __Process(self):
        self.__ProcessStringLiterals()
        self.__ProcessWhiteSpace()
        self.__ProcessWords()
        self.__ProcessKeyWords()
        self.__ProcessWordExpressions()
        self.__ProcessAssignments()
        self.__ProcessStatements()
        pass

    """
    Finds and processes string literals in the current string.
    """
    def __ProcessStringLiterals(self):
        curnode = None
        escaped = False
        newstring = []
        for c in self.__CurrentString:
            if curnode == None:
                if c in StringLiteralDelimiter:
                    curnode = StringLiteralNode()
                    curnode.Delimiter = c
                else:
                    newstring.append(c)
            else:
                if escaped:
                    curnode.Value = curnode.Value + EscapedCharTranslation[c]
                    escaped = False
                else:
                    if c == EscapeChar:
                        escaped = True
                    else:
                        if c == curnode.Delimiter:
                            newstring.append(curnode)
                            curnode = None
                        else:
                            curnode.Value = curnode.Value + c
        self.__CurrentString = newstring
        pass

    """
    Finds and processes whitespace into the current string.
    """
    def __ProcessWhiteSpace(self):
        curnode = None
        newstring = []
        for c in self.__CurrentString:
            if c in WhiteSpaceChars:
                if curnode == None:
                    curnode = WhiteSpaceNode()
            else:
                if curnode != None:
                    newstring.append(curnode)
                    curnode = None
                newstring.append(c)
        if curnode != None:
            newstring.append(curnode)
        self.__CurrentString = newstring
        pass

    """
    Finds and processes words.
    """
    def __ProcessWords(self):
        curnode = None
        newstring = []
        for c in self.__CurrentString:
            if curnode:
                if IsWordChar(c):
                    curnode.Value = curnode.Value + c
                else:
                    newstring.append(curnode)
                    curnode = None
                    newstring.append(c)
            else:
                if IsWordChar(c):
                    curnode = WordNode()
                    curnode.Value = c
                else:
                    newstring.append(c)
        self.__CurrentString = newstring
        pass

    """
    Finds and processes keywords
    """
    def __ProcessKeyWords(self):
        newstring = []
        for c in self.__CurrentString:
            if isinstance(c, WordNode):
                if c.Value in KeyWords:
                    kwn = KeyWordNode()
                    kwn.Value = c.Value
                    newstring.append(kwn)
                    continue
            newstring.append(c)
        self.__CurrentString = newstring
        pass

    """
    Finds and processes variables
    """
    def __ProcessWordExpressions(self):
        newstring = []
        for c in self.__CurrentString:
            if isinstance(c, WordNode):
                wen = WordExpressionNode()
                wen.Value = c.Value
                newstring.append(wen)
            else:
                newstring.append(c)
        self.__CurrentString = newstring
        pass

    """
    Finds and processes assignment characters.
    """
    def __ProcessAssignments(self):
        self.__Replace([
            lambda x: isinstance(x, WhiteSpaceNode),
            lambda x: x in AssignmentChars,
            lambda x: isinstance(x, WhiteSpaceNode)],
            lambda y: [AssignmentNode(y[1])])
        self.__Replace([
            lambda x: isinstance(x, ExpressionNode),
            lambda x: x in AssignmentChars,
            lambda x: isinstance(x, WhiteSpaceNode)],
            lambda y: [y[0], AssignmentNode(y[1])])
        self.__Replace([
            lambda x: isinstance(x, WhiteSpaceNode),
            lambda x: x in AssignmentChars,
            lambda x: isinstance(x, ExpressionNode)],
            lambda y: [AssignmentNode(y[1]), y[2]])
        self.__Replace([
            lambda x: isinstance(x, ExpressionNode),
            lambda x: x in AssignmentChars,
            lambda x: isinstance(x, ExpressionNode)],
            lambda y: [y[0], AssignmentNode(y[1]), y[2]])
        pass

    """
    Finds and processes statements.
    """
    def __ProcessStatements(self):

        # Replace statement ends
        self.__Replace([
            lambda x: isinstance(x, WhiteSpaceNode),
            lambda x: x == StatementEnd],
            lambda y: [StatementEndNode()])
        self.__Replace([
            lambda x: x == StatementEnd],
            lambda y: [StatementEndNode()])

        # Replace statements
        self.__Replace([
            lambda x: isinstance(x, ExpressionNode),
            lambda x: isinstance(x, WhiteSpaceNode),
            lambda x: isinstance(x, WordNode),
            lambda x: isinstance(x, StatementEndNode)],
            lambda y: [DeclarationStatementNode(y[0], y[2])])
        self.__Replace([
            lambda x: isinstance(x, ExpressionNode),
            lambda x: isinstance(x, WhiteSpaceNode),
            lambda x: isinstance(x, WordNode),
            lambda x: isinstance(x, AssignmentNode),
            lambda x: isinstance(x, ExpressionNode),
            lambda x: isinstance(x, StatementEndNode)],
            lambda y: [DefinitionStatementNode(y[0], y[2], y[3], y[4])])
        self.__Replace([
            lambda x: isinstance(x, WordNode),
            lambda x: isinstance(x, AssignmentNode) and x.AssignmentChar == "=",
            lambda x: isinstance(x, ExpressionNode),
            lambda x: isinstance(x, StatementEndNode)],
            lambda y: [AssignmentStatementNode(y[0], y[2])])
        pass

    """
    Finds and replaces a pattern with the specified rules found in the
    current string of the lexer. Rules are a list of functions that either
    return true if a character follows the rules or false if not. Result
    is a function that takes a list of nodes and returns a list indicating the
    result after applying those rules.
    """
    def __Replace(self, Rules, Result):
        newstring = []
        while len(self.__CurrentString) >= len(Rules):
            cur = 0
            match = True
            for rule in Rules:
                if not rule(self.__CurrentString[cur]):
                    match = False
                    break
                cur = cur + 1
            if match:
                newstring.extend(Result(self.__CurrentString[0:len(Rules)]))
                del self.__CurrentString[0:len(Rules)]
            else:
                newstring.append(self.__CurrentString.pop(0))
        while len(self.__CurrentString) > 0:
            newstring.append(self.__CurrentString.pop(0))
        self.__CurrentString = newstring
        pass

    """
    Finishes lexing, and returns the node representation of the script.
    """
    def Finish(self):
        self.__Process()
        return self.__CurrentString
        

"""
An object that represents a set of chars that represent a programming structure.
"""
class Node:

    pass

"""
A node that represents some amount of whitespace.
"""
class WhiteSpaceNode(Node):
    
    pass

"""
A node that represents a word which can be used as a variable or key
word. The word must be made completely out of word characters.
"""
class WordNode(Node):

    """
    The text for the word.
    """
    Value = ""

    pass

"""
A node that represents a keyword, one which can not be used as an expression.
"""
class KeyWordNode(Node):

    """
    The text of this keyword.
    """
    Value = ""

    pass

"""
A node that signals the end of a statement.
"""
class StatementEndNode(Node):

    pass


"""
A node that represents an expression
"""
class ExpressionNode(Node):

    pass

"""
A node that represents a string literal.
"""
class StringLiteralNode(ExpressionNode):

    """
    The value of the string literal.
    """
    Value = ""

    """
    The character used to delimit the string.
    """
    Delimiter = None

"""
An expression from a word. HINT: A VARIABLE
"""
class WordExpressionNode(ExpressionNode, WordNode):

    pass

"""
A node that represents an assignment operation.
"""
class AssignmentNode(Node):

    """
    The character used for the assignment, usually "=" or "~".
    """
    AssignmentChar = None

    def __init__(self, Char):
        self.AssignmentChar = Char

    pass

"""
A node that represents a statement.
"""
class StatementNode(Node):

    pass

"""
A node that represents a declaration statement, in the form "x y;"
"""
class DeclarationStatementNode(StatementNode):

    """
    The type used to declare with.
    """
    Type = None

    """
    The variable that is declared.
    """
    Variable = None

    def __init__(self, Type, Variable):
        self.Type = Type
        self.Variable = Variable

    pass

"""
A node that represents a definition statement, in the form "x y = z;"
"""
class DefinitionStatementNode(StatementNode):
    
    """
    The type used to define with.
    """
    Type = None

    """
    The variable that is defined.
    """
    Variable = None

    """
    The type of assignment used.
    """
    Assignment = None
    
    """
    The value the variable is set to.
    """
    Value = None

    def __init__(self, Type, Variable, Assignment, Value):
        self.Type = Type
        self.Variable = Variable
        self.Assignment = Assignment
        self.Value = Value

    pass

"""
A node that represents an assignment statement, in the form "x = y;"
"""
class AssignmentStatementNode(StatementNode):

    """
    The variable that is assigned.
    """
    Variable = None

    """
    The value the variable is set to.
    """
    Value = None

    def __init__(self, Variable, Value):
        self.Variable = Variable
        self.Value = Value

    pass
