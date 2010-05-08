NormalAssignmentChar = "="
DefaultValueAssignmentChar = "~"
StringLiteralDelimiter = set(["\"", "'"])
EscapeChar = "\\"
EscapedCharTranslation = { "n" : "\n", '"' : '"', '\\' : '\\'  }
WhiteSpaceChars = set([" ", "\n", "\t"])
IsWordChar = lambda C: type(C).__name__ == "str" and C.isalpha() or C == "_"

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
A node that represents a string literal.
"""
class StringLiteralNode(Node):

    """
    The value of the string literal.
    """
    Value = ""

    """
    The character used to delimit the string.
    """
    Delimiter = None

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
