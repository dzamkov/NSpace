#ifndef _NSPACE_LEXER_
#define _NSPACE_LEXER_

#include <vector>
#include <string>

/// A node created by the lexer the represents some structure in 
/// a script or program
struct Node {
public:

	/// An integer representing the type of the node
	unsigned int Type;

	/// The character this node starts on in the lexer stream
	int CharStart;

	/// The amount of characters in the node
	int CharSize;

};

/// A node that represents a statement, not including the leading
/// or trailing characters that delimit it
struct StatementNode : public Node {
public:
	static const unsigned int Type = 0x00000001;



};

/// A node that represents a set of statements executed sequentially
struct BlockNode : public Node {
public:
	static const unsigned int Type = 0x00000002;

	/// The statements this block contains
	std::vector<StatementNode*>		Statements;
};

/// A node that represents some kind of value
struct ExpressionNode : public Node {
	static const unsigned int Type = 0x00000003;

};

/// A node that represents a program variable
struct VariableNode : public ExpressionNode {
	static const unsigned int Type = 0x00000004;

	/// The name of the variable associated with the node
	std::string		Name;
};

/// A node that represents a program type
struct TypeNode : public Node {
	static const unsigned int Type = 0x00000005;

};

/// A node that represents a definition statement. One in the form
/// of "type variable = value"
struct DefinitionNode : public StatementNode {
	static const unsigned int Type = 0x00000006;

	/// The type of the variable set
	TypeNode*			VariableType;

	/// The variable that is defined
	VariableNode*		Variable;

	/// The value the variable is set to
	ExpressionNode*		Value;
};

/// Converts a stream of characters into a node-like format that
/// represents the script structure
class Lexer {
public:
	Lexer();
	~Lexer();

	/// Inputs a string of input characters into the lexer
	void Input(char* Buffer, int Length);

	/// Finishes inputting into the lexer, creating the output node.
	Node* Finish();

};

#endif