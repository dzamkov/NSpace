/**********************************************************
* Syntax rules for the NSpace code system
**********************************************************/

/*********** Declaring and defining variables
type variablename;			// Declares a variable (uninitialized)
variablename = value;		// Sets a variable
type variablename = value;  // Defines and initializes a variable
var variablename = value;	// Defines and initializes a variable with an implied type(using the var keyword)

/*********** Functions
returntype<argumenttype> variablename;							// Declares a one-parameter function
function(int a, int b) { return a + b; }						// Sum function(expression)
int<int, int> sum = function(int a, int b) { return a + b; };	// Named function
var sum = function(int a, int b) { return a + b; };				// Same as above
int res = sum(5, 6);											// Calling the sum function (makes res = 11)
int<> x = function() { return 4; }; int y = x + 2;				// Sets y to 6 using implicit function calls
int x = 4; int y = x() + 2;										// Implicit function creation (any variable is equal to a zero-parameter function that returns its type)

/*********** Types
type vector = type { double x; double y; double z; };		// Creates a vector type
vector point = new vector(4.0, 4.0, 5.0);					// Initializes a vector
vector point = new vector(x = 4.0, y = 4.0, z = 5.0);		// Same as above
vector point = new vector { x = 4.0; y = 4.0; z = 5.0; }	// Same as above
vector point = new vector { x = 4.0; y = x; z = y + 1.0; }	// Same as above
double x = point.x;											// Gets the x value from point (4.0)
vector = new vector type { double length = x + y + z; }		// Extending vector class to also have a length field.