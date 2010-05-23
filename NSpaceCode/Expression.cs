//----------------------------------------
// Copyright (c) 2010, Dmitry Zamkov 
// Open source under the new BSD License
//----------------------------------------
using System.Collections.Generic;

namespace NSpaceCode
{
    /// <summary>
    /// Represents a value.
    /// </summary>
    public class Expression
    {

    }

    /// <summary>
    /// A constant value used as a variable. Every instance of
    /// variable represents a unique value.
    /// </summary>
    public class Variable : Expression
    {
        public Variable(string Name)
        {
            this._Name = Name;
        }

        public Variable()
        {
            this._Name = null;
        }

        /// <summary>
        /// Gets an optional name for this variable for debugging purposes.
        /// </summary>
        public string Name
        {
            get
            {
                return this._Name;
            }
        }

        public override string ToString()
        {
            if (this._Name != null)
            {
                return this._Name;
            }
            else
            {
                return "<unnamed var>";
            }
        }

        private string _Name;
    }

    /// <summary>
    /// Represents the function application of one expression
    /// on another. (Function Argument)
    /// </summary>
    public class Application : Expression
    {
        public Application(Expression Function, Expression Argument)
        {
            this._Function = Function;
            this._Argument = Argument;
        }

        /// <summary>
        /// Gets the function part of the application.
        /// </summary>
        public Expression Function
        {
            get
            {
                return this._Function;
            }
        }

        /// <summary>
        /// Gets the argument part of the application.
        /// </summary>
        public Expression Argument
        {
            get
            {
                return this._Argument;
            }
        }

        public override string ToString()
        {
            Application arg = this._Argument as Application;
            if (arg == null)
            {
                return this._Function.ToString() + " " + this._Argument.ToString();
            }
            else
            {
                return this._Function.ToString() + "(" + this._Argument.ToString() + ")";
            }
        }

        private Expression _Function;
        private Expression _Argument;
    }

    /// <summary>
    /// A forall expression, one that is true if its inner expression is true for all values
    /// of the specified variables.
    /// </summary>
    public class ForAll : Expression
    {
        public ForAll(IEnumerable<Variable> Variables, Expression Expression)
        {
            this._Vars = Variables;
            this._Exp = Expression;
        }

        /// <summary>
        /// Gets the main variables of the forall expression.
        /// </summary>
        public IEnumerable<Variable> Variables
        {
            get
            {
                return this._Vars;
            }
        }

        /// <summary>
        /// Gets the inner(nested) expression for the forall expression.
        /// </summary>
        public Expression Expression
        {
            get
            {
                return this._Exp;
            }
        }

        public override string ToString()
        {
            string s = "forall ";
            foreach (Variable v in this._Vars)
            {
                s += v.ToString() + " ";
            }
            s += "{ " + this._Exp.ToString() + " }";
            return s;
        }

        private IEnumerable<Variable> _Vars;
        private Expression _Exp;
    }
}