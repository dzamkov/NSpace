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
    public abstract class Expression
    {
        /// <summary>
        /// Matches a pattern to this expression. The specified pattern can contain generalized variables
        /// that are specified in the values dictionary. If the pattern is matched, this function returns true
        /// and the values in the dictionary are set to the values that are expressed in this expression.
        /// </summary>
        public abstract bool Match(Expression Pattern, ref Dictionary<Variable, Expression> Values);

        /// <summary>
        /// Replaces a variable in the expression and returns the result.
        /// </summary>
        public abstract Expression Replace(Variable Var, Variable With);
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

        public override bool Match(Expression Pattern, ref Dictionary<Variable, Expression> Values)
        {
            Variable v = Pattern as Variable;
            if (v != null)
            {
                if (Values.ContainsKey(v))
                {
                    Values[v] = this;
                    return true;
                }
                else
                {
                    return v == this;
                }
            }
            else
            {
                return false;
            }
        }

        public override Expression Replace(Variable Var, Variable With)
        {
            if (this == Var)
            {
                return With;
            }
            else
            {
                return this;
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

        public override bool Match(Expression Pattern, ref Dictionary<Variable, Expression> Values)
        {
            Application a = Pattern as Application;
            if (a != null)
            {
                return this._Argument.Match(a._Argument, ref Values) && this._Function.Match(a._Function, ref Values);
            }
            else
            {
                Variable v = Pattern as Variable;
                if (v != null)
                {
                    if (Values.ContainsKey(v))
                    {
                        Values[v] = this;
                        return true;
                    }
                }
                return false;
            }
        }

        public override Expression Replace(Variable Var, Variable With)
        {
            return new Application(this._Function.Replace(Var, With), this._Argument.Replace(Var, With));
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

        public override bool Match(Expression Pattern, ref Dictionary<Variable, Expression> Values)
        {
            ForAll f = Pattern as ForAll;
            if (f != null)
            {

            }
        }

        public override Expression Replace(Variable Var, Variable With)
        {
            bool ismainvar = false;
            foreach (Variable v in this._Vars)
            {
                if (v == Var)
                {
                    ismainvar = true;
                    break;
                }
            }

            if (!ismainvar)
            {
                return new ForAll(this._Vars, this._Exp.Replace(Var, With));
            }
            return this;
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