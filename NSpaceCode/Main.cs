//----------------------------------------
// Copyright (c) 2010, Dmitry Zamkov 
// Open source under the new BSD License
//----------------------------------------

namespace NSpaceCode
{
    public static class Program
    {
        /// <summary>
        /// Main entry point.
        /// </summary>
        public static void Main()
        {
            // forall a, b {(a + b = c) = (a = c - b)} and x + 7 = 3 + 9
            Variable a = new Variable("a");
            Variable b = new Variable("b");
            Variable c = new Variable("c");
            Variable eq = new Variable("=");
            Variable add = new Variable("+");
            Variable sub = new Variable("-");
            Variable and = new Variable("and");
            Variable x = new Variable("x");
            Variable v7 = new Variable("7");
            Variable v3 = new Variable("3");
            Variable v9 = new Variable("9");
            Application a1 = new Application(new Application(add, a), b);
            Application a2 = new Application(new Application(eq, a1), c);
            Application a3 = new Application(new Application(sub, c), b);
            Application a4 = new Application(new Application(eq, a), a3);
            Application a5 = new Application(new Application(eq, a2), a4);
            ForAll rule = new ForAll(new Variable[] { a, b, c }, a5);
            Application a6 = new Application(new Application(add, x), v7);
            Application a7 = new Application(new Application(add, v3), v9);
            Application a8 = new Application(new Application(eq, a6), a7);
            Application prg = new Application(new Application(and, rule), a8);
        }
    }
}