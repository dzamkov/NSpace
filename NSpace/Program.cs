//----------------------------------------
// Copyright (c) 2010, Dmitry Zamkov 
// Open source under the new BSD License
//----------------------------------------
using System;
using System.Collections.Generic;

namespace NSpace
{
	public static class Program
	{
        /// <summary>
        /// Main entry point for the application. 
        /// </summary>
        public static void Main()
		{
            _ConvertMethod = NSpace.Convert.Load(System.Reflection.Assembly.GetExecutingAssembly());

			Window win = new Window();
			win.Run(60.0);
		}

        /// <summary>
        /// Converts an object of the specified source type to an object of the destination type.
        /// </summary>
        public static void Convert<S, D>(S Object, out D Result)
            where S : class, IImmutable
            where D : class, IImmutable
        {
            IConvertMethod<D> specialconvertmethod; _ConvertMethod.Specialize<D>(out specialconvertmethod);
            Result = specialconvertmethod.Convert<S>(Object);
        }

        /// <summary>
        /// Convert method to use for conversions.
        /// </summary>
        private static IConvertMethod<IImmutable> _ConvertMethod;
	}
}
