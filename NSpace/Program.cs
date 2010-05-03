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
        [System.Runtime.InteropServices.DllImport("KERNEL32")]
        private static extern bool QueryPerformanceCounter(
            ref long lpPerformanceCount);

        [System.Runtime.InteropServices.DllImport("KERNEL32")]
        private static extern bool QueryPerformanceFrequency(
            ref long lpFrequency);

        static Program()
        {
            _PerformanceOffset = _TestPerformance(
                delegate()
                {

                }, 100000);
        }

        /// <summary>
        /// Tests the performance of calling a method and returns the average time
        /// it takes in nanoseconds.
        /// </summary>
        public static double TestPerformance(Action Method, int Iterations)
        {
            return _TestPerformance(Method, Iterations) - _PerformanceOffset;
        }

        /// <summary>
        /// Same as test performance but with no accounting for loops and delegates calls.
        /// </summary>
        private static double _TestPerformance(Action Method, int Iterations)
        {
            long starttime = 0;
            long endtime = 0;
            long frequency = 0;
            QueryPerformanceFrequency(ref frequency);
            QueryPerformanceCounter(ref starttime);
            for (int t = 0; t < Iterations; t++)
            {
                Method();
            }
            QueryPerformanceCounter(ref endtime);
            double diff = (double)endtime - (double)starttime;
            diff /= (double)frequency;
            diff /= (double)Iterations;
            diff *= 1000.0 * 1000.0 * 1000.0;
            return diff;
        }

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

        private static double _PerformanceOffset;
	}
}
