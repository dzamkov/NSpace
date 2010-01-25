//----------------------------------------
// Copyright (c) 2010, Dmitry Zamkov 
// Open source under the new BSD License
//----------------------------------------
using System;
using OpenTK;

namespace NSpace
{
	public static class Program
	{
		/// <summary>
		/// Main entry point for the application. 
		/// </summary>
		public static void Main()
		{
			Window win = new Window();
			win.Run(60.0);
		}
	}
}
