//----------------------------------------
// Copyright (c) 2010, Dmitry Zamkov 
// Open source under the new BSD License
//----------------------------------------
using System;
using System.Collections.Generic;
using System.Drawing;
using System.Windows.Forms;
using OpenTK;
using OpenTK.Graphics;
using OpenTK.Input;
using OpenTK.Graphics.OpenGL;
using NSpace.Physics;

namespace NSpace
{
	public class Window : GameWindow
	{
		public Window() : base(640, 480, GraphicsMode.Default, "NSpace")
		{
			this.WindowBorder = WindowBorder.Fixed;
		}

        protected override void OnRenderFrame(FrameEventArgs e)
		{
           
		}
		
		protected override void OnUpdateFrame (FrameEventArgs e)
		{
            
		}
	}
}
