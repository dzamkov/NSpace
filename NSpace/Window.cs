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
using NSpace.Visual;

namespace NSpace
{
	public class Window : GameWindow
	{
		public Window() : base(640, 480, GraphicsMode.Default, "NSpace")
		{
			this.WindowBorder = WindowBorder.Fixed;

            // Create a simple scene
            Section r = new Section();
            Section a = r.CreateChild(new Section.Relation(Matrix.Translate(new Vector(-3.0, 0.0, 0.0))));
            Section b = r.CreateChild(new Section.Relation(Matrix.Translate(new Vector(1.0, 0.0, 0.0))));
            this._Scene = new Scene(new Cube(b), a);
		}

        protected override void OnRenderFrame(FrameEventArgs e)
		{
            GL.ClearColor(0.0f, 0.0f, 0.0f, 1.0f);

            this._Scene.Render(new Time(0.0));

            this.SwapBuffers();
		}
		
		protected override void OnUpdateFrame (FrameEventArgs e)
		{
            
		}

        private Scene _Scene;
	}
}
