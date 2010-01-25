//----------------------------------------
// Copyright (c) 2010, Dmitry Zamkov 
// Open source under the new BSD License
//----------------------------------------
using System;
using OpenTK;
using OpenTK.Graphics;
using OpenTK.Graphics.OpenGL;

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
			// Get variables for view
			double aspect = (double)this.Width / (double)this.Height;
			double fov = Math.Atan(0.5);
			Vector3d eye = new Vector3d(0.0, 10.0, -5.0);
			Vector3d target = new Vector3d(0.0, 0.0, 0.0);
			Vector3d up = new Vector3d(0.0, 1.0, 0.0);
			Matrix4d proj = Matrix4d.CreatePerspectiveFieldOfView(fov, aspect, 0.01, 100.0);
			Matrix4d view = Matrix4d.LookAt(eye, target, up);
			
			// Clear current viewport
			GL.Clear(ClearBufferMask.DepthBufferBit | ClearBufferMask.ColorBufferBit);
			
			// Set projection and model matrices
			GL.MatrixMode(MatrixMode.Projection);
			GL.LoadMatrix(ref proj);
			GL.MatrixMode(MatrixMode.Modelview);
			GL.LoadMatrix(ref view);
			
			// Draw a triangle with colored corner with rotation
            GL.PushMatrix();
            GL.Rotate(this._Rot, 0.0, 1.0, 0.0);
			GL.Begin(BeginMode.Quads);
			GL.Color4(1.0, 0.0, 0.0, 1.0);
			GL.Vertex3(1.0, 0.0, 1.0);
            GL.Color4(0.0, 1.0, 0.0, 1.0);
            GL.Vertex3(1.0, 0.0, -1.0);
            GL.Color4(0.0, 0.0, 1.0, 1.0);
            GL.Vertex3(-1.0, 0.0, -1.0);
            GL.Color4(0.0, 0.0, 1.0, 1.0);
            GL.Vertex3(-1.0, 0.0, 1.0);
			GL.End();
            GL.PopMatrix();
			
			this.SwapBuffers();
		}
		
		protected override void OnUpdateFrame (FrameEventArgs e)
		{
            this._Rot += 1;
		}

        double _Rot = 0.0;
	}
}
