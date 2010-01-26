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

            // Create a cone
            this._Mesh = new Mesh();

            int points = 5;
            Point top = this._Mesh.CreatePoint(0.0, 3.0, 0.0);
            Point[] around = new Point[points];
            for (int t = 0; t < points; t++)
            {
                double ang = (double)t / (double)points * Math.PI * 2.0;
                around[t] = this._Mesh.CreatePoint(Math.Sin(ang), 0.0, Math.Cos(ang));
            }
            for (int t = 0; t < points; t++)
            {
                this._Mesh.CreateTriangle(top, around[(t + points - 1) % points], around[t]);
            }
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
            GL.CullFace(CullFaceMode.Front);
			
			// Set projection and model matrices
			GL.MatrixMode(MatrixMode.Projection);
			GL.LoadMatrix(ref proj);
			GL.MatrixMode(MatrixMode.Modelview);
			GL.LoadMatrix(ref view);
			
			// Draw a triangle with colored corner with rotation
            GL.PushMatrix();
            GL.Rotate(this._Rot, 0.0, 1.0, 0.0);
            this._DrawMesh(this._Mesh);
            GL.PopMatrix();
			
			this.SwapBuffers();
		}
		
		protected override void OnUpdateFrame (FrameEventArgs e)
		{
            this._Rot += 1;
		}

        /// <summary>
        /// Draws the specified mesh to the current graphics context.
        /// </summary>
        private void _DrawMesh(Mesh Mesh)
        {
            GL.Begin(BeginMode.Triangles);
            foreach (Triangle tri in Mesh.Triangles)
            {
                foreach (Point point in tri.Points)
                {
                    GL.Vertex3(point);
                }
            }
            GL.End();
        }

        private double _Rot = 0.0;
        private Mesh _Mesh;
	}
}
