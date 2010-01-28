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

            // Enable graphics features
            GL.Enable(EnableCap.DepthTest);

            // Create a terrain
            this._Terrain = new SinkSource<Triangle>();
            Terrain.Create(this._Terrain, null);
		}

        protected override void OnRenderFrame(FrameEventArgs e)
		{
			// Get variables for view
			double aspect = (double)this.Width / (double)this.Height;
			double fov = Math.Atan(0.5);
			Vector3d eye = new Vector3d(0.0, 4.0, -2.0);
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
            this._DrawMesh(this._Terrain);
            GL.PopMatrix();
			
			this.SwapBuffers();
		}
		
		protected override void OnUpdateFrame (FrameEventArgs e)
		{
            this._Rot += 1;
		}

        /// <summary>
        /// Draws the specified mesh(collection of triangles) to the current graphics context.
        /// </summary>
        private void _DrawMesh(ISource<Triangle> Mesh)
        {
            GL.Begin(BeginMode.Triangles);
            foreach (Triangle tri in Mesh.Items)
            {
                foreach (Point point in tri.Points)
                {
                    ColoredPoint cp = point as ColoredPoint;
                    if (cp != null)
                    {
                        GL.Color4(cp.Color);
                    }
                    GL.Vertex3(point);
                }
            }
            GL.End();
        }

        private double _Rot = 0.0;
        private SinkSource<Triangle> _Terrain;
	}
}
