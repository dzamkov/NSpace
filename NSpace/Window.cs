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
            GL.Enable(EnableCap.Lighting);
            GL.Enable(EnableCap.ColorMaterial);
            GL.Enable(EnableCap.Texture2D);
            GL.EnableClientState(EnableCap.VertexArray);
            GL.EnableClientState(EnableCap.NormalArray);
            GL.ColorMaterial(MaterialFace.Front, ColorMaterialParameter.AmbientAndDiffuse);

            // Lighting
            GL.Enable(EnableCap.Light0);
            GL.Light(LightName.Light0, LightParameter.Ambient, Color.RGB(0, 0, 0));
            GL.Light(LightName.Light0, LightParameter.Diffuse, Color.RGB(0.5, 0.5, 0.5));
            GL.Light(LightName.Light0, LightParameter.Specular, Color.RGB(1.0, 1.0, 1.0));
            GL.Light(LightName.Light0, LightParameter.Position, new Vector4(0.0f, 0.0f, 2.0f, 1.0f));

            // Create a cube
            Mesh m = new SimpleMesh();
            Primitive.CreateCube(m, 1.0);
            this._Cube = Model.Create(m,
                new TextureNormalMaterial(
                    Texture.LoadFromFile("../../TestTex.png")));
		}

        protected override void OnRenderFrame(FrameEventArgs e)
		{
			// Get variables for view
			double aspect = (double)this.Width / (double)this.Height;
			double fov = Math.Atan(0.5);
			Vector3d eye = new Vector3d(0.0, 2.0, -2.0);
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
            this._Cube.Render();
            GL.PopMatrix();
			
			this.SwapBuffers();
		}
		
		protected override void OnUpdateFrame (FrameEventArgs e)
		{
            this._Rot += 1;
		}

        private double _Rot = 0.0;
        private Model _Cube;
	}
}
