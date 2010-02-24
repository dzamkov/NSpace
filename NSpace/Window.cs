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

            // Create view
            this._View = new View();

            // Create a cube
            Mesh m = new SimpleMesh();
            Primitive.CreateCube(m, 1.0);
            this._View.AddItem(
                Model.Create(m,
                    new TextureNormalMaterial(
                        Texture.LoadFromFile("../../TestTex.png"))));
		}

        protected override void OnRenderFrame(FrameEventArgs e)
		{
            // Clear current viewport
            GL.Clear(ClearBufferMask.DepthBufferBit | ClearBufferMask.ColorBufferBit);
			
            // Render view
            this._View.Eye = new Vector(Math.Sin(this._Rot) * 2.0, Math.Cos(this._Rot) * 2.0, 2.0);
            this._View.Render();

            // Push graphics to screen
			this.SwapBuffers();
		}
		
		protected override void OnUpdateFrame (FrameEventArgs e)
		{
            this._Rot += 0.01;
		}

        private double _Rot = 0.0;
        private View _View;
	}
}
