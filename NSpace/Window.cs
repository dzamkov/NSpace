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
            Texture tex = Texture.LoadFromFile("../../TestTex.png");
            Primitive.CreateCube(m, 1.0);
            this._World = new ComplexSection();
            this._World.AddChild(this._View, Matrix.Identity);
            Random r = new Random();
            for (int x = -5; x < 5; x++)
            {
                for (int y = -5; y < 5; y++)
                {
                    for (int z = -5; z < 5; z++)
                    {
                        if (r.Next(0, 10) == 0)
                        {
                            Model obj = Model.Create(m, new TextureNormalMaterial(tex));
                            this._World.AddChild(obj,
                                Matrix.Transform(
                                    Matrix.Translate(new Vector((double)x, (double)y, (double)z)),
                                    Matrix.Scale(0.2)));
                        }
                    }
                }
            }
		}

        protected override void OnRenderFrame(FrameEventArgs e)
		{
            // Clear current viewport
            GL.Clear(ClearBufferMask.DepthBufferBit | ClearBufferMask.ColorBufferBit);
			
            // Render view
            this._View.InverseParentTransform = Matrix.Lookat(
                new Vector(0.0, 0.0, 1.0),
                new Vector(Math.Sin(this._Rot) * 2.0, Math.Cos(this._Rot) * 2.0, 2.0),
                new Vector(0.0, 0.0, 0.0));
            this._View.Aspect = (double)this.Width / (double)this.Height;
            this._View.Render();

            // Push graphics to screen
			this.SwapBuffers();
		}
		
		protected override void OnUpdateFrame (FrameEventArgs e)
		{
            this._Rot += 0.01;
		}

        private double _Rot = 0.0;
        private ComplexSection _World;
        private View _View;
	}
}
