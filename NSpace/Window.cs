//----------------------------------------
// Copyright (c) 2010, Dmitry Zamkov 
// Open source under the new BSD License
//----------------------------------------
using System;
using System.Collections.Generic;
using OpenTK;
using OpenTK.Graphics;
using OpenTK.Input;
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
            GL.Enable(EnableCap.CullFace);
            GL.EnableClientState(EnableCap.VertexArray);
            GL.EnableClientState(EnableCap.NormalArray);
            GL.FrontFace(FrontFaceDirection.Ccw);

            // Lighting
            GL.Enable(EnableCap.Light0);
            GL.Light(LightName.Light0, LightParameter.Ambient, Color.RGB(0, 0, 0));
            GL.Light(LightName.Light0, LightParameter.Diffuse, Color.RGB(0.5, 0.5, 0.5));
            GL.Light(LightName.Light0, LightParameter.Specular, Color.RGB(1.0, 1.0, 1.0));
            GL.Light(LightName.Light0, LightParameter.Position, new Vector4(0.0f, 0.0f, 2.0f, 1.0f));

            // Create world
            this._World = new Section();

            // Create view
            this._RootVisual = new MultiVisual(this._World);
            this._View = new View(
                this._World.AddChild(Matrix.Lookat(
                    new Vector(0.0, 0.0, 1.0), 
                    new Vector(10.0, 0.0, 10.0), 
                    new Vector(0.0, 0.0, 0.0))), 
                this._RootVisual);

            // Create a cube and add to the world
            Mesh m = new SimpleMesh();
            Texture tex = Texture.LoadFromFile("../../TestTex.png");
            Primitive.CreateCube(m, 1.0);
            Random r = new Random();
            for (int x = -5; x < 5; x++)
            {
                for (int y = -5; y < 5; y++)
                {
                    for (int z = -5; z < 5; z++)
                    {
                        if (r.Next(0, 10) == 0)
                        {
                            Section objsect = 
                                this._World.AddChild(
                                    Matrix.Transform(
                                        Matrix.Translate(new Vector((double)x, (double)y, (double)z)),
                                        Matrix.Scale(0.2)));
                            this._RootVisual.Add(Model.Create(m, new TextureNormalMaterial(tex), objsect));
                        }
                    }
                }
            }

            // Add a little line
            this._RootVisual.Add(
                DebugVisual.CreateLine(
                    new Vector(2.0, 2.0, 2.0), 
                    new Vector(-2.0, -2.0, -2.0), 
                    this._World));

            // Initialize update times
            this._LastUpdate = DateTime.Now;
		}

        protected override void OnRenderFrame(FrameEventArgs e)
		{
            // Clear current viewport
            GL.Clear(ClearBufferMask.DepthBufferBit | ClearBufferMask.ColorBufferBit);
			
            // Render view
            this._View.Aspect = (double)this.Width / (double)this.Height;
            this._View.Render();

            // Push graphics to screen
			this.SwapBuffers();
		}
		
		protected override void OnUpdateFrame (FrameEventArgs e)
		{
            DateTime curtime = DateTime.Now;
            double updatetime = (curtime - this._LastUpdate).TotalSeconds;
            this._LastUpdate = curtime;

            // Keyboard movement
            Matrix trans = Matrix.Identity;
            double movespeed = 2.0;
            double turnspeed = Math.PI / 2.0;
            double scalespeed = 0.5;
            if (this.Keyboard[Key.Q]) trans *= Matrix.Scale(Math.Pow(2.0, updatetime * scalespeed), 1.0, 1.0);
            if (this.Keyboard[Key.E]) trans *= Matrix.Scale(Math.Pow(2.0, updatetime * -scalespeed), 1.0, 1.0);
            if (this.Keyboard[Key.W]) trans *= Matrix.Translate(new Vector(updatetime * movespeed, 0.0, 0.0));
            if (this.Keyboard[Key.A]) trans *= Matrix.Translate(new Vector(0.0, updatetime * movespeed, 0.0));
            if (this.Keyboard[Key.S]) trans *= Matrix.Translate(new Vector(updatetime * -movespeed, 0.0, 0.0));
            if (this.Keyboard[Key.D]) trans *= Matrix.Translate(new Vector(0.0, updatetime * -movespeed, 0.0));
            if (this.Keyboard[Key.Up]) trans *= Matrix.Pitch(updatetime * turnspeed);
            if (this.Keyboard[Key.Left]) trans *= Matrix.Yaw(updatetime * turnspeed);
            if (this.Keyboard[Key.Down]) trans *= Matrix.Pitch(updatetime * -turnspeed);
            if (this.Keyboard[Key.Right]) trans *= Matrix.Yaw(updatetime * -turnspeed);
            this._View.Section.InverseParentTransform = Matrix.Transform(trans, this._View.Section.InverseParentTransform);

            this._Rot += Math.PI / 2.0 * updatetime;
		}

        private DateTime _LastUpdate;
        private double _Rot = 0.0;
        private Section _World;
        private MultiVisual _RootVisual;
        private View _View;
	}
}
