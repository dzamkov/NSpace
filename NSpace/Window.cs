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
            ReferenceFrame r = new ReferenceFrame();
            ReferenceFrame a = r.CreateChild(new AfflineTransformFrameRelation(Matrix.Translate(new Vector(-30.0, 0.0, 0.0))));

            // Add some cubes
            List<IVolume> cubes = new List<IVolume>();
            Random rand = new Random();
            for (int t = 0; t < 10; t++)
            {
                ReferenceFrame b = r.CreateChild(new AfflineTransformFrameRelation(
                    Matrix.Translate(
                        new Vector(
                            (rand.NextDouble() * 10.0) - 5.0, 
                            (rand.NextDouble() * 10.0) - 5.0, 
                            (rand.NextDouble() * 10.0) - 5.0))));
                ReferenceFrame c = b.CreateChild(new AfflineTransformFrameRelation(
                    Matrix.Scale(rand.NextDouble() + 1.0)));
                ReferenceFrame d = c.CreateChild(new RotationalFrameRelation(
                    new Time(rand.NextDouble() * 6.0 + 1.0)));
                cubes.Add(new Cube(d, new SolidColorMaterial(Color.HLSA(rand.NextDouble() * 360.0, 0.5, 1.0, 1.0), 0.1)));
            }

            this._Scene = new Scene(new Union(cubes), a, (double)this.Width / (double)this.Height);


            this._Time = new Time(0.0);
            this._LastUpdate = DateTime.Now;
		}

        protected override void OnRenderFrame(FrameEventArgs e)
		{
            GL.ClearColor(0.0f, 0.0f, 0.0f, 1.0f);
            GL.Clear(ClearBufferMask.ColorBufferBit | ClearBufferMask.DepthBufferBit | ClearBufferMask.StencilBufferBit);

            this._Scene.Render(this._Time);

            this.SwapBuffers();
		}
		
		protected override void OnUpdateFrame (FrameEventArgs e)
		{
            DateTime curtime = DateTime.Now;
            double updatetime = (curtime - this._LastUpdate).TotalSeconds;
            this._LastUpdate = curtime;

            this._Time += new Time(updatetime);
		}

        private DateTime _LastUpdate;
        private Scene _Scene;
        private Time _Time;
	}
}
