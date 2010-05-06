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
            ReferenceFrame a = r.CreateChild(new RotationalFrameRelation(new Time(4.0)));
            ReferenceFrame camera = a.CreateChild(new AfflineTransformFrameRelation(Matrix.Lookat(new Vector(0.0, 0.0, 1.0), new Vector(-30.0, 0.0, 30.0), new Vector())));

            // Add some cubes
            ReferenceFrame bigcubeframe = r.CreateChild(new AfflineTransformFrameRelation(Matrix.Scale(10.0)));
            Cube bigcube = new Cube(bigcubeframe, new SimpleVolumeMaterial(new SolidColorMaterial(Color.RGB(0.0, 0.5, 0.5), 0.1)));
            ReferenceFrame smallcubeframe = r.CreateChild(new AfflineTransformFrameRelation(
                Matrix.Transform(Matrix.Scale(new Vector(15.0, 3.0, 6.0)), 
                    Matrix.Transform(
                        Matrix.Translate(new Vector(0.0, -2.0, 2.0)),
                        Matrix.Yaw(0.2)))));
            Cube smallcube = new Cube(smallcubeframe, new SimpleVolumeMaterial(new SolidColorMaterial(Color.RGB(0.0, 1.0, 0.0), 0.1)));

            this._Scene = new Scene(new Union(new IVolume[] { bigcube, smallcube }), camera, (double)this.Width / (double)this.Height);


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
