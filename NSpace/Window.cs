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
            this._WorldSect = new Section();
            this._World = new World();
            this._World.AddPhysicsObject(new GlobalForceObject(new Gravity(this._WorldSect, new Vector(0.0, 0.0, -9.8))));
            this._World.AddPhysicsObject(new GlobalForceObject(new Staticness()));

            // Create view
            this._RootVisual = new MultiVisual(this._WorldSect);
            this._View = new View(
                this._WorldSect.AddChild(Matrix.Lookat(
                    new Vector(0.0, 0.0, 1.0), 
                    new Vector(10.0, 0.0, 10.0), 
                    new Vector(0.0, 0.0, 0.0))), 
                this._RootVisual);

            // Create a cube and add to the world
            Mesh m = new SimpleMesh();
            Texture tex = Texture.LoadFromFile("../../TestTex.png");
            Primitive.CreateCube(m, 1.0);
            this._Cubes = new List<CompanionCube>();
            Random r = new Random();
            for (int x = -5; x < 5; x++)
            {
                for (int y = -5; y < 5; y++)
                {
                    for (int z = -5; z < 5; z++)
                    {
                        if (r.Next(0, 50) == 0)
                        {
                            this._AddCompanionCube(
                                Matrix.Transform(
                                    Matrix.Translate(new Vector((double)x, (double)y, (double)z)),
                                    Matrix.Scale(0.2)),
                                tex, m);
                        }
                    }
                }
            }
            this._AddCompanionCube(
                Matrix.Translate(new Vector(0.0, 0.0, -2.0)),
                tex, m);

            // Initialize update times
            this._LastUpdate = DateTime.Now;
		}

        protected override void OnRenderFrame(FrameEventArgs e)
		{
            // Clear current viewport
            GL.Clear(ClearBufferMask.DepthBufferBit | ClearBufferMask.ColorBufferBit);
			
            // Check collisions/ update models
            Vector sr = new Vector(0.0, 0.0, 0.0);
            Vector sp = new Vector(10.0, 0.0, 0.0);
            TraceHit? closehit = null;
            foreach (CompanionCube cc in this._Cubes)
            {
                RigidBody body = (cc.Body.GetObject(this._World.CurrentTime) as RigidBody);
                Section sect = body.GetSectionAtTime(this._World.CurrentTime);
                cc.Model.Section = sect;
                Matrix trans = this._View.Section.GetRelation(sect);
                Vector tsr = trans * sr;
                Vector tsp = trans * sp;
                TraceHit hit = new TraceHit();
                if ((body.Properties.Shape as ISurface).TracePoint(tsr, tsp, ref hit))
                {
                    Vector realpos = sect.ParentTransform * hit.Position;
                    if (closehit == null || closehit.Value.Length > hit.Length)
                    {
                        closehit = new TraceHit() { Length = hit.Length, Position = realpos, Normal = hit.Normal };
                    }
                }
            }
            if (this._Line != null)
            {
                this._RootVisual.Remove(this._Line);
                this._Line = null;
            }
            if (closehit != null)
            {
                Vector hitpos = closehit.Value.Position;
                Vector norm = closehit.Value.Normal * 0.1;
                this._Line = DebugVisual.CreateLine(hitpos, hitpos + norm, this._WorldSect);
                this._RootVisual.Add(this._Line);
            }

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

            // World update
            this._World.Update(new TimeSpan(updatetime));

            // Navigation
            Matrix trans = Matrix.Identity;
            double movespeed = 2.0;
            double turnspeed = Math.PI / 2.0;
            double scalespeed = 0.5;

            // Mouse navigation
            if (this.Keyboard[Key.Tab])
            {
                int width = this.Width; // Mouse navigation, thanks to cobra
                int height = this.Height;
                int xoffset = width / 2 - this.Mouse.X;
                int yoffset = height / 2 - this.Mouse.Y;

                trans *= Quaternion.AxisRotate(new Vector(0.0, 0.0, 1.0), 0.025 * xoffset * 0.2).ToMatrix();
                trans *= Quaternion.AxisRotate(new Vector(0.0, 1.0, 0.0), 0.025 * -yoffset * 0.2).ToMatrix();

                System.Drawing.Point pos = PointToScreen(new System.Drawing.Point(Width / 2, Height / 2));
                Cursor.Position = pos;
            }

            // Keyboard navigation
            if (this.Keyboard[Key.Q]) trans *= Matrix.Scale(Math.Pow(2.0, updatetime * scalespeed), 1.0, 1.0);
            if (this.Keyboard[Key.E]) trans *= Matrix.Scale(Math.Pow(2.0, updatetime * -scalespeed), 1.0, 1.0);
            if (this.Keyboard[Key.W]) trans *= Matrix.Translate(new Vector(updatetime * movespeed, 0.0, 0.0));
            if (this.Keyboard[Key.A]) trans *= Matrix.Translate(new Vector(0.0, updatetime * movespeed, 0.0));
            if (this.Keyboard[Key.S]) trans *= Matrix.Translate(new Vector(updatetime * -movespeed, 0.0, 0.0));
            if (this.Keyboard[Key.D]) trans *= Matrix.Translate(new Vector(0.0, updatetime * -movespeed, 0.0));
            if (this.Keyboard[Key.Up]) trans *= Quaternion.AxisRotate(new Vector(0.0, 1.0, 0.0), -updatetime * turnspeed).ToMatrix();
            if (this.Keyboard[Key.Left]) trans *= Quaternion.AxisRotate(new Vector(0.0, 0.0, 1.0), updatetime * turnspeed).ToMatrix();
            if (this.Keyboard[Key.Down]) trans *= Quaternion.AxisRotate(new Vector(0.0, 1.0, 0.0), updatetime * turnspeed).ToMatrix();
            if (this.Keyboard[Key.Right]) trans *= Quaternion.AxisRotate(new Vector(0.0, 0.0, 1.0), -updatetime * turnspeed).ToMatrix();
            this._View.Section.ParentTransform = Matrix.Transform(trans, this._View.Section.ParentTransform);
            

            this._Rot += Math.PI / 2.0 * updatetime;
		}

        /// <summary>
        /// Data used to represent a companion cube.
        /// </summary>
        public struct CompanionCube
        {
            public Model Model;
            public Marker Body;
        }

        private void _AddCompanionCube(Matrix Offset, Texture Texture, Mesh Mesh)
        {
            CompanionCube cc = new CompanionCube();
            Section objsect = this._WorldSect.AddChild(Offset);
            this._RootVisual.Add(cc.Model = Model.Create(Mesh, new TextureNormalMaterial(Texture), objsect));
            cc.Body = new Marker(RigidBody.Create(this._World, objsect,
                new RigidBody.Property(
                    new MeshSurface(Mesh),
                    new Vector(0.0, 0.0, 0.0),
                    1.0)));
            this._Cubes.Add(cc);
        }

        private List<CompanionCube> _Cubes;
        private World _World;

        private DateTime _LastUpdate;
        private double _Rot = 0.0;
        private Section _WorldSect;
        private MultiVisual _RootVisual;
        private View _View;
        private DebugVisual _Line;
	}
}
