//----------------------------------------
// Copyright (c) 2010, Dmitry Zamkov 
// Open source under the new BSD License
//----------------------------------------
using System;
using System.Collections.Generic;
using NSpace.Physics;

using OpenTK;
using OpenTK.Graphics.OpenGL;

namespace NSpace.Visual
{
    /// <summary>
    /// An object that renders a volume(which can act as a complex scene) to a
    /// two dimensional image.
    /// </summary>
    public class Scene : IImmutable
    {
        public Scene(IVolume Volume, Section Camera)
        {
            this._Volume = Volume;
            this._Camera = Camera;
        }

        /// <summary>
        /// Gets the volume that is being rendered.
        /// </summary>
        public IVolume Volume
        {
            get
            {
                return this._Volume;
            }
        }

        /// <summary>
        /// Gets the section the camera is in. The camera will be at (0,0,0) in this section
        /// looking towards (1,0,0).
        /// </summary>
        public Section Camera
        {
            get
            {
                return this._Camera;
            }
        }

        /// <summary>
        /// Renders the volume(its state at the specified time in terms
        /// of the camera section) to the current GL graphics context.
        /// </summary>
        public void Render(Time Time)
        {
            // Projection and lookat matrix
            Matrix4d proj = Matrix4d.Perspective(Math.PI / 5.0, 1.0, 0.01, 100.0);
            Matrix4d view = Matrix4d.LookAt(0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 1.0);
            GL.MatrixMode(MatrixMode.Projection);
            GL.LoadMatrix(ref proj);
            GL.MatrixMode(MatrixMode.Modelview);
            GL.LoadMatrix(ref view);

            // Do some rendering
            IDefiniteVolume defvol; this._Volume.Convert<IDefiniteVolume>(out defvol);
            if (defvol != null)
            {
                ISingleSectionMeshSurface surface; defvol.Surface.Convert<ISingleSectionMeshSurface>(out surface);
                if (surface != null)
                {
                    // Assume every vertex uses the same section.
                    Matrix4d mat = this._Camera.GetRelation(surface.Section).SpaceTransform;
                    GL.PolygonMode(MaterialFace.FrontAndBack, PolygonMode.Line);
                    GL.PushMatrix();
                    GL.MultMatrix(ref mat);
                    GL.Begin(BeginMode.Triangles);
                    foreach (IMeshTriangle tri in surface.Triangles)
                    {
                        foreach (IMeshVertex vert in tri.Vertices)
                        {
                            GL.Vertex3(vert.Position);
                        }
                    }
                    GL.End();
                    GL.PopMatrix();
                }
            }
        }

        private IVolume _Volume;
        private Section _Camera;
    }
}