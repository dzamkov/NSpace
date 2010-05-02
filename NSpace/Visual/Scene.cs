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
        public Scene(IVolume Volume, ReferenceFrame Camera)
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
        /// Gets the frame of reference the camera is in. The camera will be at (0,0,0) in this frame of reference
        /// looking towards (1,0,0).
        /// </summary>
        public ReferenceFrame Camera
        {
            get
            {
                return this._Camera;
            }
        }

        /// <summary>
        /// Renders the volume(its state at the specified time in terms
        /// of the camera frame of reference) to the current GL graphics context.
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
            ISingleSectionMeshSurface surface; this._Volume.Surface.Convert<ISingleSectionMeshSurface>(out surface);
            if (surface != null)
            {
                IUniformShape unishape; surface.Convert<IUniformShape>(out unishape);
                if (unishape != null)
                {
                    SolidColorMaterial vismat; unishape.Material.Convert<SolidColorMaterial>(out vismat);
                    if (vismat != null)
                    {
                        // Assume every vertex uses the same frame of reference.
                        IFrameRelation relate = this._Camera.GetRelation(surface.Frame);
                        GL.Begin(BeginMode.Triangles);
                        GL.Color4(vismat.Color);
                        foreach (IMeshTriangle tri in surface.Triangles)
                        {
                            foreach (IMeshVertex vert in tri.Vertices)
                            {
                                GL.Vertex3(relate.Transform(new Event(vert.Position, Time)).Point);
                            }
                        }
                        GL.End();
                    }
                }
            }
        }

        private IVolume _Volume;
        private ReferenceFrame _Camera;
    }
}