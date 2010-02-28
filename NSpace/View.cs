//----------------------------------------
// Copyright (c) 2010, Dmitry Zamkov 
// Open source under the new BSD License
//----------------------------------------
using System;
using System.Collections.Generic;
using OpenTK;
using OpenTK.Graphics.OpenGL;

namespace NSpace
{
    /// <summary>
    /// Represents a persistent view of the world. The view renders foward to its local
    /// x vector with the z vector at the top and the y vector to the right.
    /// </summary>
    public class View : Section, IRenderable
    {
        public View()
        {
            this._Aspect = 1.0;
            this._FOV = Math.PI / 5.0;
        }
        
        /// <summary>
        /// Gets the projection matrix used by the view.
        /// </summary>
        protected virtual Matrix4d ProjectionMatrix
        {
            get
            {
                return Matrix4d.Perspective(this._FOV, this._Aspect, 0.01, 100.0);
            }
        }

        /// <summary>
        /// Gets the view matrix used by the view.
        /// </summary>
        protected virtual Matrix4d ViewMatrix
        {
            get
            {
                return Matrix4d.LookAt(
                    0.0, 0.0, 0.0,
                    1.0, 0.0, 0.0,
                    0.0, 0.0, 1.0);
            }
        }

        /// <summary>
        /// Gets or sets the aspect ratio for the view. This should be the width divded by
        /// height of the render target.
        /// </summary>
        public virtual double Aspect
        {
            get
            {
                return this._Aspect;
            }
            set
            {
                this._Aspect = value;
            }
        }

        /// <summary>
        /// Gets or sets the angle of the field of view in radians.
        /// </summary>
        public virtual double FOV
        {
            get
            {
                return this._FOV;
            }
            set
            {
                this._FOV = value;
            }
        }

        /// <summary>
        /// Renders the specified section.
        /// </summary>
        public virtual void ContentRender(Section Section)
        {
            IVisual vis = Section.Visual;
            if (vis != null)
            {
                IVisualContext context = vis.GetContext(Matrix.Identity, Bound.Huge, double.PositiveInfinity, this);
                IEnumerable<Section> rendersections = context.RenderSections;
                if (rendersections != null)
                {
                    foreach (Section s in context.RenderSections)
                    {
                        this.ContentRender(s);
                    }
                }
                GL.PushMatrix();
                Matrix4d mat = this.GetRelation(Section);
                GL.MultMatrix(ref mat);
                context.Render();
                GL.PopMatrix();
            }
        }

        public virtual void Render()
        {
            // Get variables for view
            Matrix4d proj = this.ProjectionMatrix;
            Matrix4d view = this.ViewMatrix;

            // Set projection and model matrices
            GL.CullFace(CullFaceMode.Front);
            GL.MatrixMode(MatrixMode.Projection);
            GL.PushMatrix();
            GL.LoadMatrix(ref proj);
            GL.MatrixMode(MatrixMode.Modelview);
            GL.PushMatrix();
            GL.LoadMatrix(ref view);

            // Render
            this.ContentRender(this.RootParent);

            // Reset view
            GL.PopMatrix();
            GL.MatrixMode(MatrixMode.Projection);
            GL.PopMatrix();
            GL.CullFace(CullFaceMode.FrontAndBack);
        }

        private double _Aspect;
        private double _FOV;
    }

    /// <summary>
    /// An object that can be rendered, or that can otherwise affect the graphics context directly.
    /// </summary>
    public interface IRenderable
    {
        /// <summary>
        /// Renders to the current graphics context. All settings set within render must be reverted
        /// before the method exits.
        /// </summary>
        void Render();
    }
}
