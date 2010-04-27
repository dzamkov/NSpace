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
    public class View : IRenderable
    {
        public View(Section Section, IVisual RootVisual)
        {
            this._Aspect = 1.0;
            this._FOV = Math.PI / 5.0;
            this._Section = Section;
            this._RootVisual = RootVisual;
        }

        /// <summary>
        /// Gets the section this view is placed on.
        /// </summary>
        public Section Section
        {
            get
            {
                return this._Section;
            }
        }

        /// <summary>
        /// Gets or sets the root visual, which is the only visual
        /// to be rendered directly by this view.
        /// </summary>
        public IVisual RootVisual
        {
            get
            {
                return this._RootVisual;
            }
            set
            {
                this._RootVisual = value;
            }
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
        /// Renders visuals after setting transformation matricies.
        /// </summary>
        public virtual void ContentRender()
        {
            // Get a list of all visuals and renderables rendered.
            Dictionary<IVisual, IRenderable> rens = new Dictionary<IVisual, IRenderable>();
            Stack<IVisual> unproc = new Stack<IVisual>();
            unproc.Push(this._RootVisual);
            while (unproc.Count > 0)
            {
                IVisual vis = unproc.Pop();

                // Check if used yet
                if (!rens.ContainsKey(vis))
                {
                    IVisualContext viscont = vis.GetContext(Matrix.Identity, Bound.Huge, double.PositiveInfinity, this);

                    // Add child visuals to unprocessed stack
                    IEnumerable<IVisual> children = viscont.Children;
                    if (children != null)
                    {
                        foreach (IVisual child in viscont.Children)
                        {
                            unproc.Push(child);
                        }
                    }

                    // Add entry for visual/renderable
                    IRenderable rend = viscont.Renderable;
                    if (rend != null)
                    {
                        rens.Add(vis, viscont.Renderable);
                    }
                }
            }
            
            // Actual rendering
            foreach(KeyValuePair<IVisual, IRenderable> v in rens)
            {
                GL.PushMatrix();
                Matrix4d mat = this._Section.GetRelation(v.Key.Section).Inverse.SpaceTransform;
                GL.MultMatrix(ref mat);
                v.Value.Render();
                GL.PopMatrix();
            }      
        }

        public virtual void Render()
        {
            // Get variables for view
            Matrix4d proj = this.ProjectionMatrix;
            Matrix4d view = this.ViewMatrix;

            // Set projection and model matrices
            GL.MatrixMode(MatrixMode.Projection);
            GL.PushMatrix();
            GL.LoadMatrix(ref proj);
            GL.MatrixMode(MatrixMode.Modelview);
            GL.PushMatrix();
            GL.LoadMatrix(ref view);

            // Render
            this.ContentRender();

            // Reset view
            GL.PopMatrix();
            GL.MatrixMode(MatrixMode.Projection);
            GL.PopMatrix();
        }

        private Section _Section;
        private IVisual _RootVisual;
        private double _Aspect;
        private double _FOV;
    }
}
