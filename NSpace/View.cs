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
    /// Represents a persistent view of the world.
    /// </summary>
    public class View : IRenderable
    {
        public View()
        {
            this._Up = new Vector(0.0, 0.0, 1.0);
            this._Aspect = 1.0;
            this._FOV = Math.PI / 5.0;
            this._Items = new List<IRenderable>();
        }
        
        /// <summary>
        /// Gets the projection matrix used by the view.
        /// </summary>
        public virtual Matrix ProjectionMatrix
        {
            get
            {
                return Matrix.Perspective(this._FOV, this._Aspect, 0.01, 100.0);
            }
        }

        /// <summary>
        /// Gets the view matrix used by the view.
        /// </summary>
        public virtual Matrix4d ViewMatrix
        {
            get
            {
                return Matrix4d.LookAt(this._Eye, this._Target, this._Up);
            }
        }

        /// <summary>
        /// Gets or sets the position of the eye in the view.
        /// </summary>
        public virtual Vector Eye
        {
            get
            {
                return this._Eye;
            }
            set
            {
                this._Eye = value;
            }
        }

        /// <summary>
        /// Gets or sets the up vector of the view. This should point upward in
        /// the world.
        /// </summary>
        public virtual Vector Up
        {
            get
            {
                return this._Up;
            }
            set
            {
                this._Up = value;
            }
        }

        /// <summary>
        /// Gets or sets the position where the view is looking.
        /// </summary>
        public virtual Vector Target
        {
            get
            {
                return this._Target;
            }
            set
            {
                this._Target = value;
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
        /// Manually adds an item to render in this view.
        /// </summary>
        public virtual void AddItem(IRenderable Item)
        {
            this._Items.Add(Item);
        }

        /// <summary>
        /// Renders all items in the view without setting the projection
        /// or view matricies.
        /// </summary>
        public virtual void ContentRender()
        {
            foreach (IRenderable item in this._Items)
            {
                item.Render();
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
            GL.LoadMatrix(ref proj);
            GL.MatrixMode(MatrixMode.Modelview);
            GL.LoadMatrix(ref view);

            // Render
            this.ContentRender();

            // Reset view
            GL.MatrixMode(MatrixMode.Projection);
            GL.LoadIdentity();
            GL.MatrixMode(MatrixMode.Modelview);
            GL.LoadIdentity();
            GL.CullFace(CullFaceMode.FrontAndBack);
        }

        private List<IRenderable> _Items;
        private Vector _Eye;
        private Vector _Target;
        private Vector _Up;
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
