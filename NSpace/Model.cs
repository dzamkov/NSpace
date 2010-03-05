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
    /// A representation of an object made from a mesh that has a material attached.
    /// </summary>
    public class Model : IVisual, IVisualContext
    {
        public Model(IVisualShape Shape, Section Section)
        {
            this._Shape = Shape;
            this._Section = Section;
        }

        /// <summary>
        /// Creates a new model with the specified mesh and material in the
        /// specified section.
        /// </summary>
        public static Model Create(IVisualShape Shape, Section Section)
        {
            return new Model(Shape, Section);
        }

        public Section Section
        {
            get 
            {
                return this._Section;
            }
        }

        public Bound Bound
        {
            get 
            {
                return this._Shape.Bound;
            }
        }

        public IVisualContext GetContext(Matrix ScreenTransform, Bound ScreenBounds, double Resolution, View View)
        {
            return this;
        }

        public IEnumerable<IVisual> Children
        {
            get 
            {
                return null;
            }
        }

        public IRenderable Renderable
        {
            get 
            {
                return this._Shape.Renderable;
            }
        }

        /// <summary>
        /// Gets the shape this model shows.
        /// </summary>
        public IVisualShape Shape
        {
            get
            {
                return this._Shape;
            }
        }

        private IVisualShape _Shape;
        private Section _Section;
    }
}
