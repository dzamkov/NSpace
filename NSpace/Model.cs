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
    /// A representation of an object made of triangles that can be rendered.
    /// </summary>
    public class Model
    {
        public Model()
        {
 
        }

        /// <summary>
        /// Gets or sets the source of triangles thisW model displays.
        /// </summary>
        public ISource<Triangle> Source
        {
            get
            {
                return this._Source;
            }
            set
            {
                this._Source = value;
            }
        }

        private ISource<Triangle> _Source;
    }
}
