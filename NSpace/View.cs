//----------------------------------------
// Copyright (c) 2010, Dmitry Zamkov 
// Open source under the new BSD License
//----------------------------------------
using System;
using System.Collections.Generic;
using OpenTK;

namespace NSpace
{

    /// <summary>
    /// An object that can be rendered, or that can otherwise affect the graphics context directly.
    /// </summary>
    public interface IRenderable
    {
        /// <summary>
        /// Renders to the current graphics context.
        /// </summary>
        void Render();
    }
}
