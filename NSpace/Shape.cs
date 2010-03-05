//----------------------------------------
// Copyright (c) 2010, Dmitry Zamkov 
// Open source under the new BSD License
//----------------------------------------
using System;
using System.Collections.Generic;

namespace NSpace
{
    /// <summary>
    /// General type for a shape in three-dimensional space. This shape may be a surface, volume, line, point or nothing, just as
    /// long as there is a clear definition of what is inside the shape and what isnt.
    /// </summary>
    public interface IShape
    {
        /// <summary>
        /// The bounding box of the shape in its local coordinates. Nothing that does not intersect this
        /// box intersects the shape.
        /// </summary>
        Bound Bound { get; }
    }

    /// <summary>
    /// A shape with visual properties.
    /// </summary>
    public interface IVisualShape : IShape
    {
        /// <summary>
        /// Gets the renderable that will render the shape in its local coordinate system.
        /// </summary>
        IRenderable Renderable { get; }
    }
}
