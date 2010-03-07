//----------------------------------------
// Copyright (c) 2010, Dmitry Zamkov 
// Open source under the new BSD License
//----------------------------------------
using System;
using System.Collections.Generic;

namespace NSpace
{
    /// <summary>
    /// A static form or shape in three-dimensional space. This can be a volume, surface, line, point,
    /// nothing or a combination of these.
    /// </summary>
    public interface IShape
    {
        /// <summary>
        /// Gets a bound for this shape such that nothing outside of this bound intersects the shape.
        /// </summary>
        Bound Bound { get; }
    }

    /// <summary>
    /// A two-dimensional surface.
    /// </summary>
    public interface ISurface : IShape
    {
        /// <summary>
        /// Traces a moving point(line) across the surface from the start to the stop position and
        /// returns true and sets the result if it hits part of the surface. This, like every other
        /// trace function returns the closest hit point.
        /// </summary>
        bool TracePoint(Vector Start, Vector Stop, ref TraceHit Result);
    }

    /// <summary>
    /// A shape that can be divided into some amount of child shapes that together
    /// make up the original.
    /// </summary>
    public interface IDivisibleShape : IShape
    {
        /// <summary>
        /// Gets a set of shapes that make up this shape or itself if this
        /// is not a compound shape.
        /// </summary>
        IEnumerable<IShape> NaturalDivisions { get; }
    }
}
