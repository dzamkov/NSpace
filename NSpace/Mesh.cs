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
    /// A collection of triangles and points that make up a shape.
    /// </summary>
    public interface IMesh : IShape
    {
        /// <summary>
        /// Gets the collection of all triangles within this mesh. This may be an expensive operation
        /// as it requires the mesh to convert from its internal storage format to Point/Triangle.
        /// Relationships between triangles will be preserved; if two triangles share a point, the
        /// point will be the same object for both triangles.
        /// </summary>
        IEnumerable<Triangle> Triangles { get; }

        /// <summary>
        /// Gets the amount of triangles in this mesh.
        /// </summary>
        int TriangleCount { get; }
    }
}
