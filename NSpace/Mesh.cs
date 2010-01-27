//----------------------------------------
// Copyright (c) 2010, Dmitry Zamkov 
// Open source under the new BSD License
//----------------------------------------
using System;
using System.Collections.Generic;

namespace NSpace
{

    /// <summary>
    /// Collection of triangles that can be drawn and manipulated.
    /// </summary>
    public class Mesh
    {
        public Mesh()
        {
            this._Tris = new List<Triangle>();
        }

        /// <summary>
        /// Adds a new triangle to the mesh.
        /// </summary>
        public void AddTriangle(Triangle Triangle)
        {
            this._Tris.Add(Triangle);
        }

        /// <summary>
        /// Gets all the triangles in the mesh.
        /// </summary>
        public IEnumerable<Triangle> Triangles
        {
            get
            {
                return this._Tris;
            }
        }

        private List<Triangle> _Tris;
    }
}