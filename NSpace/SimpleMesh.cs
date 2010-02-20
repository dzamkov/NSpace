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
    /// Mesh that can perform all operations, but with no optimization. This mesh
    /// is not thread safe.
    /// </summary>
    public class SimpleMesh : Mesh, Mesh.IEditContext
    {
        public SimpleMesh()
        {
            this._Tris = new Dictionary<Triangle, object>();
        }

        public override IEnumerable<Triangle> Triangles
        {
            get 
            {
                return this._Tris.Keys;
            }
        }

        public override int TriangleCount
        {
            get
            {
                return this._Tris.Count;
            }
        }

        void Mesh.IEditContext.Commit()
        {
            
        }

        void Mesh.IEditContext.AddTriangle(Triangle Tri)
        {
            this._Tris[Tri] = null;
        }

        Triangle Mesh.IEditContext.ModifyTriangle(Triangle Tri)
        {
            return Tri;
        }

        void Mesh.IEditContext.RemoveTriangle(Triangle Tri)
        {
            this._Tris.Remove(Tri);
        }

        Point Mesh.IEditContext.ModifyPoint(Point Point)
        {
            return Point;
        }

        public override Mesh.IEditContext GetEditContext()
        {
            return this;
        }

        private Dictionary<Triangle, object> _Tris; // Simulated set with dictionary
    }
}
