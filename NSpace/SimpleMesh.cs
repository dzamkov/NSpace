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
            this._Tris = new Dictionary<Geometry, object>();
        }

        public override IEnumerable<Geometry> Triangles
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

        void Mesh.IEditContext.AddTriangle(Geometry Tri)
        {
            this._Tris[new ElasticGeometry(this, Tri)] = null;
        }

        Geometry Mesh.IEditContext.ModifyTriangle(Geometry Tri)
        {
            return Tri;
        }

        void Mesh.IEditContext.RemoveTriangle(Geometry Tri)
        {
            this._Tris.Remove(Tri);
        }

        Geometry Mesh.IEditContext.ModifyPoint(Geometry Point)
        {
            return Point;
        }

        public override Mesh.IEditContext GetEditContext()
        {
            return this;
        }

        private Dictionary<Geometry, object> _Tris; // Simulated set with dictionary
    }
}
