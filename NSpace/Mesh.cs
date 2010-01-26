//----------------------------------------
// Copyright (c) 2010, Dmitry Zamkov 
// Open source under the new BSD License
//----------------------------------------
using System;
using System.Collections.Generic;

namespace NSpace
{
    /// <summary>
    /// A collection of related points and triangles. Points and triangles within a mesh
    /// can not be used with other meshes.
    /// </summary>
    public class Mesh
    {
        public Mesh()
        {
            this._Tris = new List<Triangle>();
        }

        /// <summary>
        /// Creates a new point in the mesh without any specified location. The location can be
        /// later specified with x, y, and z.
        /// </summary>
        public virtual Point CreatePoint()
        {
            return new Point();
        }

        /// <summary>
        /// Creates a new triangle in the mesh without specifing its points. The points can later
        /// be specified with a, b and c.
        /// </summary>
        public virtual Triangle CreateTriangle()
        {
            Triangle t = new Triangle();
            this.AddTriangle(t);
            return t;
        }

        /// <summary>
        /// Creates a new point in the mesh with the supplied unit vectors. This should be
        /// called to create points because it places the correct data in them.
        /// </summary>
        public Point CreatePoint(double X, double Y, double Z)
        {
            Point p = this.CreatePoint();
            p.X = X;
            p.Y = Y;
            p.Z = Z;
            return p;
        }

        /// <summary>
        /// Creates a new triangle in the mesh using the supplied endpoints.
        /// </summary>
        public Triangle CreateTriangle(Point A, Point B, Point C)
        {
            Triangle t = this.CreateTriangle();
            t.A = A;
            t.B = B;
            t.C = C;
            return t;
        }

        /// <summary>
        /// Gets the list of triangles in the mesh.
        /// </summary>
        public IEnumerable<Triangle> Triangles
        {
            get
            {
                return this._Tris;
            }
        }

        /// <summary>
        /// Gets the type used to define points in this mesh.
        /// </summary>
        public virtual Type PointType
        {
            get
            {
                return typeof(Point);
            }
        }

        /// <summary>
        /// Gets the type used to define triangles in this mesh.
        /// </summary>
        public virtual Type TriangleType
        {
            get
            {
                return typeof(Triangle);
            }
        }

        /// <summary>
        /// Adds a triangle to the triangle list. This must be done once a new triangle is
        /// created.
        /// </summary>
        protected void AddTriangle(Triangle Triangle)
        {
            this._Tris.Add(Triangle);
        }

        private List<Triangle> _Tris;
    }

    /// <summary>
    /// Mesh that uses points or triangles with additional information. The supplied types, which
    /// are derived from point and triangle, will act as the new type of the points and triangles in
    /// this mesh.
    /// </summary>
    /// <typeparam name="P">The type to use for points in this mesh.</typeparam>
    /// <typeparam name="T">The type to use for triangles in this mesh.</typeparam>
    public class Mesh<P, T> : Mesh
        where P : Point, new()
        where T : Triangle , new()
    {
        public override Point CreatePoint()
        {
            return new P();
        }

        public override Triangle CreateTriangle()
        {
            T t = new T();
            this.AddTriangle(t);
            return t;
        }

        public override Type PointType
        {
            get
            {
                return typeof(P);
            }
        }

        public override Type TriangleType
        {
            get
            {
                return typeof(T);
            }
        }
    }
}