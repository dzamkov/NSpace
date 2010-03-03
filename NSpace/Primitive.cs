//----------------------------------------
// Copyright (c) 2010, Dmitry Zamkov 
// Open source under the new BSD License
//----------------------------------------
using System;
using System.Collections.Generic;

namespace NSpace
{
    /// <summary>
    /// Contains methods for creating geometric primitives.
    /// </summary>
    public static class Primitive
    {
        /// <summary>
        /// Creates a cube in the specified edit context. This does not commit the
        /// changes made. The cube will have uv data.
        /// </summary>
        public static void CreateCube(Mesh.IEditContext Context, double EdgeLength)
        {
            double hlength = EdgeLength / 2.0;
            for (int t = 0; t < 6; t++)
            {
                Geometry[] points = new Geometry[4];
                for (int r = 0; r < 4; r++)
                {
                    double a = (r % 2) < 1 ? 0.0 : 1.0;
                    double b = (r % 4) < 2 ? 0.0 : 1.0;
                    double c = (t % 6) < 3 ? 0.0 : 1.0;
                    Vector vec = new Vector();
                    switch (t % 3)
                    {
                        case 0: vec = new Vector(a, b, c); break;
                        case 1: vec = new Vector(b, c, a); break;
                        case 2: vec = new Vector(c, a, b); break;
                    }
                    points[r] = Point.Create(vec * EdgeLength - new Vector(hlength, hlength, hlength), a, b);
                }
                if (t < 3)
                {
                    Context.AddTriangle(Triangle.Create(points[0], points[2], points[1]));
                    Context.AddTriangle(Triangle.Create(points[1], points[2], points[3]));
                }
                else
                {
                    Context.AddTriangle(Triangle.Create(points[0], points[1], points[2]));
                    Context.AddTriangle(Triangle.Create(points[1], points[3], points[2]));
                }
            }
        }

        /// <summary>
        /// Creates a cube in the specified mesh.
        /// </summary>
        public static void CreateCube(Mesh Mesh, double EdgeLength)
        {
            Mesh.IEditContext ec = Mesh.GetEditContext();
            CreateCube(ec, EdgeLength);
            ec.Commit();
        }
    }
}
