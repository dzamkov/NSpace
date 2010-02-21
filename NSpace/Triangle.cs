//----------------------------------------
// Copyright (c) 2010, Dmitry Zamkov 
// Open source under the new BSD License
//----------------------------------------
using System;
using System.Collections.Generic;
using System.Linq;

namespace NSpace
{
    /// <summary>
    /// Methods and classes for the manipulation of triangles.
    /// </summary>
    public static class Triangle
    {
        /// <summary>
        /// Triangle data that can be stored in geometry.
        /// </summary>
        public class Data : IData
        {
            public Geometry A;
            public Geometry B;
            public Geometry C;

            /// <summary>
            /// Gets an array of the points that make up this data.
            /// </summary>
            public Geometry[] Points
            {
                get
                {
                    return new Geometry[]
                    { 
                        this.A, this.B, this.C
                    };
                }
            }

            public IData Copy()
            {
                return new Data()
                {
                    A = this.A,
                    B = this.B,
                    C = this.C
                };
            }

            /// <summary>
            /// Gets or sets the point in this triangle at the specified index.
            /// </summary>
            public Geometry this[int Index]
            {
                get
                {
                    switch (Index)
                    {
                        case 0: return this.A;
                        case 1: return this.B;
                        case 2: return this.C;
                    }
                    return null;
                }
                set
                {
                    switch (Index)
                    {
                        case 0: this.A = value; break;
                        case 1: this.B = value; break;
                        case 2: this.C = value; break;
                    }
                }
            }

            /// <summary>
            /// Gets the normal of this triangle, which is a normalized vector pointing
            /// outward from this triangle's face. This vector is in relation to the
            /// origin.
            /// </summary>
            public Vector Normal
            {
                get
                {
                    Vector pa = this[0].GetData<Point.Data>().Position;
                    Vector pb = this[1].GetData<Point.Data>().Position;
                    Vector pc = this[2].GetData<Point.Data>().Position;
                    Vector a = pb - pa;
                    Vector b = pc - pa;
                    Vector c = Vector.Cross(a, b);
                    c.Normalize();
                    return c;
                }
            }

            /// <summary>
            /// Gets the midpoint of this triangle, the average of all the points.
            /// </summary>
            public Vector Midpoint
            {
                get
                {
                    return (this[0].GetData<Point.Data>().Position
                        + this[1].GetData<Point.Data>().Position
                        + this[2].GetData<Point.Data>().Position) / 3.0;
                }
            }

            /// <summary>
            /// Gets the triangle data from the geometry.
            /// </summary>
            public static explicit operator Data(Geometry Geom)
            {
                return Geom.GetData<Data>();
            }
        }

        /// <summary>
        /// Creates a triangle from the provided points.
        /// </summary>
        public static Geometry Create(Geometry[] Points)
        {
            ElasticGeometry geom = new ElasticGeometry(null);
            geom.SetData<Data>(new Data()
            {
                A = Points[0],
                B = Points[1],
                C = Points[2]
            });
            return geom;
        }

        /// <summary>
        /// Creates a triangle from the provided points.
        /// </summary>
        public static Geometry Create(Geometry A, Geometry B, Geometry C)
        {
            ElasticGeometry geom = new ElasticGeometry(null);
            geom.SetData<Data>(new Data()
            {
                A = A,
                B = B,
                C = C
            });
            return geom;
        }

        /// <summary>
        /// Gets the base points in the specified triangle. This ignores triangle superdata and is
        /// needed if editing is intended on any of the points.
        /// </summary>
        public static Geometry[] Points(Geometry Triangle)
        {
            return Triangle.GetData<Data>().Points;
        }
    }
}
