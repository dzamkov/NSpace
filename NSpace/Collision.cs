//----------------------------------------
// Copyright (c) 2010, Dmitry Zamkov 
// Open source under the new BSD License
//----------------------------------------
using System;
using System.Collections.Generic;

namespace NSpace
{

    /// <summary>
    /// Information given when the frontside of a surface is hit with a trace.
    /// </summary>
    public struct TraceHit
    {
        /// <summary>
        /// The relative amount along the trace where the hit was at. A value of 0.0 indicates
        /// the hit was at start and a value at 1.0 indicates the hit was at stop.
        /// </summary>
        public double Length;

        /// <summary>
        /// The position at which the surface was hit.
        /// </summary>
        public Vector Position;

        /// <summary>
        /// The normal going outward at the part of the surface that was hit.
        /// </summary>
        public Vector Normal;

        /// <summary>
        /// Tests the trace of a moving point(line) with a static triangle. If they intersect, this will return
        /// true and set Result to the result of the trace. This will only check for intersections on the frontside face
        /// of the triangle.
        /// </summary>
        public static bool PointTriangleTrace(
            Vector A, Vector B, Vector C, 
            Vector Start, Vector Stop, 
            ref TraceHit Result)
        {
            Vector u = B - A;
            Vector v = C - A;
            Vector n = Vector.Cross(u, v);

            // Test intersection of segment and triangle plane.
            Vector raydir = Stop - Start;
            Vector rayw = Start - A;
            double a = -Vector.Dot(n, rayw);
            double b = Vector.Dot(n, raydir);
            double r = a / b;

            if (r >= 0.0 && r <= 1.0 && b < 0.0)
            {
                Result.Length = r;
                Result.Position = Start + (raydir * r);
                Result.Normal = n;

                // Check if point is in triangle.
                Vector w = Result.Position - A;
                double uu = Vector.Dot(u, u);
                double uv = Vector.Dot(u, v);
                double vv = Vector.Dot(v, v);
                double wu = Vector.Dot(w, u);
                double wv = Vector.Dot(w, v);
                double d = (uv * uv) - (uu * vv);
                double s = ((uv * wv) - (vv * wu)) / d;
                if (s >= 0.0 && s <= 1.0)
                {
                    double t = ((uv * wu) - (uu * wv)) / d;
                    if (t >= 0.0 && (s + t) <= 1.0)
                    {
                        // HIT!
                        return true;
                    }
                }
            }
            return false;
        }
    }

    /// <summary>
    /// A surface from a mesh.
    /// </summary>
    public class MeshSurface : DerivedMesh, ISurface
    {
        public MeshSurface(Mesh Base)
            : base(Base)
        {

        }

        public Bound Bound
        {
            get
            {
                List<Vector> vecs = new List<Vector>();
                foreach (Geometry tri in this.Triangles)
                {
                    Geometry[] points = Triangle.Points(tri);
                    foreach (Geometry point in points)
                    {
                        vecs.Add(Point.Position(point));
                    }
                }
                return new Bound(vecs);
            }
        }

        public bool TracePoint(Vector Start, Vector Stop, ref TraceHit Result)
        {
            Result.Length = double.NegativeInfinity;
            bool hashit = false;
            foreach (Geometry tri in this.Triangles)
            {
                Triangle.Data data = tri.GetData<Triangle.Data>();
                TraceHit hit = new TraceHit();

                // Test intersection
                if(TraceHit.PointTriangleTrace(
                    Point.Position(data.A),
                    Point.Position(data.B),
                    Point.Position(data.C),
                    Start, Stop,
                    ref hit) && Result.Length < hit.Length)
                {
                    Result = hit;
                    hashit = true;
                }
            }
            return hashit;
        }
    }
}
