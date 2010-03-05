//----------------------------------------
// Copyright (c) 2010, Dmitry Zamkov 
// Open source under the new BSD License
//----------------------------------------
using System;
using System.Collections.Generic;

namespace NSpace
{
    /// <summary>
    /// A shape that can be used for traces or checking for collisions.
    /// </summary>
    public interface ICollisionShape : IShape
    {
        /// <summary>
        /// Performs a trace on the mesh. A trace is a test that gives the
        /// triangles in order, that are hit by the specified line.
        /// </summary>
        IEnumerable<TraceHit> Trace(Vector Start, Vector Stop);
    }

    /// <summary>
    /// Information given when the frontside of a surface is hit with a trace line.
    /// </summary>
    public struct TraceHit
    {
        /// <summary>
        /// The relative amount along the line where the hit was at. A value of 0.0 indicates
        /// the hit was at start and a value at 1.0 indicates the hit was at stop.
        /// </summary>
        public double Length;

        /// <summary>
        /// The position at which the surface was hit.
        /// </summary>
        public Vector Position;

        /// <summary>
        /// The normal of the surface hit.
        /// </summary>
        public Vector Normal;

        /// <summary>
        /// Tests the intersection between a triangle with a line. If they intersect, this will return
        /// true and set Length, Normal and Position to the correct values. If there is no intersection, this will return
        /// false and possibly alter some ref values. This can only test the frontside of a triangle.
        /// </summary>
        public static bool IntersectTriangle(
            Vector A, Vector B, Vector C, 
            Vector Start, Vector Stop, 
            ref double Length, 
            ref Vector Position, 
            ref Vector Normal)
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
                Length = r;
                Normal = n;
                Position = Start + (raydir * r);

                // Check if point is in triangle.
                Vector w = Position - A;
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
}
