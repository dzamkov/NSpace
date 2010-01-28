//----------------------------------------
// Copyright (c) 2010, Dmitry Zamkov 
// Open source under the new BSD License
//----------------------------------------
using System;
using System.Collections.Generic;

namespace NSpace
{
    /// <summary>
    /// Contains methods used to manipulate and interact with a terrain.
    /// </summary>
    public static class Terrain
    {

        static Terrain()
        {
            PRNG = new Random();
        }

        /// <summary>
        /// Creates a terrain and outputs it to the target. The resulting terrain will
        /// be on an equilateral triangle.
        /// </summary>
        public static void Create(ISink<Triangle> TriangleOutput, ISink<Point> PointOutput)
        {
            int levels = 6;
            double mag = 2.0;
            double magl = 2.0;
            double magc = 1.1;
            ISource<Triangle> trisource = null;
            for(int t = 0; t < levels; t++)
            {
                ISource<Triangle> nexttrisource = null;
                ISink<Triangle> trisink;
                if (t + 1 == levels)
                {
                    trisink = TriangleOutput;
                }
                else
                {
                    SinkSource<Triangle> ss = new SinkSource<Triangle>();
                    trisink = ss;
                    nexttrisource = ss;
                }

                if (t == 0)
                {
                    _Init(trisink, PointOutput, mag);
                }
                else
                {
                    _Step(trisink, PointOutput, trisource, mag);
                }
                trisource = nexttrisource;
                mag /= magl;
                magl *= magc;
            }
        }

        /// <summary>
        /// Performs a step of the subdivision needed to create a terrain. This will take in some
        /// triangles of the previous level and create some triangles for the next level while adding
        /// noise to the newly created points.
        /// </summary>
        private static void _Step(
            ISink<Triangle> TriSink, ISink<Point> PointSink,
            ISource<Triangle> TriSource, double Magnitude)
        {
            SinkSource<Point> interpoint = new SinkSource<Point>();
            foreach (Triangle tri in TriSource.Items)
            {
                DivTriangle div = tri as DivTriangle;
                if (tri != null)
                {
                    div.Split(TriSink, interpoint);
                }
            }
            foreach (Point p in interpoint.Items)
            {
                _AddNoise(p, Magnitude);
                if (PointSink != null)
                {
                    PointSink.Add(p);
                }
            }
        }

        /// <summary>
        /// Initializes the first step, or level of the terrian and outputs points and triangles.
        /// </summary>
        private static void _Init(ISink<Triangle> TriSink, ISink<Point> PointSink, double Magnitude)
        {
            ColoredPoint[] points = new ColoredPoint[3];
            for (int x = 0; x < 3; x++)
            {
                double ang = (double)x * Math.PI * 2.0 / 3.0;
                points[x] = new ColoredPoint();
                points[x].SetPoint(Math.Sin(ang), 0.0, Math.Cos(ang));
                points[x].Color = Color.HLSA(ang / Math.PI * 180.0, 0.5, 1.0, 1.0);
                _AddNoise(points[x], Magnitude);
            }
            if (PointSink != null)
            {
                PointSink.Add(points);
            }


            DivTriangle tri = new DivTriangle();
            for (int t = 0; t < 3; t++)
            {
                tri.Points[t] = points[t];
            }
            TriSink.Add(tri);
        }

        /// <summary>
        /// Adds some noise to the point of the specified magnitude. Magnitude should be
        /// C / (4.0 ^ L) where ^ is exponentation, C is a constant, and L is the level
        /// depth at which the point appears.
        /// </summary>
        private static void _AddNoise(Point Point, double Magnitude)
        {
            Point.Y += Magnitude * (PRNG.NextDouble() - 0.5);
        }

        private static Random PRNG;
    }
}
