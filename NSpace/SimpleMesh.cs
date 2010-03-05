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
    /// A mesh with raw triangle and point data stored in an array.
    /// </summary>
    public class SimpleMesh : IMesh, ICollisionShape
    {
        public SimpleMesh(int[] Indices, Vector[] Vertices)
        {
            this._Indices = Indices;
            this._Vertices = Vertices;
            this._Bound = new Bound(Vertices);
        }

        public IEnumerable<TraceHit> Trace(Vector Start, Vector Stop)
        {
            LinkedList<TraceHit> res = new LinkedList<TraceHit>();
            for (int t = 0; t < this._Indices.Length; t += 3)
            {
                double len = 0.0;
                Vector norm = new Vector();
                Vector pos = new Vector();

                // Test intersection
                if (TraceHit.IntersectTriangle(
                    this._Vertices[this._Indices[t + 0]],
                    this._Vertices[this._Indices[t + 1]],
                    this._Vertices[this._Indices[t + 2]],
                    Start, Stop,
                    ref len, ref pos, ref norm))
                {
                    // Add as hit in correct spot along list
                    TraceHit th = new TraceHit();
                    th.Length = len;
                    th.Position = pos;
                    th.Normal = norm;

                    LinkedListNode<TraceHit> node = res.First;
                    while (true)
                    {
                        if (node != null)
                        {
                            if (node.Value.Length > len)
                            {
                                res.AddBefore(node, th);
                                break;
                            }
                            node = node.Next;
                        }
                        else
                        {
                            res.AddLast(th);
                            break;
                        }
                    }
                }
            }
            return res;
        }

        public Bound Bound
        {
            get 
            { 
                return this._Bound;
            }
        }

        public IEnumerable<Triangle> Triangles
        {
            get 
            {
                Dictionary<int, Point> points = new Dictionary<int, Point>();
                for (int t = 0; t < this._Indices.Length; t += 3)
                {
                    Triangle tri = new Triangle();
                    Point[] tripoints = new Point[3];
                    for (int l = 0; l < 3; l++)
                    {
                        int pointindex = this._Indices[t + l];
                        Point p;
                        if (!points.TryGetValue(pointindex, out p))
                        {
                            p = new Point(this._Vertices[pointindex]);
                            points[pointindex] = p;
                        }
                        tripoints[l] = p;
                    }
                    tri.Points = tripoints;
                    yield return tri;
                }
            }
        }

        public int TriangleCount
        {
            get 
            {
                return this._Indices.Length / 3;
            }
        }

        private int[] _Indices;
        private Vector[] _Vertices;
        private Bound _Bound;
    }
}
