//----------------------------------------
// Copyright (c) 2010, Dmitry Zamkov 
// Open source under the new BSD License
//----------------------------------------
using System.Collections.Generic;

namespace NSpace
{
    /// <summary>
    /// Connection between 3 points that forms a face. The ordering of the points determines
    /// which side of the triangle is up and visible. When looked at with points in clockwise
    /// order, the up side of the triangle is directed towards the viewer. Triangles may
    /// contain additional data that can be used to manipulate them and change the way
    /// they are viewed.
    /// </summary>
    public class Triangle
    {
        /// <summary>
        /// Gets an enumerator for all the points in this triangle.
        /// </summary>
        public IEnumerable<Point> Points
        {
            get
            {
                return new Point[] { A, B, C };
            }
        }

        public Point A;
        public Point B;
        public Point C;
    }
}
