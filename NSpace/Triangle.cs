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
    /// they are viewed in a derived class.
    /// </summary>
    public class Triangle
    {
        public Triangle()
        {
            this.Points = new Point[3];
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
                Vector a = this.Points[1].Position - this.Points[0].Position;
                Vector b = this.Points[2].Position - this.Points[0].Position;
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
                return (this.Points[0].Position + this.Points[1].Position + this.Points[2].Position) / 3.0;
            }
        }

        public Point[] Points;
    }
}
