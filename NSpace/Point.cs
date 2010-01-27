//----------------------------------------
// Copyright (c) 2010, Dmitry Zamkov 
// Open source under the new BSD License
//----------------------------------------
using OpenTK;

namespace NSpace
{
    /// <summary>
    /// A single point in a mesh that represents a location relative to other
    /// points in the same mesh. A single point can be used multiple times within
    /// a mesh by multiple triangles. Points may contain additional information in
    /// a derived class.
    /// </summary>
    public class Point
    {
        /// <summary>
        /// Sets the x, y and z unit vectors of the point.
        /// </summary>
        public void SetPoint(double X, double Y, double Z)
        {
            this.X = X;
            this.Y = Y;
            this.Z = Z;
        }

        /// <summary>
        /// Converts a point into a vector usable by OpenTK.
        /// </summary>
        public static implicit operator Vector3d(Point Point)
        {
            return new Vector3d(Point.X, Point.Y, Point.Z);
        }

        /// <summary>
        /// Converts a point into a less precise vector usable by
        /// OpenTK.
        /// </summary>
        public static implicit operator Vector3(Point Point)
        {
            return new Vector3(
                (float)Point.X,
                (float)Point.Y,
                (float)Point.Z);
        }

        /// <summary>
        /// Mixes this point with another by the specified amount. If the amount is
        /// 0.0, this point will remain unchanged; if the amount is 1.0, the point
        /// will take the state of the other point. Values between the two are
        /// interpolated.
        /// </summary>
        public virtual void Mix(Point Other, double Amount)
        {
            double xd = Other.X - this.X;
            double yd = Other.Y - this.Y;
            double zd = Other.Z - this.Z;
            this.X += xd * Amount;
            this.Y += yd * Amount;
            this.Z += zd * Amount;
        }

        /// <summary>
        /// Creates a copy of this point.
        /// </summary>
        public virtual Point Copy()
        {
            Point p = new Point();
            this.Clone(p);
            return p;
        }

        /// <summary>
        /// Clones the state of this point into the state of the
        /// other specified point.
        /// </summary>
        protected virtual void Clone(Point Point)
        {
            Point.X = this.X;
            Point.Y = this.Y;
            Point.Z = this.Z;
        }

        public double X;
        public double Y;
        public double Z;
    }
}
