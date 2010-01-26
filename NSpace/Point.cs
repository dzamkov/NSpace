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
    /// a mesh by multiple triangles and may contain additional data.
    /// </summary>
    public class Point
    {
        internal Point()
        {

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

        public double X;
        public double Y;
        public double Z;
    }

    /// <summary>
    /// A point that contains additional data with an unspecified purpose.
    /// </summary>
    /// <typeparam name="T">The type of additional data to store within
    /// the point.</typeparam>
    public class Point<T> : Point where T : struct
    {

        /// <summary>
        /// The additional data contained in this point.
        /// </summary>
        public T Data;
    }
}
