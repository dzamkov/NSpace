//----------------------------------------
// Copyright (c) 2010, Dmitry Zamkov 
// Open source under the new BSD License
//----------------------------------------
using System;
using System.Collections.Generic;
using OpenTK;

namespace NSpace
{
    /// <summary>
    /// Represents a vector in three dimensional space.
    /// </summary>
    public struct Vector
    {
        public Vector(double X, double Y, double Z)
        {
            this.X = X;
            this.Y = Y;
            this.Z = Z;
        }

        public Vector(Vector Source)
        {
            this.X = Source.X;
            this.Y = Source.Y;
            this.Z = Source.Z;
        }

        public double X;
        public double Y;
        public double Z;

        /// <summary>
        /// Converts a vector into a vector usable by OpenTK.
        /// </summary>
        public static implicit operator Vector3d(Vector Vector)
        {
            return new Vector3d(Vector.X, Vector.Y, Vector.Z);
        }

        /// <summary>
        /// Converts a vector into a less precise vector usable by
        /// OpenTK.
        /// </summary>
        public static implicit operator Vector3(Vector Vector)
        {
            return new Vector3(
                (float)Vector.X,
                (float)Vector.Y,
                (float)Vector.Z);
        }

        /// <summary>
        /// Computes the cross product between two vectors, with the resulting
        /// vector being the normal.
        /// </summary>
        public static Vector Cross(Vector A, Vector B)
        {
            return new Vector(
                (A.Y * B.Z) - (A.Z * B.Y),
                (A.Z * B.X) - (A.X * B.Z),
                (A.X * B.Y) - (A.Y * B.X));
        }

        /// <summary>
        /// Computes the dot product between two vectors.
        /// </summary>
        public static double Dot(Vector A, Vector B)
        {
            return (A.X * B.X) + (A.Y * B.Y) + (A.Z * B.Z);
        }

        /// <summary>
        /// Gets the length of this vector, its distance from the origin.
        /// </summary>
        public double Length()
        {
            return Math.Sqrt((this.X * this.X) + (this.Y * this.Y) + (this.Z * this.Z));
        }

        /// <summary>
        /// Normalizes this vector, making its length one without changing its
        /// direction.
        /// </summary>
        public void Normalize()
        {
            this.Divide(this.Length());
        }

        /// <summary>
        /// Subtracts another vector from this vector.
        /// </summary>
        public void Subtract(Vector Other)
        {
            this.X -= Other.X;
            this.Y -= Other.Y;
            this.Z -= Other.Z;
        }

        /// <summary>
        /// Adds another vector to this vector.
        /// </summary>
        public void Add(Vector Other)
        {
            this.X += Other.X;
            this.Y += Other.Y;
            this.Z += Other.Z;
        }

        /// <summary>
        /// Multiplies this vector by a scalar.
        /// </summary>
        public void Multiply(double Scalar)
        {
            this.X *= Scalar;
            this.Y *= Scalar;
            this.Z *= Scalar;
        }

        /// <summary>
        /// Divides this vector by a scalar.
        /// </summary>
        public void Divide(double Scalar)
        {
            this.X /= Scalar;
            this.Y /= Scalar;
            this.Z /= Scalar;
        }

        public static Vector operator -(Vector A, Vector B)
        {
            Vector c = new Vector(A);
            c.Subtract(B);
            return c;
        }

        public static Vector operator +(Vector A, Vector B)
        {
            Vector c = new Vector(A);
            c.Add(B);
            return c;
        }

        public static Vector operator *(Vector A, double Scalar)
        {
            Vector c = new Vector(A);
            c.Multiply(Scalar);
            return c;
        }

        public static Vector operator /(Vector A, double Scalar)
        {
            Vector c = new Vector(A);
            c.Divide(Scalar);
            return c;
        }
    }
}
