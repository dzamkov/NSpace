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
    /// Methods and classes for the manipulation of points.
    /// </summary>
    public static class Point
    {
        /// <summary>
        /// A type of data within a point that can be mixed with other data of the same
        /// type.
        /// </summary>
        public interface IMixable : IData
        {
            /// <summary>
            /// Mixes this data with the other data by the specified
            /// amount. If Amount is 0.0, no change is made. If Amount
            /// is 1.0, this data will become equivalent to Other.
            /// </summary>
            void Mix(IData Other, double Amount);
        }

        /// <summary>
        /// A single point in a mesh that represents a location relative to other
        /// points in the same mesh. A single point can be used multiple times within
        /// a mesh by multiple triangles.
        /// </summary>
        public class Data : IData, IMixable
        {
            public Vector Position;

            public IData Copy()
            {
                return new Data() { Position = this.Position };
            }

            public void Mix(IData Other, double Amount)
            {
                Data l = (Data)Other;
                this.Position += (l.Position - this.Position) * Amount;
            }

            /// <summary>
            /// Gets the point data from the geometry.
            /// </summary>
            public static explicit operator Data(Geometry Geom)
            {
                return Geom.GetData<Data>();
            }
        }

        /// <summary>
        /// Color data for a point.
        /// </summary>
        public class ColorData : IData, IMixable
        {
            public Color Color;

            public IData Copy()
            {
                return new ColorData() { Color = this.Color };
            }

            public void Mix(IData Other, double Amount)
            {
                ColorData cd = (ColorData)Other;
                this.Color = Color.Mix(this.Color, cd.Color, Amount);
            }
        }

        /// <summary>
        /// UV, or texture mapping data for a point.
        /// </summary>
        public class UVData : IData, IMixable
        {
            public double U;
            public double V;

            public IData Copy()
            {
                return new UVData() { U = this.U, V = this.V };
            }

            public void Mix(IData Other, double Amount)
            {
                UVData ud = (UVData)Other;
                this.U += (ud.U - this.U) * Amount;
                this.V += (ud.V - this.V) * Amount;
            }
        }

        /// <summary>
        /// Mixes a point with another by the specified amount. If the amount is
        /// 0.0, the point will remain unchanged; if the amount is 1.0, the point
        /// will take the state of the other point. Values between the two are
        /// interpolated.
        /// </summary>
        public static void Mix(Geometry A, Geometry B, double Amount)
        {
            List<KeyValuePair<Type, IData>> changed = new List<KeyValuePair<Type, IData>>();
            foreach (KeyValuePair<Type, IData> kvp in A.FullData)
            {
                IData o = B.GetData(kvp.Key);
                IMixable l = kvp.Value as IMixable;
                l.Mix(o, Amount);
                changed.Add(new KeyValuePair<Type, IData>(kvp.Key, l));
            }
            foreach (KeyValuePair<Type, IData> kvp in changed)
            {
                A.SetData(kvp.Key, kvp.Value);
            }
        }

        /// <summary>
        /// Creates a new point with only position data.
        /// </summary>
        public static Geometry Create(Vector Position)
        {
            ElasticGeometry geom = new ElasticGeometry(null);
            geom.SetData<Data>(new Data() { Position = Position });
            return geom;
        }

        /// <summary>
        /// Creates a new point with UV data.
        /// </summary>
        public static Geometry Create(Vector Position, double U, double V)
        {
            Geometry geom = Create(Position);
            geom.SetData<UVData>(new UVData() { U = U, V = V });
            return geom;
        }

        /// <summary>
        /// Creates a new point with color data.
        /// </summary>
        public static Geometry Create(Vector Position, Color Color)
        {
            Geometry geom = Create(Position);
            geom.SetData<ColorData>(new ColorData() { Color = Color });
            return geom;
        }

        
    }

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
