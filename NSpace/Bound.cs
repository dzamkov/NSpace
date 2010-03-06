//----------------------------------------
// Copyright (c) 2010, Dmitry Zamkov 
// Open source under the new BSD License
//----------------------------------------
using System;
using System.Collections.Generic;

namespace NSpace
{
    /// <summary>
    /// A three-dimensional orthographical rectangular volume that specifies the location where
    /// items may be contained.
    /// </summary>
    public struct Bound : IBound<Bound>
    {
        public Bound(Vector Min, Vector Max)
        {
            this.Min = Min;
            this.Max = Max;
        }

        public Bound(IEnumerable<Vector> Points)
        {
            Bound none = None;
            this.Min = none.Min;
            this.Max = none.Max;
            foreach (Vector v in Points)
            {
                this.AddVector(v);
            }
        }

        public Bound(Bound Source)
        {
            this.Min = Source.Min;
            this.Max = Source.Max;
        }

        /// <summary>
        /// Minimum value for the bounds. The x, y and z values in this should specify
        /// the corner of the bounds with the lowest values.
        /// </summary>
        public Vector Min;

        /// <summary>
        /// Maximum value for the bounds. The x, y and z values in this should specify
        /// the corner of the bounds with the highest values.
        /// </summary>
        public Vector Max;

        /// <summary>
        /// Gets the volume of the bound in cubic units.
        /// </summary>
        public double Volume
        {
            get
            {
                return
                    (this.Max.X - this.Min.X) *
                    (this.Max.Y - this.Min.Y) *
                    (this.Max.Z - this.Min.Z);
            }
        }

        /// <summary>
        /// Adds a vector to be bounded by this bounds. This will not have an
        /// effect if the vector is already in the bounds.
        /// </summary>
        public void AddVector(Vector Vector)
        {
            this.Min.X = Vector.X < this.Min.X ? Vector.X : this.Min.X;
            this.Min.Y = Vector.Y < this.Min.Y ? Vector.Y : this.Min.Y;
            this.Min.Z = Vector.Z < this.Min.Z ? Vector.Z : this.Min.Z;
            this.Max.X = Vector.X > this.Max.X ? Vector.X : this.Max.X;
            this.Max.Y = Vector.Y > this.Max.Y ? Vector.Y : this.Max.Y;
            this.Max.Z = Vector.Z > this.Max.Z ? Vector.Z : this.Max.Z;
        }

        /// <summary>
        /// Transforms a bound by a matrix. The new bound will be in the coordinate space
        /// specified by the matrix and will contain everything within the old bound. This may
        /// cause the bound to grow larger then its initial area.
        /// </summary>
        public static Bound Transform(Matrix A, Bound B)
        {
            return new Bound(new Vector[] {
                A * new Vector(B.Min.X, B.Min.Y, B.Min.Z),
                A * new Vector(B.Min.X, B.Min.Y, B.Max.Z),
                A * new Vector(B.Min.X, B.Max.Y, B.Min.Z),
                A * new Vector(B.Min.X, B.Max.Y, B.Max.Z),
                A * new Vector(B.Max.X, B.Min.Y, B.Min.Z),
                A * new Vector(B.Max.X, B.Min.Y, B.Max.Z),
                A * new Vector(B.Max.X, B.Max.Y, B.Min.Z),
                A * new Vector(B.Max.X, B.Max.Y, B.Max.Z),
            });
        }

        /// <summary>
        /// Gets the union between two bounds, which encompasses both completely.
        /// </summary>
        public static Bound Union(Bound A, Bound B)
        {
            return new Bound(
                new Vector(
                    A.Min.X < B.Min.X ? A.Min.X : B.Min.X,
                    A.Min.Y < B.Min.Y ? A.Min.Y : B.Min.Y,
                    A.Min.Z < B.Min.Z ? A.Min.Z : B.Min.Z),
                new Vector(
                    A.Max.X > B.Max.X ? A.Max.X : B.Max.X,
                    A.Max.Y > B.Max.Y ? A.Max.Y : B.Max.Y,
                    A.Max.Z > B.Max.Z ? A.Max.Z : B.Max.Z));
        }

        /// <summary>
        /// Gets if the two bounds intersect.
        /// </summary>
        public static bool Intersects(Bound A, Bound B)
        {
            return
                A.Max.X > B.Min.X && A.Min.X < B.Max.X &&
                A.Max.Y > B.Min.Y && A.Min.Y < B.Max.Y &&
                A.Max.Z > B.Min.Z && A.Min.Z < B.Max.Z;
        }

        /// <summary>
        /// Gets a bound that encompasses everything in all coordinate spaces.
        /// </summary>
        public static Bound Huge
        {
            get
            {
                double inf = double.PositiveInfinity;
                double ninf = double.NegativeInfinity;
                return new Bound(
                    new Vector(ninf, ninf, ninf),
                    new Vector(inf, inf, inf));
            }
        }

        public Bound Union(Bound Other)
        {
            return Bound.Union(this, Other);
        }

        /// <summary>
        /// Gets a bound that contains no volume in it.
        /// </summary>
        public static Bound None
        {
            get
            {
                double inf = double.PositiveInfinity;
                double ninf = double.NegativeInfinity;
                return new Bound(
                    new Vector(inf, inf, inf),
                    new Vector(ninf, ninf, ninf));
            }
        }
    }

    /// <summary>
    /// Intersect test to see if two bounds intersect.
    /// </summary>
    public struct BoundIntersectTest : IIntersectTest<Bound>
    {
        public BoundIntersectTest(Bound Bound)
        {
            this._Bound = Bound;
        }

        public bool Intersects(Bound Bound)
        {
            return Bound.Intersects(this._Bound, Bound);
        }

        private Bound _Bound;
    }

    /// <summary>
    /// A type of bound tree for bounds that tries to minimize volume.
    /// </summary>
    public class VolumeBoundTree<O> : BoundTree<Bound, O>
        where O : class
    {
        public override double GetScore(Bound Bound)
        {
            return Bound.Volume;
        }
    }
}
