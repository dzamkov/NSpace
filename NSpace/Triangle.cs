//----------------------------------------
// Copyright (c) 2010, Dmitry Zamkov 
// Open source under the new BSD License
//----------------------------------------
using System;
using System.Collections.Generic;
using System.Linq;

namespace NSpace
{
    /// <summary>
    /// A (hopefully) three-pointed triangular triangle-shaped object.
    /// </summary>
    public class Triangle : IShape
    {
        public Point A;
        public Point B;
        public Point C;

        /// <summary>
        /// Gets or sets the array of the points that make up this triangle.
        /// </summary>
        public Point[] Points
        {
            get
            {
                return new Point[]
                { 
                    this.A, this.B, this.C
                };
            }
            set
            {
                this.A = value[0];
                this.B = value[1];
                this.C = value[2];
            }
        }

        /// <summary>
        /// Gets or sets the point in this triangle at the specified index.
        /// </summary>
        public Point this[int Index]
        {
            get
            {
                switch (Index)
                {
                    case 0: return this.A;
                    case 1: return this.B;
                    case 2: return this.C;
                }
                return null;
            }
            set
            {
                switch (Index)
                {
                    case 0: this.A = value; break;
                    case 1: this.B = value; break;
                    case 2: this.C = value; break;
                }
            }
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
                Vector pa = this.A.Position;
                Vector pb = this.B.Position;
                Vector pc = this.C.Position;
                Vector a = pb - pa;
                Vector b = pc - pa;
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
                return (this.A.Position + this.B.Position + this.C.Position) / 3.0;
            }
        }

        public Bound Bound
        {
            get 
            {
                return new Bound(
                    new Vector[] 
                    {
                        this.A.Position,
                        this.B.Position,
                        this.C.Position
                    });
            }
        }
    }
}
