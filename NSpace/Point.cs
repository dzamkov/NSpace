//----------------------------------------
// Copyright (c) 2010, Dmitry Zamkov 
// Open source under the new BSD License
//----------------------------------------
using System;
using System.Collections.Generic;

namespace NSpace
{
    /// <summary>
    /// A position relative to a shape.
    /// </summary>
    public class Point : IShape
    {
        public Point()
        {

        }

        public Point(Vector Position)
        {
            this.Position = Position;
        }

        public Vector Position;

        public Bound Bound
        {
            get
            {
                return new Bound(this.Position, this.Position);
            }
        }
    }
}
