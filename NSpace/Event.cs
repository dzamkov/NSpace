//----------------------------------------
// Copyright (c) 2010, Dmitry Zamkov 
// Open source under the new BSD License
//----------------------------------------
using System;
using System.Collections.Generic;

namespace NSpace
{
    /// <summary>
    /// A vector paired with a time to represent a point in spacetime.
    /// </summary>
    public struct Event
    {
        public Event(Vector Point, Time Time)
        {
            this.Point = Point;
            this.Time = Time;
        }

        public Vector Point;
        public Time Time;
    }
}
