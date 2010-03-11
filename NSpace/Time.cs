//----------------------------------------
// Copyright (c) 2010, Dmitry Zamkov 
// Open source under the new BSD License
//----------------------------------------
using System;
using System.Collections.Generic;

namespace NSpace
{
    /// <summary>
    /// Represents a time.
    /// </summary>
    public struct Time : IIntersectTest<TimeBound>
    {
        public Time(double Seconds)
        {
            this.Seconds = Seconds;
        }

        public static TimeSpan operator -(Time A, Time B)
        {
            return new TimeSpan(A.Seconds - B.Seconds);
        }

        public static Time operator +(Time A, TimeSpan B)
        {
            return new Time(A.Seconds + B.Seconds);
        }

        public static bool operator >(Time A, Time B)
        {
            return A.Seconds > B.Seconds;
        }

        public static bool operator <(Time A, Time B)
        {
            return A.Seconds < B.Seconds;
        }

        public bool Intersects(TimeBound Bound)
        {
            return this.Seconds > Bound.TimeStart.Seconds && this.Seconds < Bound.TimeEnd.Seconds;
        }

        public double Seconds;
    }

    /// <summary>
    /// Represents a difference between times.
    /// </summary>
    public struct TimeSpan
    {
        public TimeSpan(double Seconds)
        {
            this.Seconds = Seconds;
        }

        public double Seconds;
    }

    /// <summary>
    /// A definite period of time.
    /// </summary>
    public struct TimeBound : IBound<TimeBound>, IIntersectTest<TimeBound>
    {
        public TimeBound(Time TimeStart, Time TimeEnd)
        {
            this.TimeStart = TimeStart;
            this.TimeEnd = TimeEnd;
        }

        /// <summary>
        /// Gets the size of the time bound.
        /// </summary>
        public TimeSpan Size
        {
            get
            {
                return this.TimeEnd - this.TimeStart;
            }
        }

        public TimeBound Union(TimeBound Other)
        {
            return new TimeBound(
                this.TimeStart < Other.TimeStart ? this.TimeStart : Other.TimeStart,
                this.TimeEnd > Other.TimeEnd ? this.TimeEnd : Other.TimeEnd);
        }

        public bool Intersects(TimeBound Bound)
        {
            return this.TimeStart < Bound.TimeEnd && this.TimeEnd > Bound.TimeStart;
        }

        public static TimeBound Huge
        {
            get
            {
                return new TimeBound(new Time(double.NegativeInfinity), new Time(double.PositiveInfinity));
            }
        }

        public static TimeBound None
        {
            get
            {
                return new TimeBound(new Time(double.PositiveInfinity), new Time(double.NegativeInfinity));
            }
        }

        public Time TimeStart;
        public Time TimeEnd;
    }

    /// <summary>
    /// Bound tree that organizes objects by time.
    /// </summary>
    public class TimeBoundTree<O> : BoundTree<TimeBound, O>
        where O : class
    {
        public override double GetScore(TimeBound Bound)
        {
            // Minimize timebound size
            return Bound.Size.Seconds;
        }
    }
}
