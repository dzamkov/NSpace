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
        public Time(double Amount)
        {
            this.Amount = Amount;
        }

        public static TimeSpan operator -(Time A, Time B)
        {
            return new TimeSpan(A.Amount - B.Amount);
        }

        public static Time operator +(Time A, TimeSpan B)
        {
            return new Time(A.Amount + B.Amount);
        }

        public static bool operator >(Time A, Time B)
        {
            return A.Amount > B.Amount;
        }

        public static bool operator <(Time A, Time B)
        {
            return A.Amount < B.Amount;
        }

        public static bool operator ==(Time A, Time B)
        {
            return A.Amount == B.Amount;
        }

        public static bool operator !=(Time A, Time B)
        {
            return A.Amount != B.Amount;
        }

        public override int GetHashCode()
        {
            return this.Amount.GetHashCode();
        }

        public override bool Equals(object obj)
        {
            Time? t = obj as Time?;
            if (t != null && t.Value.Amount == this.Amount)
            {
                return true;
            }
            else
            {
                return false;
            }
        }

        public bool Intersects(TimeBound Bound)
        {
            return this.Amount > Bound.TimeStart.Amount && this.Amount < Bound.TimeEnd.Amount;
        }

        /// <summary>
        /// An amount or value that represents this time in a section. This value only makes sense
        /// in terms of a section.
        /// </summary>
        public double Amount;
    }

    /// <summary>
    /// Represents a difference between times or a length of time.
    /// </summary>
    public struct TimeSpan
    {
        public TimeSpan(double Amount)
        {
            this.Amount = Amount;
        }

        public static bool operator ==(TimeSpan A, TimeSpan B)
        {
            return A.Amount == B.Amount;
        }

        public static bool operator !=(TimeSpan A, TimeSpan B)
        {
            return A.Amount != B.Amount;
        }

        public override int GetHashCode()
        {
            return this.Amount.GetHashCode();
        }

        public override bool Equals(object obj)
        {
            TimeSpan? t = obj as TimeSpan?;
            if (t != null && t.Value.Amount == this.Amount)
            {
                return true;
            }
            else
            {
                return false;
            }
        }

        /// <summary>
        /// An amount or value that represents this time in a section. This value only makes sense
        /// in terms of a section.
        /// </summary>
        public double Amount;
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

        public TimeBound(double SecondsTimeStart, double SecondsTimeEnd)
        {
            this.TimeStart = new Time(SecondsTimeStart);
            this.TimeEnd = new Time(SecondsTimeEnd);
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

        /// <summary>
        /// Gets the distance across the bound a certain time is with 0.0 indicating the time
        /// is at the start of the bound and 1.0 indicating the time is at the end of the bound.
        /// </summary>
        public double BoundRelation(Time Time)
        {
            return (Time.Amount - TimeStart.Amount) / (TimeEnd - TimeStart).Amount;
        }

        public Time TimeStart;
        public Time TimeEnd;
    }

    /// <summary>
    /// Scorer for time bounds that tries to minimize the size, or period of the time
    /// bounds.
    /// </summary>
    public struct TimeBoundScorer : IBoundScorer<TimeBound>
    {
        public double GetScore(TimeBound Bound)
        {
            // Minimize timebound size
            return Bound.Size.Amount;
        }
    }
}
