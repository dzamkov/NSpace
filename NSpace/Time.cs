//----------------------------------------
// Copyright (c) 2010, Dmitry Zamkov 
// Open source under the new BSD License
//----------------------------------------
using System;
using System.Collections.Generic;

namespace NSpace
{
    /// <summary>
    /// Represents a time within a section.
    /// </summary>
    public struct Time
    {
        public Time(double Amount)
        {
            this.Amount = Amount;
        }

        public static Time operator -(Time A, Time B)
        {
            return new Time(A.Amount - B.Amount);
        }

        public static Time operator +(Time A, Time B)
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

        /// <summary>
        /// An amount or value that represents this time in a section. This value only makes sense
        /// in terms of a section.
        /// </summary>
        public double Amount;
    }
}
