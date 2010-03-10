//----------------------------------------
// Copyright (c) 2010, Dmitry Zamkov 
// Open source under the new BSD License
//----------------------------------------
using System;
using System.Collections.Generic;
using OpenTK;
using OpenTK.Graphics.OpenGL;

namespace NSpace
{
    /// <summary>
    /// Represents a sphere moving over time. This sphere can intersect and
    /// create unions with other spans.
    /// </summary>
    public struct Span : IBound<Span>, IIntersectTest<Span>
    {
        public Span(Vector PosStart, Vector PosEnd, double TimeStart, double TimeEnd, double Radius)
        {
            this.PosStart = PosStart;
            this.PosEnd = PosEnd;
            this.TimeStart = TimeStart;
            this.TimeEnd = TimeEnd;
            this.Radius = Radius;
        }

        public bool Intersects(Span Bound)
        {
            Span thiscut = this.Cut(Bound.TimeStart, Bound.TimeEnd);
            Span thatcut = Bound.Cut(this.TimeStart, this.TimeEnd);
            if (thiscut.TimeStart < thatcut.TimeEnd && thatcut.TimeStart < thiscut.TimeEnd)
            {
                // Compute line/line distance between equal-time cuts.
                // http://mathworld.wolfram.com/Line-LineDistance.html
                Vector a = thiscut.PosEnd - thiscut.PosStart;
                Vector b = thatcut.PosEnd - thatcut.PosStart;
                Vector c = thatcut.PosStart - thiscut.PosEnd;
                Vector ab = Vector.Cross(a, b);
                double d = Vector.Dot(c, ab) / ab.Length();
                return d <= thiscut.Radius + thatcut.Radius;
            }
            return false;
        }
        
        public Span Union(Span Other)
        {
            Span un = new Span();
            double rad;
            un.TimeStart = this.TimeStart < Other.TimeStart ? this.TimeStart : Other.TimeStart;
            un.TimeEnd = this.TimeEnd > Other.TimeEnd ? this.TimeEnd : Other.TimeEnd;
            _CircleUnion(this.PositionAtTime(un.TimeStart), this.Radius, Other.PositionAtTime(un.TimeStart), Other.Radius, out un.PosStart, out un.Radius);
            _CircleUnion(this.PositionAtTime(un.TimeEnd), this.Radius, Other.PositionAtTime(un.TimeEnd), Other.Radius, out un.PosEnd, out rad);
            un.Radius = rad > un.Radius ? rad : un.Radius;
            return un;
        }

        /// <summary>
        /// Gets the union of two circles defined by their center points and radius.
        /// </summary>
        private static void _CircleUnion(Vector CenterA, double RadiusA, Vector CenterB, double RadiusB, out Vector CenterC, out double RadiusC)
        {
            // Check if one circle is inside the other.
            double len = (CenterA - CenterB).Length();
            if (len + RadiusB < RadiusA)
            {
                CenterC = CenterA;
                RadiusC = RadiusB;
                return;
            }
            if (len + RadiusA < RadiusB)
            {
                CenterC = CenterB;
                RadiusC = RadiusB;
                return;
            }

            // Case when neither circle is a subset of the other.
            Vector dir = CenterB - CenterA;
            dir.Normalize();
            Vector afar = CenterA - dir * RadiusA;
            Vector bfar = CenterB + dir * RadiusB;
            CenterC = (afar + bfar) / 2.0;
            RadiusC = (afar - bfar).Length() / 2.0;
        }

        /// <summary>
        /// Returns the subsection span of this span with the specified start and stop times. The resulting
        /// span will be the intersection of this span and an infinitely large span between TimeStart and TimeEnd.
        /// </summary>
        public Span Cut(double TimeStart, double TimeEnd)
        {
            Span sp = new Span();
            sp.TimeStart = TimeStart > this.TimeStart ? TimeStart : this.TimeStart;
            sp.TimeEnd = TimeEnd < this.TimeEnd ? TimeEnd : this.TimeEnd;
            if (sp.TimeEnd >= sp.TimeStart)
            {
                sp.PosStart = this.PositionAtTime(sp.TimeStart);
                sp.PosEnd = this.PositionAtTime(sp.TimeEnd);
                sp.Radius = this.Radius;
                return sp;
            }
            else
            {
                return None;
            }
        }

        /// <summary>
        /// Gets a span that contains no area and doesnt intersect anything.
        /// </summary>
        public static Span None
        {
            get
            {
                double inf = double.PositiveInfinity;
                double neginf = double.NegativeInfinity;
                return new Span(new Vector(), new Vector(), inf, neginf, neginf);
            }
        }

        /// <summary>
        /// Gets the position of the center of the sphere at the specified time.
        /// </summary>
        public Vector PositionAtTime(double Time)
        {
            double reltime = (Time - TimeStart) / (TimeEnd - TimeStart);
            return (this.PosStart * (1.0 - reltime)) + (this.PosEnd * reltime);
        }

        public Vector PosStart;
        public Vector PosEnd;
        public double Radius;
        public double TimeStart;
        public double TimeEnd;
    }
}
