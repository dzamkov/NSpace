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
    /// A bound of time and space.
    /// </summary>
    public struct Span : IBound<Span>, IIntersectTest<Span>, IIntersectTest<Bound>
    {
        public Span(Bound SpaceBound, TimeBound TimeBound)
        {
            this.SpaceBound = SpaceBound;
            this.TimeBound = TimeBound;
        }

        public bool Intersects(Span Bound)
        {
            return 
                this.TimeBound.Intersects(Bound.TimeBound) &&
                this.SpaceBound.Intersects(Bound.SpaceBound);
        }

        public bool Intersects(Bound Bound)
        {
            return this.SpaceBound.Intersects(Bound);
        }
        
        public Span Union(Span Other)
        {
            return new Span(
                this.SpaceBound.Union(Other.SpaceBound),
                this.TimeBound.Union(Other.TimeBound));
        }

        /// <summary>
        /// The space this span occupies over time.
        /// </summary>
        public Bound SpaceBound;

        /// <summary>
        /// The time period this span occupies.
        /// </summary>
        public TimeBound TimeBound;
    }
}
