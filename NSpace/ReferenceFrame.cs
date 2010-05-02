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

    /// <summary>
    /// A "frame of reference" for which time and position measurements are for. A frame of reference
    /// is given a definition in terms of another reference frame.
    /// </summary>
    public class ReferenceFrame
    {
        public ReferenceFrame(ReferenceFrame Parent, IFrameRelation ParentRelation)
        {
            this._Parent = Parent;
            this._ParentRelation = ParentRelation;
        }

        /// <summary>
        /// The reference frame that is used to define this frame. If this is null, this frame of
        /// reference is absolute an not in terms of any other.
        /// </summary>
        public ReferenceFrame Parent
        {
            get
            {
                return this._Parent;
            }
        }

        /// <summary>
        /// Gets the relation from this frame to its parent.
        /// </summary>
        public IFrameRelation ParentRelation
        {
            get
            {
                return this._ParentRelation;
            }
        }

        /// <summary>
        /// The amount of levels down in the tree of reference frames this is away from an
        /// absolute frame of reference.
        /// </summary>
        public int Level
        {
            get
            {
                if (this._Parent == null)
                {
                    return 0;
                }
                else
                {
                    return 1 + this._Parent.Level;
                }
            }
        }

        /// <summary>
        /// Gets a relation one frame of reference has to another. Null is returned if
        /// there is no way to convert the frames of reference, they belong to different
        /// absolute frames.
        /// </summary>
        public IFrameRelation GetRelation(ReferenceFrame Other)
        {
            if (this.Level == Other.Level)
            {
                if (this == Other)
                {
                    return new IdentityFrameRelation();
                }
                else
                {
                    if (this._Parent != null)
                    {
                        return new CompoundFrameRelation(this._ParentRelation, 
                            new CompoundFrameRelation(
                                this._Parent.GetRelation(Other._Parent),
                                Other._ParentRelation.Inverse)); 
                    }
                    else
                    {
                        return null;
                    }
                }
            }
            if (this.Level > Other.Level)
            {
                return new CompoundFrameRelation(this._ParentRelation, this._Parent.GetRelation(Other));
            }
            else
            {
                return new CompoundFrameRelation(this.GetRelation(Other._Parent), Other._ParentRelation.Inverse);
            }
        }

        private ReferenceFrame _Parent;
        private IFrameRelation _ParentRelation;
    }

    /// <summary>
    /// Relation from one frame of reference to another.
    /// </summary>
    public interface IFrameRelation : IImmutable
    {
        /// <summary>
        /// Converts an event in the source frame of this relation to an event
        /// in the destination frame.
        /// </summary>
        Event Convert(Event Event);

        /// <summary>
        /// Gets the inverse of the relation, which is the relation with the
        /// source and destination frames switched. A event applied to both
        /// a relation and its inverse will remain unchanged.
        /// </summary>
        IFrameRelation Inverse { get; }
    }

    /// <summary>
    /// A frame relation this is the combonation of two others applied together.
    /// </summary>
    public class CompoundFrameRelation : IFrameRelation
    {
        public CompoundFrameRelation(IFrameRelation A, IFrameRelation B)
        {
            this._A = A;
            this._B = B;
        }

        Event IFrameRelation.Convert(Event Event)
        {
            return this._A.Convert(this._B.Convert(Event));
        }

        IFrameRelation IFrameRelation.Inverse
        {
            get 
            {
                return new CompoundFrameRelation(this._B.Inverse, this._A.Inverse);
            }
        }

        private IFrameRelation _A;
        private IFrameRelation _B;
    }

    /// <summary>
    /// A frame relation that will always return every event it is given unchanged.
    /// </summary>
    public class IdentityFrameRelation : IFrameRelation
    {
        public IdentityFrameRelation()
        {

        }

        Event IFrameRelation.Convert(Event Event)
        {
            return Event;
        }

        IFrameRelation IFrameRelation.Inverse
        {
            get 
            {
                return this;
            }
        }
    }
}
