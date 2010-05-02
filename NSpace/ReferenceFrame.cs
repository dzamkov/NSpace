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
        public ReferenceFrame()
        {
            this._Parent = null;
            this._ParentRelation = null;
            this._Level = 0;
        }
        
        public ReferenceFrame(ReferenceFrame Parent, IFrameRelation ParentRelation)
        {
            this._Parent = Parent;
            this._ParentRelation = ParentRelation;
            this._Level = Parent == null ? 0 : Parent._Level + 1;
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
                return this._Level;
            }
        }

        /// <summary>
        /// Gets a relation one frame of reference has to another. Null is returned if
        /// there is no way to convert the frames of reference, they belong to different
        /// absolute frames. The resulting frame relation should be able to convert events in
        /// Other to events in this.
        /// </summary>
        public IFrameRelation GetRelation(ReferenceFrame Other)
        {
            // TODO: Something is very, very wrong here, I dont know what
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
                        return new CompoundFrameRelation(Other._ParentRelation.Inverse, 
                            new CompoundFrameRelation(
                                this._Parent.GetRelation(Other._Parent),
                                this._ParentRelation)); 
                    }
                    else
                    {
                        return null;
                    }
                }
            }
            if (this.Level > Other.Level)
            {
                return new CompoundFrameRelation(this._Parent.GetRelation(Other), this._ParentRelation.Inverse);
            }
            else
            {
                return new CompoundFrameRelation(this.GetRelation(Other.Parent), Other._ParentRelation.Inverse);
            }
        }

        /// <summary>
        /// Gets a frame relation that can convert from the source frame to the destination frame.
        /// </summary>
        public static IFrameRelation Relation(ReferenceFrame Source, ReferenceFrame Destination)
        {
            return Destination.GetRelation(Source);
        }

        /// <summary>
        /// Creates a child frame of reference with the specified parent to child
        /// relation.
        /// </summary>
        public ReferenceFrame CreateChild(IFrameRelation Relation)
        {
            return new ReferenceFrame(this, Relation.Inverse);
        }

        private int _Level;
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
        Event Transform(Event Event);

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

        Event IFrameRelation.Transform(Event Event)
        {
            return this._A.Transform(this._B.Transform(Event));
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

        Event IFrameRelation.Transform(Event Event)
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

    /// <summary>
    /// A static frame relation that uses a matrix to change vector spaces while disregarding time.
    /// </summary>
    public class AfflineTransformFrameRelation : IFrameRelation
    {
        public AfflineTransformFrameRelation(Matrix Transform)
        {
            this._Transform = Transform;
        }

        Event IFrameRelation.Transform(Event Event)
        {
            return new Event(this._Transform * Event.Point, Event.Time);
        }

        IFrameRelation IFrameRelation.Inverse
        {
            get 
            {
                return new AfflineTransformFrameRelation(this._Transform.Inverse()); 
            }
        }

        private Matrix _Transform;
    }

    /// <summary>
    /// A frame relation that rotates on the z axis over time.
    /// </summary>
    public class RotationalFrameRelation : IFrameRelation
    {
        public RotationalFrameRelation(Time Period)
        {
            this._Period = Period;
        }

        Event IFrameRelation.Transform(Event Event)
        {
            return new Event(Quaternion.AxisRotate(
                new Vector(0.0, 0.0, 1.0),
                Event.Time.Amount * 2.0 * Math.PI / this._Period.Amount).ToMatrix() *
                Event.Point, Event.Time);
        }

        IFrameRelation IFrameRelation.Inverse
        {
            get 
            {
                return new RotationalFrameRelation(new Time(-this._Period.Amount));
            }
        }

        private Time _Period;
    }
}
