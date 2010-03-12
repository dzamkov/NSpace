//----------------------------------------
// Copyright (c) 2010, Dmitry Zamkov 
// Open source under the new BSD License
//----------------------------------------
using System;
using System.Collections.Generic;

namespace NSpace.Physics
{
    /// <summary>
    /// An object that can participate in physical interactions.
    /// </summary>
    public abstract class PhysicsObject
    {
        public PhysicsObject()
        {
            this._Markers = new List<Marker>();
        }

        /// <summary>
        /// The time period in which this object can interact with others.
        /// </summary>
        public abstract TimeBound TimeBound { get; }

        /// <summary>
        /// Causes this physics object to interact with another. This produces changes on
        /// the physics object without affect any other.
        /// </summary>
        public abstract void Interact(PhysicsObject Other);

        /// <summary>
        /// Updates the bounds of the physics object when it is changed. Whenever
        /// the bound of the object changes, this must be called.
        /// </summary>
        protected virtual void UpdateBound()
        {
            foreach (Marker m in this._Markers)
            {
                m._BoundChange(this);
            }
        }

        /// <summary>
        /// Causes all markers that mark this object to also mark the other specified object.
        /// </summary>
        protected virtual void Mark(PhysicsObject Other)
        {
            foreach (Marker m in this._Markers)
            {
                m.Mark(Other);
            }
        }

        internal List<Marker> _Markers;
    }

    /// <summary>
    /// Acts as a pointer to a physics object, but changes with time to fit the object. For example, if
    /// at a point in time, an object is split into two, this marker will point to both.
    /// </summary>
    public class Marker
    {
        public Marker()
        {
            this._Objs = new DynamicBoundMap<TimeBound, PhysicsObject>(new TimeBoundScorer());
        }

        public Marker(IEnumerable<PhysicsObject> Objs) : this()
        {
            foreach (PhysicsObject obj in Objs)
            {
                obj._Markers.Add(this);
                this._Objs[obj] = obj.TimeBound;
            }
        }

        public Marker(PhysicsObject Obj) : this()
        {
            this.Mark(Obj);
        }

        /// <summary>
        /// Adds an object to be marked by this marker.
        /// </summary>
        public void Mark(PhysicsObject Object)
        {
            Object._Markers.Add(this);
            this._Objs[Object] = Object.TimeBound;
        }

        /// <summary>
        /// Gets the marked physics objects at the specified time.
        /// </summary>
        public IEnumerable<PhysicsObject> GetObjects(Time Time)
        {
            return this._Objs.Intersect(Time);
        }

        /// <summary>
        /// Gets all objects that are marked by this marker.
        /// </summary>
        public IEnumerable<PhysicsObject> Objects
        {
            get
            {
                return this._Objs.Objects;
            }
        }

        /// <summary>
        /// Called when the bounds of a marked object is changed.
        /// </summary>
        internal void _BoundChange(PhysicsObject Obj)
        {
            this._Objs[Obj] = Obj.TimeBound;
        }

        private DynamicBoundMap<TimeBound, PhysicsObject> _Objs;
    }

    /// <summary>
    /// A physics object that exists at a particular location at any time
    /// in its life.
    /// </summary>
    public abstract class Body : PhysicsObject
    {
        /// <summary>
        /// Gets the section the body uses as its coordinate space.
        /// </summary>
        public abstract Section Section { get; }

        /// <summary>
        /// Gets the span relative to the bodies section that specifies the time and space
        /// this body occupies.
        /// </summary>
        public abstract Span Span { get; }

        public override TimeBound TimeBound
        {
            get 
            {
                return this.TimeBound; 
            }
        }
    }

    /// <summary>
    /// A physics object that is composed of several other physics objects. A physics object is only
    /// a composite physics object if its behavior is determined entirely by its parts.
    /// </summary>
    public interface ICompositeObject
    {
        /// <summary>
        /// Gets the physics objects that make up this physics object.
        /// </summary>
        IEnumerable<PhysicsObject> Parts { get; }
    }

    /// <summary>
    /// A physics object that can be extended in time.
    /// </summary>
    public interface IExtendableObject
    {
        /// <summary>
        /// Extends the life of the physics object by the specified amount of time.
        /// </summary>
        void Extend(TimeSpan Time);
    }
}
