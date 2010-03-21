//----------------------------------------
// Copyright (c) 2010, Dmitry Zamkov 
// Open source under the new BSD License
//----------------------------------------
using System;
using System.Collections.Generic;

namespace NSpace.Physics
{
    /// <summary>
    /// An object at a point or span of time that can interact with other bodies
    /// at such time.
    /// </summary>
    public interface IBody
    {
        /// <summary>
        /// The time at which the body has effect.
        /// </summary>
        TimeBound TimeBound { get; }

        /// <summary>
        /// Causes this body to interact with another body. This body will take the changes
        /// created by the other body and may cause the other body to interact with a part
        /// of this body aswell. Bodies may not be directly affected and this is the only
        /// means by which changes to a body can be performed. Note that every body in a world
        /// must interact with every other body if any possible changes can be made. The receiving
        /// body should use event handlers to ensure the interaction stays consistent with the body.
        /// </summary>
        void Interact(IBody Other);

        /// <summary>
        /// Attaches an event handler to the body. The event handler will then be informed
        /// if there are any changes to the body.
        /// </summary>
        void Attach(IBodyEventHandler EventHandler);

        /// <summary>
        /// Detachs an event handler from the body so it no longer receives events from
        /// this body.
        /// </summary>
        void Detach(IBodyEventHandler EventHandler);
    }

    /// <summary>
    /// A body that only represents a portion of the time of another object and
    /// can be extended by adding another body after itself representing its
    /// progression.
    /// </summary>
    public interface IExtendableBody : IBody
    {
        /// <summary>
        /// Extends the body and returns a new body representing its progression
        /// over the specified amount of time. The new body may interact with the
        /// specified world. This function returning null will be taken as an act
        /// of defiance and indicate that there is no more bodies that represent the
        /// object after the current one.
        /// </summary>
        IBody Extend(TimeSpan Time, World World);
    }

    /// <summary>
    /// Handles simple general events for a body.
    /// </summary>
    public interface IBodyEventHandler
    {
        /// <summary>
        /// Called when another body takes the place of the body the event handler is for. The
        /// event handler will be transfered to the body automatically. This does not indicate
        /// a modification unless OnModified is called.
        /// </summary>
        void OnReassign(IBody Old, IBody New);

        /// <summary>
        /// Called after any properties on the body are modified. This includes a change
        /// in timebound.
        /// </summary>
        void OnModified(IBody Body);

        /// <summary>
        /// Called when the body is removed.
        /// </summary>
        void OnRemoved(IBody Body);
    }

    /// <summary>
    /// A body that is composed of a set of unique bodies that define the main
    /// bodies behavior and interactions. A graph of compound bodies must never
    /// create a circular link. This means there is no way to navigate back to
    /// an original body by just calling Bodies recursively on it and its parts.
    /// </summary>
    public interface ICompoundBody : IBody
    {
        /// <summary>
        /// Gets the set of bodies that make up the compound body.
        /// </summary>
        IEnumerable<IBody> Bodies { get; }
    }

    /// <summary>
    /// A group of bodies that act as a compound body.
    /// </summary>
    public class BodyGroup : ICompoundBody, IBodyEventHandler
    {
        public BodyGroup()
        {
            this._Bodies = new List<IBody>();
            this._EventHandlers = new List<IBodyEventHandler>();
            this._TimeBound = TimeBound.None;
        }

        public IEnumerable<IBody> Bodies
        {
            get 
            {
                return this._Bodies;
            }
        }
        public TimeBound TimeBound
        {
            get 
            {
                return this._TimeBound;
            }
        }

        public void Interact(IBody Other)
        {
            foreach (IBody Body in this._Bodies)
            {
                Body.Interact(Other);
            }
        }

        public void Attach(IBodyEventHandler EventHandler)
        {
            this._EventHandlers.Add(EventHandler);
        }

        public void Detach(IBodyEventHandler EventHandler)
        {
            this._EventHandlers.Remove(EventHandler);
        }

        /// <summary>
        /// Adds a body to this group.
        /// </summary>
        public void Add(IBody Body)
        {
            this._Bodies.Add(Body);
            Body.Attach(this);
            this._TimeBound = this._TimeBound.Union(Body.TimeBound);
            foreach (IBodyEventHandler beh in this._EventHandlers)
            {
                beh.OnModified(this);
            }
        }

        /// <summary>
        /// Removes a body from this group.
        /// </summary>
        public void Remove(IBody Body)
        {
            this._Bodies.Remove(Body);
            Body.Detach(this);
            if (this._TimeBound.TimeStart == Body.TimeBound.TimeStart ||
                this._TimeBound.TimeEnd == Body.TimeBound.TimeEnd)
            {
                //Recalculate time bound
                this._TimeBound = TimeBound.None;
                foreach (IBody body in this._Bodies)
                {
                    this._TimeBound = this._TimeBound.Union(body.TimeBound);
                }
            }
            foreach (IBodyEventHandler beh in this._EventHandlers)
            {
                beh.OnModified(this);
            }
        }

        public void OnReassign(IBody Old, IBody New)
        {

        }

        public void OnModified(IBody Body)
        {
            foreach (IBodyEventHandler beh in this._EventHandlers)
            {
                beh.OnModified(this);
            }
        }

        public void OnRemoved(IBody Body)
        {
            this.Remove(Body);
        }

        private List<IBody> _Bodies;
        private List<IBodyEventHandler> _EventHandlers;
        private TimeBound _TimeBound;
    }
}