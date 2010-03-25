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

        /// <summary>
        /// Attaches a compound body event handler.
        /// </summary>
        void Attach(ICompoundBodyEventHandler EventHandler);

        /// <summary>
        /// Detachs a compound body event handler.
        /// </summary>
        void Detach(ICompoundBodyEventHandler EventHandler);
    }

    /// <summary>
    /// A compound body where new bodies may be freely added and removed.
    /// </summary>
    public interface IContentBody : ICompoundBody
    {
        /// <summary>
        /// Adds a body to the content body.
        /// </summary>
        void Add(IBody Body);

        /// <summary>
        /// Removes a previously added body from the content body.
        /// </summary>
        void Remove(IBody Body);
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
    }

    /// <summary>
    /// Handles specific events for compound bodies.
    /// </summary>
    public interface ICompoundBodyEventHandler
    {
        /// <summary>
        /// Called when a body is added.
        /// </summary>
        void OnAdd(ICompoundBody CompoundBody, IBody Added);

        /// <summary>
        /// Called when a body is removed.
        /// </summary>
        void OnRemove(ICompoundBody CompoundBody, IBody Removed);
    }
}