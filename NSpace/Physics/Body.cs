//----------------------------------------
// Copyright (c) 2010, Dmitry Zamkov 
// Open source under the new BSD License
//----------------------------------------
using System;
using System.Collections.Generic;

namespace NSpace.Physics
{
    /// <summary>
    /// An object in a spacetime that is affect by physical interactions. Bodies must
    /// have a one-to-one relationship with a spacetime for their lifetime.
    /// </summary>
    public interface IBody
    {
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
        /// Called when the affect of the body is removed permanently. If the body is not
        /// doing anything, and will never do anything again, this gets called.
        /// </summary>
        void OnRemoved(IBody Body);
    }
}