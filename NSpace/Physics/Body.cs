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
        /// Causes this body to interact with another body. This body will take the changes
        /// created by the other body and may cause the other body to interact with a part
        /// of this body aswell. Bodies may not be directly affected and this is the only
        /// means by which changes to a body can be performed. Note that every body in a world
        /// must interact with every other body if any possible changes can be made. In the case
        /// that this has already interacted with the other body, this will update the interaction
        /// if the specified body has changed since the last interaction.
        /// </summary>
        void Interact(IBody Other);
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
}