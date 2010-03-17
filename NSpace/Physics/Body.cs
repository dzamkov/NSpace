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
        /// means by which changes to a body can be performed.
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