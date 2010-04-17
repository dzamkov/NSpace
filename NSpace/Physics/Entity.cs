//----------------------------------------
// Copyright (c) 2010, Dmitry Zamkov 
// Open source under the new BSD License
//----------------------------------------
using System;
using System.Collections.Generic;

namespace NSpace.Physics
{
    /// <summary>
    /// A dynamic object capable of interaction with other
    /// entities and possibly external systems. All effects
    /// one entity has on another must be pushed to it, either by
    /// calling a method or setting a member directly. The full
    /// information about the internal state of an entity may
    /// be lazy evaluated.
    /// </summary>
    public interface IEntity
    {

    }

    /// <summary>
    /// An object that has control of the interactions between several entities.
    /// The states of the entities are interdependant with each other. Systems have
    /// a certain type of object that they can use for collecting information about
    /// the effects the entities have that is pushed to the system. When the system
    /// recalculates the state of the entity after interaction, it is pushed back to
    /// the entity.
    /// </summary>
    /// <typeparam name="E">The type of entity the system may get its data from and push
    /// results to.</typeparam>
    public interface ISystem<E> : IEntity
        where E : class, IEntity
    {

    }
}