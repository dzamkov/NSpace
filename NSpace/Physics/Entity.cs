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
    /// entities and possibly external systems. This may be a combination
    /// of other entities, or the resulting state of entities after an
    /// interaction.
    /// </summary>
    public interface IEntity : IImmutable
    {

    }

    /// <summary>
    /// An effect or interaction an entity has on a system.
    /// </summary>
    /// <typeparam name="I">The type of data given by the effects to the system.</typeparam>
    /// <typeparam name="O">The type of data the system gives to the effect after interaction.</typeparam>
    public interface IEffect<I, O> : IImmutable
    {
        /// <summary>
        /// Gets the input data of the effect.
        /// </summary>
        D Input { get; }

        /// <summary>
        /// Sets the output data of the effect. This should not change the input, because this
        /// is an immutable object.
        /// </summary>
        O Output { set; }
    }

    /// <summary>
    /// An entity that produces a set of effects of a specified type.
    /// </summary>
    public interface IEffectEntity<D, R> : IEntity
    {
        /// <summary>
        /// Gets the set of effects of the correct type this entity produces.
        /// </summary>
        IEnumerable<IEffect<D, R>> Effects { get; }
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