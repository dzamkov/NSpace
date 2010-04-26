//----------------------------------------
// Copyright (c) 2010, Dmitry Zamkov 
// Open source under the new BSD License
//----------------------------------------
using System;
using System.Collections.Generic;

namespace NSpace.Physics
{
    /// <summary>
    /// A object capable of interaction with other
    /// entities and possibly external systems. This may be a combination
    /// of other entities, or the resulting state of entities after an
    /// interaction.
    /// </summary>
    public interface IEntity : IImmutable
    {
        /// <summary>
        /// Causes the entity to interact with the specified system and returns
        /// an system entity that, when supplied with the results of the system,
        /// will create an entity that represents this entity after interactions
        /// and effects of the system are applied.
        /// </summary>
        void Interact<I, R>(ISystem<I, R> System, out I Input, out ISystemEntity<I, R> SystemEntity)
            where I : IImmutable
            where R : ISystemResult<I, R>;
    }

    /// <summary>
    /// An object that allows interactions or effects to be applied to
    /// entities. When a system is given to the entity, the entity inputs
    /// its effects and interactions to the system along with information of how to
    /// create a new entity representing it after changes are applied.
    /// </summary>
    /// <typeparam name="I">The type of input given to the system.</typeparam>
    /// <typeparam name="R">The type of result the system gives.</typeparam>
    public interface ISystem<I, R> : IImmutable
        where I : IImmutable
        where R : ISystemResult<I, R>
    {
        /// <summary>
        /// Applies the system to an input, and returns the result based on it.
        /// </summary>
        R Apply(I Input);

        /// <summary>
        /// Combines several inputs into one.
        /// </summary>
        I Combine(IEnumerable<I> Inputs);
    }

    /// <summary>
    /// A result from a system.
    /// </summary>
    /// <remarks>if x and y are system results with the same exact input and were processed by the same 
    /// system, they must themselves be the same.</remarks>
    /// <typeparam name="I">The type of input the system takes.</typeparam>
    /// <typeparam name="R">The type of this result.</typeparam>
    public interface ISystemResult<I, R> : IImmutable
        where I : IImmutable
        where R : ISystemResult<I, R>
    {
        /// <summary>
        /// Gets the input that corresponds to this result.
        /// </summary>
        I Input { get; }

        /// <summary>
        /// If this result corresponds to an input that was combined from other
        /// inputs using the combine method, this will return the result for a
        /// specified input that is part of said combined input.
        /// </summary>
        void GetSubResult(I Input, out R Result);
    }

    /// <summary>
    /// An object that can create an entity after a system calculates the interactions
    /// and effects it has.
    /// </summary>
    /// <typeparam name="I">The type of input given to the system.</typeparam>
    /// <typeparam name="R">The type of result from a system this system entity can handle.</typeparam>
    public interface ISystemEntity<I, R> : IImmutable
        where I : IImmutable
        where R : ISystemResult<I, R>
    {
        /// <summary>
        /// Creates a new entity with the results of a system.
        /// </summary>
        IEntity Create(ISystem<I, R> System, R Result);
    }

    /// <summary>
    /// An entity that represents the combination of the interactions
    /// and effects of a set of other entities.
    /// </summary>
    public class CompoundEntity : IEntity
    {
        public CompoundEntity(IEnumerable<IEntity> SubEntities)
        {
            this._Entities = new List<IEntity>(SubEntities);
        }

        void IEntity.Interact<I, R>(ISystem<I, R> System, out I Input, out ISystemEntity<I, R> SystemEntity)
        {
            // Inputs of the entities that make up this compound entity paired with their corresponding system entities.
            Dictionary<I, ISystemEntity<I, R>> ise = new Dictionary<I, ISystemEntity<I, R>>();

            foreach (IEntity ent in this._Entities)
            {
                // Get the inputs and system entities for each entity.
                I entinput;
                ISystemEntity<I, R> sysent;
                ent.Interact<I, R>(System, out entinput, out sysent);
                ise.Add(entinput, sysent);
            }

            Input = System.Combine(ise.Keys);
            SystemEntity = new SystemEntity<I, R>(ise);
        }

        /// <summary>
        /// The type of system entity given for a compound entity.
        /// </summary>
        private class SystemEntity<I, R> : ISystemEntity<I, R>
            where I : IImmutable
            where R : ISystemResult<I, R>
        {
            public SystemEntity(IDictionary<I, ISystemEntity<I, R>> ISE)
            {
                this._ISE = ISE;
            }

            IEntity ISystemEntity<I, R>.Create(ISystem<I, R> System, R Result)
            {
                // Entities after interaction
                List<IEntity> postents = new List<IEntity>();
                foreach (KeyValuePair<I, ISystemEntity<I, R>> preent in this._ISE)
                {
                    R result; Result.GetSubResult(preent.Key, out result);
                    postents.Add(preent.Value.Create(System, result));
                }
                return new CompoundEntity(postents);
            }

            private IDictionary<I, ISystemEntity<I, R>> _ISE;
        }

        private IEnumerable<IEntity> _Entities;
    }
}