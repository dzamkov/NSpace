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
        ISystemEntity Interact(ISystem System);
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

        public ISystemEntity Interact(ISystem System)
        {
            
        }

        /// <summary>
        /// A system entity returned by a compound entity.
        /// </summary>
        private class SystemEntity : ISystemEntity
        {
            public SystemEntity(IEnumerable<IEntity> Entities, ISystem System)
            {
                // Give each entity a subsystem to interact with.
                this.SystemEntities = new Dictionary<ISystem, ISystemEntity>();
                foreach (IEntity e in Entities)
                {
                    ISystem sub = System.CreateSubSystem();
                    this.SystemEntities[sub] = e.Interact(sub);
                }
            }

            public IEntity Create(ISystemResult Result)
            {
                // Create new entities from the results of their interactions with the subsystems.
                List<IEntity> subents = new List<IEntity>();
                foreach (KeyValuePair<ISystem, ISystemResult> res in Result.SubSystemResults)
                {
                    subents.Add(this.SystemEntities[res.Key].Create(res.Value));
                }
                return new CompoundEntity(subents);
            }

            public Dictionary<ISystem, ISystemEntity> SystemEntities;
        }

        private IEnumerable<IEntity> _Entities;
    }

    /// <summary>
    /// An object that allows interactions or effects to be applied to
    /// entities. When a system is given to the entity, the entity inputs
    /// its effects and interactions to the system along with information of how to
    /// create a new entity representing it after changes are applied.
    /// </summary>
    public interface ISystem
    {
        /// <summary>
        /// Creates a subsystem for this system. A subsystem contains its own
        /// set of interactions and effects which can be combined with the main
        /// system. The results of subsystems may only be calculated indirectly.
        /// </summary>
        ISystem CreateSubSystem();

        /// <summary>
        /// Converts and outputs a system of a specific type representing this system.
        /// Null is returned if the conversion is not possible or is unknown to the
        /// system. If an object supports a more complex, specific type of system, it
        /// is best to check if this system can be converted to that type before defaulting
        /// to a simple, generalized system.
        /// </summary>
        void As<S>(out S System) where S : ISystem;
    }

    /// <summary>
    /// The results of the effects of a system.
    /// </summary>
    public interface ISystemResult : IImmutable
    {
        /// <summary>
        /// Gets a reference to the system this result is for.
        /// </summary>
        ISystem System { get; }

        /// <summary>
        /// Returns a mapping of subsystems of the system this result is for
        /// and the results of the subsystems for that system.
        /// </summary>
        IDictionary<ISystem, ISystemResult> SubSystemResults { get; }
    }

    /// <summary>
    /// An object that can create an entity after a system calculates the interactions
    /// and effects it has.
    /// </summary>
    public interface ISystemEntity : IImmutable
    {
        /// <summary>
        /// Creates a new entity with the results of a system.
        /// </summary>
        IEntity Create(ISystemResult Result);
    }
}