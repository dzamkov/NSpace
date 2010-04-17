//----------------------------------------
// Copyright (c) 2010, Dmitry Zamkov 
// Open source under the new BSD License
//----------------------------------------
using System;
using System.Collections.Generic;

namespace NSpace.Physics
{
    /// <summary>
    /// A system which stores spatial and temporal entities and can be used with other
    /// systems to produce effects between them.
    /// </summary>
    public interface ISpaceTime : ITemporalSystem<ITemporalEntity>
    {
        /// <summary>
        /// Adds a system to affect the entities of the spacetime and allow
        /// them to interact.
        /// </summary>
        void AddSystem<E>(ITemporalSystem<E> System);
    }

    /// <summary>
    /// A system that affects the entities in a spacetime of a specific type.
    /// </summary>
    /// <typeparam name="E">The type of entities this system
    /// can affect.</typeparam>
    public interface ITemporalSystem<E> : ISystem<E>
        where E : class, ITemporalEntity
    {
        /// <summary>
        /// Adds an entity in the system to interact with other entities. A tag is
        /// given to allow changes to the entity once added.
        /// </summary>
        ITemporalEntityTag AddEntity(E Entity);
    }

    /// <summary>
    /// Represents the state of an entity in a spacetime or system.
    /// </summary>
    public interface ITemporalEntityTag
    {
        /// <summary>
        /// Indicates an in-place change of the entity's state has occured.
        /// </summary>
        void Update();

        /// <summary>
        /// Removes the entity from the spacetime.
        /// </summary>
        void Remove();
    }

    /// <summary>
    /// An entity that can be stored in a spacetime.
    /// </summary>
    public interface ITemporalEntity
    {

    }

    /// <summary>
    /// Simple unoptimized implementation of a spacetime.
    /// </summary>
    public class SimpleSpaceTime : ISpaceTime
    {
        public SimpleSpaceTime()
        {
            this._Entities = new Dictionary<ITemporalEntity, List<ITemporalEntityTag>>();
            this._Systems = new Dictionary<Type, List<ITemporalSystem<ITemporalEntity>>>();
        }

        public void AddSystem<E>(ITemporalSystem<E> System)
            where E : class, ITemporalEntity
        {
            // Add to system list
            ITemporalSystem<ITemporalEntity> sys = 
                typeof(E) == typeof(ITemporalEntity) ? 
                (ITemporalSystem<ITemporalEntity>)System : 
                new System<E>(System);


            List<ITemporalSystem<ITemporalEntity>> typesys = null;
            if (this._Systems.TryGetValue(typeof(E), out typesys))
            {
                typesys.Add(sys);
            }
            else
            {
                this._Systems[typeof(E)] = new List<ITemporalSystem<ITemporalEntity>>(new ITemporalSystem<ITemporalEntity>[1] { sys });
            }

            // Add entities to system
            foreach (KeyValuePair<ITemporalEntity, List<ITemporalEntityTag>> ent in this._Entities)
            {
                E e = ent.Key as E;
                if (e != null)
                {
                    ent.Value.Add(System.AddEntity(e));
                }
            }
        }

        public ITemporalEntityTag AddEntity(ITemporalEntity Entity)
        {
            // Create an entry for the entity in the spacetime.
            List<ITemporalEntityTag> tags = this._Entities[Entity] = new List<ITemporalEntityTag>();

            // Add the entity to relevant subsystems.
            foreach (KeyValuePair<Type, List<ITemporalSystem<ITemporalEntity>>> typesystems in this._Systems)
            {
                if (typesystems.Key.IsAssignableFrom(Entity.GetType()))
                {
                    foreach (ITemporalSystem<ITemporalEntity> system in typesystems.Value)
                    {
                        tags.Add(system.AddEntity(Entity));
                    }
                }
            }

            // Give a tag for the entity in this spacetime.
            return new Tag(this, Entity);
        }

        /// <summary>
        /// Called when an entity updates its state.
        /// </summary>
        internal void _UpdateEntity(ITemporalEntity Entity)
        {
            // Update the entity in all subsystems.
            foreach (ITemporalEntityTag tag in this._Entities[Entity])
            {
                tag.Update();
            }
        }

        /// <summary>
        /// Called when an entity is removed from the spacetime.
        /// </summary>
        internal void _RemoveEntity(ITemporalEntity Entity)
        {
            // Remove the entity from all subsystems.
            foreach (ITemporalEntityTag tag in this._Entities[Entity])
            {
                tag.Remove();
            }

            // Remove the entity from spacetime.
            this._Entities.Remove(Entity);
        }

        /// <summary>
        /// Adapter that allows systems of a more complicated entity type
        /// to be used as a generalized system of temporal entities.
        /// </summary>
        private class System<E> : ITemporalSystem<ITemporalEntity>
            where E : class, ITemporalEntity
        {
            public System(ITemporalSystem<E> System)
            {
                this.Source = System;
            }

            public ITemporalEntityTag AddEntity(ITemporalEntity Entity)
            {
                return this.Source.AddEntity(Entity as E);
            }

            /// <summary>
            /// The system this is from.
            /// </summary>
            public ITemporalSystem<E> Source; 
        }

        /// <summary>
        /// Tag given for an entity in the spacetime.
        /// </summary>
        private class Tag : ITemporalEntityTag
        {
            public Tag(SimpleSpaceTime SpaceTime, ITemporalEntity Entity)
            {
                this.SpaceTime = SpaceTime;
                this.Entity = Entity;
            }

            public void Update()
            {
                this.SpaceTime._UpdateEntity(this.Entity);
            }

            public void Remove()
            {
                this.SpaceTime._RemoveEntity(this.Entity);
            }

            /// <summary>
            /// The spacetime the tag is for.
            /// </summary>
            public SimpleSpaceTime SpaceTime;

            /// <summary>
            /// The entity this tag is for.
            /// </summary>
            public ITemporalEntity Entity;
        }

        private Dictionary<ITemporalEntity, List<ITemporalEntityTag>> _Entities;
        private Dictionary<Type, List<ITemporalSystem<ITemporalEntity>>> _Systems;
    }
}