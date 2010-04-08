//----------------------------------------
// Copyright (c) 2010, Dmitry Zamkov 
// Open source under the new BSD License
//----------------------------------------
using System;
using System.Collections.Generic;

namespace NSpace.Physics
{
    /// <summary>
    /// Object that participates in physical interactions.
    /// </summary>
    public abstract class Entity
    {
        public Entity()
        {
            this._Causes = new Entity[0];
            this._Effects = new Dictionary<Entity, object>();
        }

        /// <summary>
        /// Gets a set of entities that currently affect this entity.
        /// </summary>
        public abstract IEnumerable<Entity> Causes { get; }

        /// <summary>
        /// Gets a set of entities this entity currently affects.
        /// </summary>
        public IEnumerable<Entity> Effects
        {
            get
            {
                return this._Effects.Keys;
            }
        }

        /// <summary>
        /// Reports a change to the entity after it is made. An optional
        /// description can be specified and will be given to entities
        /// listening to changes. This must be called on any change to
        /// the outward interface(nonprivate members) of the entity. This
        /// must also be called to update the causes of the entity.
        /// </summary>
        protected void ReportChange(object Description)
        {
            this._UpdateCauses();
            foreach (Entity e in this.Effects)
            {
                // Check if this is still a cause of e.
                foreach (Entity c in e._Causes)
                {
                    if (c == e)
                    {
                        e.OnCauseChange(this, Description);
                    }
                }
            }
        }

        /// <summary>
        /// Called when an entity indentified as a possible cause of this entity
        /// is changed. An optional description of the change may be supplied.
        /// </summary>
        protected virtual void OnCauseChange(Entity Entity, object Description)
        {

        }

        /// <summary>
        /// Updates the causes of this entity and updates the effects
        /// on involved entites.
        /// </summary>
        private void _UpdateCauses()
        {
            this._Causes = this.Causes;
            foreach(Entity e in this._Causes)
            {
                this._Effects[e] = null;
            }
        }

        private IEnumerable<Entity> _Causes; // Cached, changed only on report change.
        private Dictionary<Entity, object> _Effects;
    }
}