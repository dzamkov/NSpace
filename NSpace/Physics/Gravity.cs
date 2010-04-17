﻿//----------------------------------------
// Copyright (c) 2010, Dmitry Zamkov 
// Open source under the new BSD License
//----------------------------------------
using System;
using System.Collections.Generic;

namespace NSpace.Physics
{
    /// <summary>
    /// A system which controls the application of a uniform force to all
    /// entities within the system.
    /// </summary>
    public class GravitySystem : ITemporalSystem<IGravitationalEntity>
    {
        public GravitySystem(ISpaceTime SpaceTime, Vector Force, Section Section)
        {
            this._SpaceTime = SpaceTime;
            this._Force = Force;
            this._Section = Section;
        }

        /// <summary>
        /// Gets the force gravity applies relative to its section.
        /// </summary>
        public Vector Force
        {
            get
            {
                return this._Force;
            }
        }

        /// <summary>
        /// Gets the section that the force is relative to.
        /// </summary>
        public Section Section
        {
            get
            {
                return this._Section;
            }
        }

        /// <summary>
        /// Gets the force vector of the force applied to a specific section.
        /// </summary>
        public Vector ForceAtSection(Section Section)
        {
            return this._Section.GetRelation(Section).LinearTransform(this._Force);
        }

        private ISpaceTime _SpaceTime;
        private Vector _Force;
        private Section _Section;
    }

    /// <summary>
    /// An entity that may participate in gravitational interactions.
    /// </summary>
    public interface IGravitationalEntity : ITemporalEntity
    {
        /// <summary>
        /// Gets the section the entity is in.
        /// </summary>
        Section Section { get; }

        /// <summary>
        /// Gets the total mass of the entity in kilograms.
        /// </summary>
        double Mass { get; }

        /// <summary>
        /// Sets the force vector due to gravity on the entity.
        /// </summary>
        Vector GravityForce { set; }
    }
}
