//----------------------------------------
// Copyright (c) 2010, Dmitry Zamkov 
// Open source under the new BSD License
//----------------------------------------
using System;
using System.Collections.Generic;

namespace NSpace.Physics
{
    /// <summary>
    /// A body that produces a force that acts on bodies over time.
    /// </summary>
    public interface IForce : IBody
    {
        /// <summary>
        /// Creates a curve that represents the force applied to a mass(a single point
        /// in space with no surface area or volume). The position and velocity of
        /// the point are specified relative to the section.
        /// </summary>
        ICurve Apply(Section Section, double Mass, TimeBound Time, ICurve Position, ICurve Velocity);
    }

    /// <summary>
    /// A body which applies a uniform force to all other bodies in a world that forces
    /// the bodies in a direction.
    /// </summary>
    public class Gravity : IForce
    {
        public Gravity(Vector Force, Section Section)
        {
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

        public ICurve Apply(Section Section, double Mass, TimeBound Time, ICurve Position, ICurve Velocity)
        {
            Vector forcevec = this._Section.GetRelation(Section).LinearTransform(this._Force) * Mass;
            return new ConstantCurve(forcevec);
        }

        public void Interact(IBody Other)
        {

        }

        private Vector _Force;
        private Section _Section;
    }
}
