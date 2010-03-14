﻿//----------------------------------------
// Copyright (c) 2010, Dmitry Zamkov 
// Open source under the new BSD License
//----------------------------------------
using System;
using System.Collections.Generic;

namespace NSpace.Physics
{
    /// <summary>
    /// A certain kind of force that can be applied to a body over time. The force will
    /// be applied as a curve indicating acceleration over time. The integral of this
    /// will produce velocity which will be factored into object movement. Forces must
    /// be finite and applied within a finite amount of time.
    /// </summary>
    public interface IForce
    {
        /// <summary>
        /// Applies the force to the specified point defined with position over time, velocity
        /// over time and the start and end times for those curves. This function returns an acceleration 
        /// over time that should be added to the forces acting upon the object.
        /// </summary>
        ICurve Apply(TimeBound Time, double Mass, Section Section, ICurve Position, ICurve Velocity);

        /// <summary>
        /// Combines the effects of this force with that of another. Since it is practically impossible to
        /// know what every combination of forces does to each other, it is somewhat suggested that a generalization
        /// of the forces is made. This function may be destructive to both of the supplied forces as long as it
        /// returns a force that represents the combination of them.
        /// </summary>
        IForce Combine(IForce Other);
    }

    /// <summary>
    /// A physics object that can apply a force.
    /// </summary>
    public interface IForceObject
    {
        /// <summary>
        /// Gets the forces produced by this physics object.
        /// </summary>
        IEnumerable<IForce> Forces { get; }
    }

    /// <summary>
    /// A force which presents a constant attraction to a direction in space.
    /// </summary>
    public class Gravity : IForce
    {
        public Gravity(Section WorldSection, Vector Force)
        {
            this._WorldSection = WorldSection;
            this._Force = Force;
        }

        /// <summary>
        /// The section used by the world where gravity is applied. The force vector
        /// is in relation to this section. Bodies outside this section, but still inside
        /// the world will still feel the effects of this force.
        /// </summary>
        public Section WorldSection
        {
            get
            {
                return this._WorldSection;
            }
        }

        /// <summary>
        /// The force in units/second^2 applied by gravity. This vector is in relation to the 
        /// world section. 9.8 meters/second^2 is gravitational force on the surface of earth.
        /// </summary>
        public Vector Force
        {
            get
            {
                return this._Force;
            }
        }

        public ICurve Apply(TimeBound Time, double Mass, Section Section, ICurve Position, ICurve Velocity)
        {
            Vector forceinbody = this._WorldSection.GetRelation(Section).LinearTransform(this._Force) * Mass;
            return new ConstantCurve(forceinbody);
        }

        public IForce Combine(IForce Other)
        {
            return this;
        }

        private Section _WorldSection;
        private Vector _Force;
    }

    /// <summary>
    /// A force that tries to keep the any object its applied to static.
    /// </summary>
    public class Staticness : IForce
    {
        public ICurve Apply(TimeBound Time, double Mass, Section Section, ICurve Position, ICurve Velocity)
        {
            ICurve force = Velocity.Derivative();
            force.Negate();
            return force;
        }

        public IForce Combine(IForce Other)
        {
            return this;
        }
    }

    /// <summary>
    /// A physics object that applies a force across an entire world.
    /// </summary>
    public class GlobalForceObject : PhysicsObject, IForceObject
    {
        public GlobalForceObject(IForce Force)
        {
            this._Force = Force;
        }

        public override TimeBound TimeBound
        {
            get
            {
                return TimeBound.Huge;
            }
        }

        public override void Interact(PhysicsObject Other)
        {
            
        }

        public IEnumerable<IForce> Forces
        {
            get 
            {
                return new IForce[1] { this._Force };
            }
        }

        private IForce _Force;
    }
}
