//----------------------------------------
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
    public abstract class Force : PhysicsObject
    {
        /// <summary>
        /// Applies the force to the specified point defined with position over time, velocity
        /// over time and the start and end times for those curves. This function returns an acceleration 
        /// over time that should be added to the forces acting upon the object.
        /// </summary>
        public abstract ICurve Apply(double TimeStart, double TimeEnd, Section Section, ICurve Position, ICurve Velocity);
    }

    /// <summary>
    /// A force which presents a constant attraction to a direction in space.
    /// </summary>
    public class Gravity : Force
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

        public override ICurve Apply(double TimeStart, double TimeEnd, Section Section, ICurve Position, ICurve Velocity)
        {
            Vector forceinbody = this._WorldSection.GetRelation(Section) * this._Force;
            return new ConstantCurve(forceinbody);
        }

        public override TimeBound TimeBound
        {
            get 
            {
                return TimeBound.Huge;
            }
        }

        private Section _WorldSection;
        private Vector _Force;
    }
}
