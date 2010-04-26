//----------------------------------------
// Copyright (c) 2010, Dmitry Zamkov 
// Open source under the new BSD License
//----------------------------------------
using System;
using System.Collections.Generic;

namespace NSpace.Physics
{

    /// <summary>
    /// A physics object whose collision is defined by a single shape that does
    /// not change over the lifetime of the body.
    /// </summary>
    public class RigidBody : IEntity
    {
        private RigidBody()
        {
            
        }

        public Section Section
        {
            get 
            {
                return this._Section;
            }
        }

        public TimeBound TimeBound
        {
            get 
            {
                return this._TimeBound;
            }
        }

        /// <summary>
        /// Creates a body with the specified parameters and adds it to a spacetime.
        /// </summary>
        public static RigidBody Create(ISpaceTime SpaceTime, Section Section, Property Properties)
        {
            RigidBody rb = new RigidBody();
            rb._InitVelocity = new Vector();
            rb._TimeBound = new TimeBound(0.0, 5.0);
            rb._Section = Section;
            rb._Properties = Properties;
            rb._RecalculatePath();
            SpaceTime.AddEntity(rb);
            return rb;
        }

        /// <summary>
        /// Gets the properties of the rigid body.
        /// </summary>
        public Property Properties
        {
            get
            {
                return this._Properties;
            }
        }

        /// <summary>
        /// Gets the section that shows the orientation of the rigid body
        /// at the specified time within the rigid bodies time bounds.
        /// </summary>
        public Section GetSectionAtTime(Time Time)
        {
            return this._Section.CreateRelation(
                Matrix.Translate(
                    this._Position.GetPoint(this._TimeBound.BoundRelation(Time))));
        }

        /// <summary>
        /// Properties of a rigid body such as shape, mass, etc.
        /// </summary>
        public struct Property
        {
            public Property(IShape Shape, Vector MassCenter, double Mass)
            {
                this.Shape = Shape;
                this.MassCenter = MassCenter;
                this.Mass = Mass;
            }

            /// <summary>
            /// The shape of the rigid body, used for collisions.
            /// </summary>
            public IShape Shape;

            /// <summary>
            /// The center of mass for the body in local
            /// coordinates of its shape.
            /// </summary>
            public Vector MassCenter;

            /// <summary>
            /// The mass of the body in  kilograms.
            /// </summary>
            public double Mass;
        }

        /// <summary>
        /// Recalculates the path of the body over time based on interactions.
        /// </summary>
        private void _RecalculatePath()
        {
            ICurve force = new ConstantCurve(this._GravityForce);

            ICurve velocity = force.Integral(this._InitVelocity);

            this._Position = velocity.Integral(new Vector(0.0, 0.0, 0.0));
        }

        public double Mass
        {
            get 
            {
                return this._Properties.Mass;
            }
        }

        public Vector GravityForce
        {
            set 
            {
                this._GravityForce = value;
                this._RecalculatePath();
            }
        }

        private Vector _GravityForce;
        private TimeBound _TimeBound;
        private ICurve _Position;
        private Vector _InitVelocity;
        private Section _Section;
        private Property _Properties;  
    }
}