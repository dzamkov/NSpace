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
    public class RigidBody : PhysicsObject, IExtendableObject
    {
        private RigidBody()
        {

        }

        public override TimeBound TimeBound
        {
            get 
            {
                return this._TimeBound;
            }
        }

        public override void Interact(PhysicsObject Other)
        {
            
        }

        public void Extend(TimeSpan Time, World World)
        {
            RigidBody rb = new RigidBody();
            rb._Properties = this._Properties;
            rb._Section = this.GetSectionAtTime(this._TimeBound.TimeEnd);
            rb._TimeBound = new TimeBound(this._TimeBound.TimeEnd, this._TimeBound.TimeEnd + Time);
            Init(this._Velocity.GetPoint(this._TimeBound.Size.Seconds), rb, World);
            this.Mark(rb);
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
        /// Creates and initializes a rigid body in the specified world.
        /// </summary>
        public static RigidBody Create(World World, Section Section, Property Properties)
        {
            RigidBody rb = new RigidBody();
            rb._Properties = Properties;
            rb._Section = Section;
            rb._TimeBound = new TimeBound(World.CurrentTime, World.CurrentTime + new TimeSpan(5.0));
            Init(new Vector(0.0, 0.0, 0.0), rb, World);
            World.AddPhysicsObject(rb);
            return rb;
        }

        /// <summary>
        /// Initializes a rigid body within a world by simulating its interaction with other objects.
        /// </summary>
        private static void Init(Vector InitVelocity, RigidBody Body, World World)
        {
            foreach (PhysicsObject physobj in World.Parts)
            {
                IForceObject forceobj = physobj as IForceObject;
                if (forceobj != null)
                {
                    foreach (IForce force in forceobj.Forces)
                    {
                        ICurve accel = force.Apply(Body._TimeBound, Body._Properties.Mass, Body._Section, null, null);
                        ICurve vel = accel.Integral(InitVelocity);
                        ICurve pos = vel.Integral(new Vector(0.0, 0.0, 0.0));
                        Body._Position = pos;
                        Body._Velocity = vel;
                        break;
                    }
                    break;
                }
            }
        }

        /// <summary>
        /// Gets the section this rigid body is in.
        /// </summary>
        public Section Section
        {
            get
            {
                return this._Section;
            }
        }

        /// <summary>
        /// Gets the section that shows the orientation of the rigid body
        /// at the specified time within the rigid bodies time bounds.
        /// </summary>
        public Section GetSectionAtTime(Time Time)
        {
            return this._Section.CreateRelation(Matrix.Translate(this._Position.GetPoint(this._TimeBound.BoundRelation(Time))));
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

        private TimeBound _TimeBound;
        private ICurve _Position;
        private ICurve _Velocity;
        private Section _Section;
        private Property _Properties;
    }
}