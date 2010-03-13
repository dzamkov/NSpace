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
            rb._Mass = this._Mass;
            rb._MassCenter = this._MassCenter;
            rb._Shape = this._Shape;
            rb._Section = this.GetSectionAtTime(this._TimeBound.TimeEnd);
            rb._TimeBound = new TimeBound(this._TimeBound.TimeEnd, this._TimeBound.TimeEnd + Time);
            Init(new Vector(0.0, 0.0, 0.0), rb, World);
            this.Mark(rb);
        }

        /// <summary>
        /// Creates and initializes a rigid body in the specified world.
        /// </summary>
        public static RigidBody Create(World World, Section Section, double Mass, Vector MassCenter, IShape Shape)
        {
            RigidBody rb = new RigidBody();
            rb._Mass = Mass;
            rb._MassCenter = MassCenter;
            rb._Shape = Shape;
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
                        ICurve accel = force.Apply(Body._TimeBound, Body._Mass, Body._Section, null, null);
                        ICurve vel = accel.Integral(InitVelocity);
                        ICurve pos = vel.Integral(new Vector(0.0, 0.0, 0.0));
                        Body._Position = pos;
                        break;
                    }
                    break;
                }
            }
        }

        /// <summary>
        /// Gets the mass of the body in kilograms.
        /// </summary>
        public double Mass
        {
            get
            {
                return this._Mass;
            }
        }

        /// <summary>
        /// Gets the center of mass for the body in local
        /// coordinates of its shape.
        /// </summary>
        public Vector MassCenter
        {
            get
            {
                return this._MassCenter;
            }
        }

        /// <summary>
        /// Gets the shape of the rigid body.
        /// </summary>
        public IShape Shape
        {
            get
            {
                return this._Shape;
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

        private TimeBound _TimeBound;
        private ICurve _Position;
        private Section _Section;
        private Vector _MassCenter;
        private IShape _Shape;
        private double _Mass;
    }
}