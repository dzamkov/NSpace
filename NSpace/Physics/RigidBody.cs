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
    public class RigidBody : IBody
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

        public void Interact(IBody Other)
        {
            ICompoundBody cb = Other as ICompoundBody;
            if (cb != null)
            {
                foreach (IBody body in cb.Bodies)
                {
                    this.Interact(body);
                }
            }

            IForce force = Other as IForce;
            if (force != null)
            {
                ICurve fr = force.Apply(this._Section, this._Properties.Mass, this._TimeBound, this._Position, this._Velocity);
                this._Velocity = fr.Integral(new Vector());
                this._Velocity.Multiply(this._TimeBound.Size.Seconds);
                this._Position = this._Velocity.Integral(new Vector());
                this._Position.Multiply(this._TimeBound.Size.Seconds);
            }
        }

        /// <summary>
        /// Creates a body with the specified parameters and adds it to a world.
        /// </summary>
        public static RigidBody Create(World World, Section Section, Property Properties)
        {
            RigidBody rb = new RigidBody();
            rb._Position = null;
            rb._Velocity = null;
            rb._TimeBound = new TimeBound(0.0, 5.0);
            rb._Section = Section;
            rb._Properties = Properties;
            rb.Interact(World.Contents);
            World.Contents.Add(rb);
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

        public void Attach(IBodyEventHandler EventHandler)
        {
            
        }

        public void Detach(IBodyEventHandler EventHandler)
        {
            
        }

        private TimeBound _TimeBound;
        private ICurve _Position;
        private ICurve _Velocity;
        private Section _Section;
        private Property _Properties;
    }
}