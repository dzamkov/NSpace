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
    public class RigidBody
    {
        public RigidBody(double Mass, Vector MassCenter, IShape Shape)
        {
            this._Mass = Mass;
            this._MassCenter = MassCenter;
            this._Shape = Shape;
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
        /// Creates an image for this rigid body.
        /// </summary>
        public Image CreateImage(Section Section, Vector Velocity)
        {
            return new Image(Section, this, Velocity);
        }

        /// <summary>
        /// An image, or snapshot of the state of a rigid body.
        /// </summary>
        public class Image
        {
            internal Image(Section Section, RigidBody Body, Vector Velocity)
            {
                this._Section = Section;
                this._Body = Body;
                this._Vel = Velocity;
            }

            /// <summary>
            /// Gets the section the rigid body image and the shape for
            /// it is in.
            /// </summary>
            public Section Section
            {
                get
                {
                    return this._Section;
                }
            }

            /// <summary>
            /// Gets the rigid body that this image is based off.
            /// </summary>
            public RigidBody Body
            {
                get
                {
                    return this._Body;
                }
            }

            /// <summary>
            /// Gets the velocity of the image in parent section units per second.
            /// </summary>
            public Vector Velocity
            {
                get
                {
                    return this._Vel;
                }
            }

            /// <summary>
            /// Gets the image that represents the state of the rigid body in the specified
            /// amount of seconds after this image.
            /// </summary>
            public Image Next(double Seconds)
            {
                return new Image(
                    this._Section.CreateRelation(Matrix.Translate(this._Vel * Seconds)), 
                    this._Body, this._Vel);
            }

            private Section _Section;
            private RigidBody _Body;
            private Vector _Vel;
        }

        private Vector _MassCenter;
        private IShape _Shape;
        private double _Mass;
    }
}
