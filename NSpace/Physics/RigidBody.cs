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
        public RigidBody(Section Section, double Mass, Vector MassCenter, IShape Shape)
        {
            this._Force = new ConstantCurve(new Vector(0.0, 0.0, -9.8));
            this._Velocity = this._Force.Integral(new Vector(0.0, 0.0, 0.0));

            this._Section = Section;
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
        /// Gets the section for this body at the specified world time.
        /// </summary>
        public Section GetSection(double Time)
        {
            return this._Section.CreateRelation(Matrix.Translate(this._Velocity.Integral(new Vector(0.0, 0.0, 0.0)).GetPoint(Time)));
        }

        private Section _Section;
        private ICurve _Velocity;
        private ICurve _Force;
        private Vector _MassCenter;
        private IShape _Shape;
        private double _Mass;
    }
}
