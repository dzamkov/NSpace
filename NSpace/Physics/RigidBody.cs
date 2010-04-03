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
            this._Gravity = new Dictionary<Gravity, object>();
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
        /// Creates a body with the specified parameters and adds it to a world.
        /// </summary>
        public static RigidBody Create(ISpaceTime SpaceTime, Section Section, Property Properties)
        {
            RigidBody rb = new RigidBody();
            rb._InitVelocity = new Vector();
            rb._TimeBound = new TimeBound(0.0, 5.0);
            rb._Section = Section;
            rb._Properties = Properties;
            rb._RecalculatePath();
            SpaceTime.Add(rb);
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

        public void Attach(IBodyEventHandler EventHandler)
        {
            
        }

        public void Detach(IBodyEventHandler EventHandler)
        {
            
        }

        /// <summary>
        /// Recalculates the path of the body over time based on interactions.
        /// </summary>
        private void _RecalculatePath()
        {
            ICurve force = new ConstantCurve(new Vector()); // Assuming 0 mass
            foreach (Gravity g in this._Gravity.Keys)
            {
                force = new SumCurve(force, new ConstantCurve(g.ForceAtSection(this.Section)));
            }

            ICurve velocity = force.Integral(this._InitVelocity);

            this._Position = velocity.Integral(new Vector(0.0, 0.0, 0.0));
        }

        /// <summary>
        /// Applies gravity so that it affects the rigid body.
        /// </summary>
        internal void _ApplyGravity(Gravity Gravity)
        {
            this._Gravity[Gravity] = null;
            this._RecalculatePath();
        }

        private TimeBound _TimeBound;
        private ICurve _Position;
        private Vector _InitVelocity;
        private Dictionary<Gravity, object> _Gravity;
        private Section _Section;
        private Property _Properties;
    }

    /// <summary>
    /// Interaction between rigid bodies and gravity.
    /// </summary>
    public class RigidBodyGravityInteraction : IInteractProcedure
    {
        public void ApplyInteractions(ISpaceTime SpaceTime)
        {
            IEnumerable<RigidBody> rbs; SpaceTime.FindByType<RigidBody>(out rbs);
            IEnumerable<Gravity> gravs; SpaceTime.FindByType<Gravity>(out gravs);
            foreach (RigidBody rb in rbs)
            {
                foreach (Gravity grav in gravs)
                {
                    rb._ApplyGravity(grav);
                }
            }
        }
    }
}