//----------------------------------------
// Copyright (c) 2010, Dmitry Zamkov 
// Open source under the new BSD License
//----------------------------------------
using System;
using System.Collections.Generic;

namespace NSpace.Physics
{
    /// <summary>
    /// A body which applies a uniform force to all other bodies in a world that forces
    /// the bodies in a direction.
    /// </summary>
    public class Gravity : IBody
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

        /// <summary>
        /// Gets the force vector of the force applied to a specific section.
        /// </summary>
        public Vector ForceAtSection(Section Section)
        {
            return this._Section.GetRelation(Section).LinearTransform(this._Force);
        }

        public void Attach(IBodyEventHandler EventHandler)
        {
            
        }

        public void Detach(IBodyEventHandler EventHandler)
        {
            
        }

        private Vector _Force;
        private Section _Section;
    }
}
