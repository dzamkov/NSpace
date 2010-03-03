//----------------------------------------
// Copyright (c) 2010, Dmitry Zamkov 
// Open source under the new BSD License
//----------------------------------------
using System;
using System.Collections.Generic;

namespace NSpace.Physics
{
    /// <summary>
    /// An area containing a set of physics objects and the rules governing their
    /// interactions.
    /// </summary>
    public class World
    {
        public World(Section Section)
        {
            this._Section = Section;
        }

        /// <summary>
        /// Updates the world and objects within by the specified time in seconds.
        /// </summary>
        public void Update(double Time)
        {

        }

        /// <summary>
        /// Gets the section this world runs its simulations in. This is not relevant to
        /// how objects in the world are handled but must be specified to provide a common
        /// coordinate system between objects.
        /// </summary>
        public Section Section
        {
            get
            {
                return this._Section;
            }
        }

        private Section _Section;
    }
}
