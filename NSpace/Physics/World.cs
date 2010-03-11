//----------------------------------------
// Copyright (c) 2010, Dmitry Zamkov 
// Open source under the new BSD License
//----------------------------------------
using System;
using System.Collections.Generic;

namespace NSpace.Physics
{
    /// <summary>
    /// A set of objects and the rules governing their interactions.
    /// </summary>
    public class World
    {
        public World()
        {
            this._Objects = new Marker();
        }

        /// <summary>
        /// Updates the world and objects within by the specified time in seconds.
        /// </summary>
        public void Update(double Time)
        {

        }

        /// <summary>
        /// Adds a physics object to be processed directly by the world.
        /// </summary>
        public void AddPhysicsObject(PhysicsObject Object)
        {
            this._Objects.Mark(Object);
        }

        private Marker _Objects;
    }
}
