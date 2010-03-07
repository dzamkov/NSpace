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
            this._Bodies = new Dictionary<RigidBody, RigidBody.Image>();
        }

        /// <summary>
        /// Gets the current image for a body.
        /// </summary>
        public RigidBody.Image this[RigidBody Body]
        {
            get
            {
                return this._Bodies[Body];
            }
        }

        /// <summary>
        /// Updates the world and objects within by the specified time in seconds.
        /// </summary>
        public void Update(double Time)
        {
            Dictionary<RigidBody, RigidBody.Image> next = new Dictionary<RigidBody, RigidBody.Image>();
            foreach (RigidBody.Image im in this._Bodies.Values)
            {
                RigidBody.Image nextim = im.Next(Time);
                next.Add(nextim.Body, nextim);
            }
            this._Bodies = next;
        }

        /// <summary>
        /// Adds a rigid body to the world.
        /// </summary>
        public void AddBody(RigidBody.Image Image)
        {
            this._Bodies.Add(Image.Body, Image);
        }

        private Dictionary<RigidBody, RigidBody.Image> _Bodies;
    }
}
