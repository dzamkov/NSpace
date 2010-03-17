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
    public class World : ICompoundBody
    {
        public World()
        {
            this._Bodies = new List<IBody>();
        }

        public void Interact(IBody Body)
        {

        }

        public IEnumerable<IBody> Bodies
        {
            get
            {
                return this._Bodies;
            }
        }

        /// <summary>
        /// Updates the world and objects within by the specified time in seconds.
        /// </summary>
        public void Update(TimeSpan Time)
        {
            Time nexttime = this._CurTime + Time;
            this._CurTime = nexttime;
        }

        /// <summary>
        /// Adds a body to the world so it can begin interactions on objects
        /// in it.
        /// </summary>
        public void AddBody(IBody Body)
        {
            this._Bodies.Add(Body);
        }

        /// <summary>
        /// Gets the current time in the world.
        /// </summary>
        public Time CurrentTime
        {
            get
            {
                return this._CurTime;
            }
        }

        List<IBody> _Bodies;
        private Time _CurTime;
    }
}
