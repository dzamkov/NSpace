//----------------------------------------
// Copyright (c) 2010, Dmitry Zamkov 
// Open source under the new BSD License
//----------------------------------------
using System;
using System.Collections.Generic;

namespace NSpace.Physics
{
    /// <summary>
    /// A manager for a set a of bodies that can interact. While the bodies are all
    /// placed in space-time, the world is able to manage a single slice of time to insure
    /// all bodies are evaluated when needed.
    /// </summary>
    public class World
    {
        public World()
        {

        }

        /// <summary>
        /// Updates the world and objects within by the specified time in seconds.
        /// </summary>
        public void Update(TimeSpan Time)
        {
            this._CurTime += Time;
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

        private Time _CurTime;
    }
}
