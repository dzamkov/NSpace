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
    public class World : IBodyEventHandler
    {
        public World(IBody Body)
        {
            this._Body = Body;
            this._Body.Attach(this);
        }

        /// <summary>
        /// Gets a body that represents all the interactions and effects made by
        /// the world. Hint: this is probably a compound body.
        /// </summary>
        public IBody Body
        {
            get
            {
                return this._Body;
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
        /// Gets the current time in the world.
        /// </summary>
        public Time CurrentTime
        {
            get
            {
                return this._CurTime;
            }
        }

        public void OnReassign(IBody Old, IBody New)
        {
            if (Old == this._Body)
            {
                this._Body = New;
            }
        }

        public void OnModified(IBody Body)
        {
            
        }

        public void OnRemoved(IBody Body)
        {
            if(this._Body == Body)
            {
                this._Body = null;
            }
        }

        private IBody _Body;
        private Time _CurTime;
    }
}
