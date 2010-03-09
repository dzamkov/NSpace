﻿//----------------------------------------
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

        }

        /// <summary>
        /// Updates the world and objects within by the specified time in seconds.
        /// </summary>
        public void Update(double Time)
        {
            this._CurTime += Time;
        }

        /// <summary>
        /// Gets the current time in seconds in the world.
        /// </summary>
        public double CurrentTime
        {
            get
            {
                return this._CurTime;
            }
        }

        private double _CurTime;
    }
}
