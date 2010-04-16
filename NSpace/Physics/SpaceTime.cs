//----------------------------------------
// Copyright (c) 2010, Dmitry Zamkov 
// Open source under the new BSD License
//----------------------------------------
using System;
using System.Collections.Generic;

namespace NSpace.Physics
{
    /// <summary>
    /// An entity which stores spatial and temporal entities that can
    /// interact with each other based on their presence.
    /// </summary>
    public class SpaceTime : Entity
    {

    }

    /// <summary>
    /// Simple unoptimized implementation of a spacetime.
    /// </summary>
    public class SimpleSpaceTime : SpaceTime
    {
        public SimpleSpaceTime()
        {

        }

        public override IEnumerable<Entity> Causes
        {
            get 
            {
                return this._Contents; 
            }
        }

        private List<Entity> _Contents;
    }
}