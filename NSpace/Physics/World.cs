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
    public class World : PhysicsObject, ICompositeObject
    {
        public World()
        {
            this._Objects = new Marker();
        }

        public override TimeBound TimeBound
        {
            get 
            {
                return TimeBound.Huge;
            }
        }

        public override void Interact(PhysicsObject Other)
        {
            foreach (PhysicsObject physobj in this._Objects.Objects)
            {
                physobj.Interact(Other);
            }
        }

        public IEnumerable<PhysicsObject> Parts
        {
            get 
            {
                return this._Objects.Objects;
            }
        }

        /// <summary>
        /// Updates the world and objects within by the specified time in seconds.
        /// </summary>
        public void Update(TimeSpan Time)
        {
            Time nexttime = this._CurTime + Time;
            foreach (PhysicsObject o in this._Objects.GetObjects(this._CurTime))
            {
                if (nexttime > o.TimeBound.TimeEnd)
                {
                    IExtendableObject ext = o as IExtendableObject;
                    ext.Extend(new TimeSpan(5.0), this);
                }
            }
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

        /// <summary>
        /// Adds a physics object to be processed directly by the world. This only
        /// needs to be called on an initial physics object just added to the world.
        /// </summary>
        public void AddPhysicsObject(PhysicsObject Object)
        {
            this._Objects.Mark(Object);
        }

        private Time _CurTime;
        private Marker _Objects;
    }
}
