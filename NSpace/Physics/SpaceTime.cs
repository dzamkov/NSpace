//----------------------------------------
// Copyright (c) 2010, Dmitry Zamkov 
// Open source under the new BSD License
//----------------------------------------
using System;
using System.Collections.Generic;

namespace NSpace.Physics
{
    /// <summary>
    /// Contains spatially and temporally organized bodies. Spacetimes can be modified by
    /// modifing contained spatial bodies, or adding and removing them.
    /// </summary>
    public interface ISpaceTime : IBody
    {
        /// <summary>
        /// Gets the bodies in the spacetime.
        /// </summary>
        IEnumerable<ISpatialBody> Bodies { get; }

        /// <summary>
        /// Adds a spatial body to the spacetime, organizing it in the process.
        /// </summary>
        void Add(ISpatialBody Body);

        /// <summary>
        /// Removes a previously added body from the spacetime.
        /// </summary>
        void Remove(ISpatialBody Body);
    }

    /// <summary>
    /// A body that can be organized in spacetime.
    /// </summary>
    public interface ISpatialBody : IBody
    {
 
    }

    /// <summary>
    /// A simple unoptimized implementation of a spacetime.
    /// </summary>
    public class SpaceTime : ISpaceTime, IBodyEventHandler
    {
        public SpaceTime()
        {
            this._Contents = new List<ISpatialBody>();
            this._BodyEventHandlers = new List<IBodyEventHandler>();
        }

        public IEnumerable<ISpatialBody> Bodies
        {
            get 
            {
                return this._Contents;
            }
        }

        public void Add(ISpatialBody Body)
        {
            this._Contents.Add(Body);
            foreach (IBodyEventHandler beh in this._BodyEventHandlers)
            {
                beh.OnModified(this);
            }
        }

        public void Remove(ISpatialBody Body)
        {
            this._Contents.Remove(Body);
            foreach (IBodyEventHandler beh in this._BodyEventHandlers)
            {
                beh.OnModified(this);
            }
        }

        public void Attach(IBodyEventHandler EventHandler)
        {
            this._BodyEventHandlers.Add(EventHandler);
        }

        public void Detach(IBodyEventHandler EventHandler)
        {
            this._BodyEventHandlers.Remove(EventHandler);
        }

        public void OnReassign(IBody Old, IBody New)
        {
            this._Contents.Remove((ISpatialBody)Old);
            this._Contents.Add((ISpatialBody)New);
        }

        public void OnModified(IBody Body)
        {
            throw new NotImplementedException();
        }

        private List<ISpatialBody> _Contents;
        private List<IBodyEventHandler> _BodyEventHandlers;
    }
}