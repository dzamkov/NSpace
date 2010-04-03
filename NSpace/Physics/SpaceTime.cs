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
    public interface ISpaceTime
    {
        /// <summary>
        /// Gets the bodies in the spacetime.
        /// </summary>
        IEnumerable<IBody> Bodies { get; }

        /// <summary>
        /// Adds a spatial body to the spacetime, organizing it in the process. The spacetime
        /// may check that Body.SpaceTime is an expected value before adding the body.
        /// </summary>
        void Add(IBody Body);

        /// <summary>
        /// Gets all bodes of the specified type in the spacetime.
        /// </summary>
        void FindByType<T>(out IEnumerable<T> Results) where T : IBody;
    }

    /// <summary>
    /// A simple unoptimized implementation of a spacetime.
    /// </summary>
    public class SpaceTime : ISpaceTime, IBodyEventHandler
    {
        public SpaceTime()
        {
            this._Contents = new List<IBody>();
        }

        public IEnumerable<IBody> Bodies
        {
            get 
            {
                return this._Contents;
            }
        }

        public void Add(IBody Body)
        {
            this._Contents.Add(Body);
        }

        public void FindByType<T>(out IEnumerable<T> Results) where T : IBody
        {
            List<T> res = new List<T>();
            Results = res;
            foreach(IBody b in this._Contents)
            {
                if(b is T)
                {
                    res.Add((T)b);
                }
            }
        }

        public void OnReassign(IBody Old, IBody New)
        {
            this._Contents.Remove(Old);
            this._Contents.Add(New);
        }

        public void OnModified(IBody Body)
        {
            throw new NotImplementedException();
        }

        public void OnRemoved(IBody Body)
        {
            this._Contents.Remove(Body);
        }

        private List<IBody> _Contents;
    }
}