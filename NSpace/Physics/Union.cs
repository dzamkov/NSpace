//----------------------------------------
// Copyright (c) 2010, Dmitry Zamkov 
// Open source under the new BSD License
//----------------------------------------
using System;
using System.Collections.Generic;

namespace NSpace.Physics
{
    /// <summary>
    /// A volume created from multiple other volumes. For every point in the union, the material
    /// is determined by going through the list of source volumes until one is in the point, and then
    /// taking its material.
    /// </summary>
    public class Union : IVolume
    {
        public Union(IEnumerable<IVolume> Source)
        {
            this._Source = Source;
        }

        /// <summary>
        /// Gets the source volumes that make up the union.
        /// </summary>
        public IEnumerable<IVolume> Source
        {
            get
            {
                return this._Source;
            }
        }

        ISurface IVolume.Surface
        {
            get 
            { 
                // NO, this is hard, im not gonna do it yet, NO
                throw new NotImplementedException(); 
            }
        }

        bool IVolume.InVolume(Event Event, ReferenceFrame Frame, ref IVolumeMaterial Material)
        {
            foreach (IVolume sourcevol in this._Source)
            {
                if (sourcevol.InVolume(Event, Frame, ref Material))
                {
                    return true;
                }
            }
            return false;
        }

        void IConvertible<IVolume>.Convert<D>(out D Result)
        {
            Result = this as D;
        }

        private IEnumerable<IVolume> _Source;      
    }
}