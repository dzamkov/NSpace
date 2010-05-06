//----------------------------------------
// Copyright (c) 2010, Dmitry Zamkov 
// Open source under the new BSD License
//----------------------------------------
using System;
using System.Collections.Generic;

namespace NSpace.Physics
{
    /// <summary>
    /// A volume created from a volume subtracted by another. The points in the volume
    /// are those not occupied by the Subtrahend while stilling being occupied by the
    /// Minuend.
    /// </summary>
    public class Difference : IVolume
    {
        public Difference(IVolume Minuend, IVolume Subtrahend)
        {
            this._Minuend = Minuend;
            this._Subtrahend = Subtrahend;
        }

        ISurface IVolume.Surface
        {
            get 
            {
                return new DifferenceSurface(this._Minuend.Surface, this._Subtrahend.Surface); 
            }
        }

        bool IVolume.InVolume(Event Event, ReferenceFrame Frame, ref IVolumeMaterial Material)
        {
            IVolumeMaterial dummy = null;
            if (!this._Subtrahend.InVolume(Event, Frame, ref dummy))
            {
                return this._Minuend.InVolume(Event, Frame, ref Material);
            }
            else
            {
                return false;
            }
        }

        void IConvertible<IVolume>.Convert<D>(out D Result)
        {
            Result = this as D;
        }

        private IVolume _Minuend;
        private IVolume _Subtrahend;
    }

    /// <summary>
    /// Difference of two intersecting surfaces. The material of the subtrahend
    /// is used on the newly created parts of surface.
    /// </summary>
    public class DifferenceSurface : ISurface
    {
        public DifferenceSurface(ISurface Minuend, ISurface Subtrahend)
        {
            this._Minuend = Minuend;
            this._Subtrahend = Subtrahend;
        }

        /// <summary>
        /// Gets the base surface to use before difference.
        /// </summary>
        public ISurface Minuend
        {
            get
            {
                return this._Minuend;
            }
        }

        /// <summary>
        /// Gets the surface to "cut in" the minuend.
        /// </summary>
        public ISurface Subtrahend
        {
            get
            {
                return this._Subtrahend;
            }
        }

        void IConvertible<ISurface>.Convert<D>(out D Result)
        {
            if (typeof(D) == typeof(IStaticMesh))
            {
                // Convert difference to mesh
                // But i wont do it now
                // because its hard
                throw new NotImplementedException();
            }
            Result = this as D;
        }

        private ISurface _Minuend;
        private ISurface _Subtrahend;
    }
}