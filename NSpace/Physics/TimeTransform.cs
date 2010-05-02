//----------------------------------------
// Copyright (c) 2010, Dmitry Zamkov 
// Open source under the new BSD License
//----------------------------------------
using System;
using System.Collections.Generic;

namespace NSpace.Physics
{
    public class StaticTransformVolume : IStaticShape, IVolume
    {

    }

    /// <summary>
    /// A volume that is based on another transformed over time.
    /// </summary>
    public class TimeTransformVolume : IVolume
    {
        public TimeTransformVolume(IVolume Volume, Section<Matrix, Vector> Source, ITimeTransform Transform)
        {
            this._Volume = Volume;
            this._Source = Source;
            this._Transform = Transform;
        }

        /// <summary>
        /// Gets the source volume that is being transformed.
        /// </summary>
        public IVolume Volume
        {
            get
            {
                return this._Volume;
            }
        }

        /// <summary>
        /// Gets the section in the space of the source volume that is transformed
        /// to a destination section specified by the time transform.
        /// </summary>
        public Section<Matrix, Vector> Source
        {
            get
            {
                return this._Source;
            }
        }

        /// <summary>
        /// Gets the time transform that specifies how this volume moves over time.
        /// </summary>
        public ITimeTransform TimeTransform
        {
            get
            {
                return this._Transform;
            }
        }

        IStaticShape IShape.Slice(Time Time)
        {
            return this._Transform.Transform(Time);
        }

        ISurface IVolume.Surface
        {
            get 
            { 
                throw new NotImplementedException(); 
            }
        }

        bool IVolume.InVolume(Vector Point, Time Time, Section<Vector, Math> Section, ref IMaterial Material)
        {
            throw new NotImplementedException();
        }

        void IConvertible<IShape>.Convert<O>(out O Object)
        {
            throw new NotImplementedException();
        }

        private IVolume _Volume;
        private Section<Matrix, Vector> _Source;
        private ITimeTransform _Transform;
    }

    /// <summary>
    /// A transformation that changes over time.
    /// </summary>
    public interface ITimeTransform : IImmutable, IConvertible<ITimeTransform>
    {
        /// <summary>
        /// Given a time, this function returns a section to place an object
        /// in.
        /// </summary>
        Section<Vector, Matrix> Transform(Time Time);
    }
}