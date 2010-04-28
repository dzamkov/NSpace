//----------------------------------------
// Copyright (c) 2010, Dmitry Zamkov 
// Open source under the new BSD License
//----------------------------------------
using System;
using System.Collections.Generic;
using NSpace.Physics;

namespace NSpace.Visual
{
    /// <summary>
    /// An object that renders a volume(which can act as a complex scene) to a
    /// two dimensional image.
    /// </summary>
    public class Scene : IImmutable
    {
        public Scene(IVolume Volume, Section Camera)
        {
            this._Volume = Volume;
            this._Camera = Camera;
        }

        /// <summary>
        /// Gets the volume that is being rendered.
        /// </summary>
        public IVolume Volume
        {
            get
            {
                return this._Volume;
            }
        }

        /// <summary>
        /// Gets the section the camera is in. The camera will be at (0,0,0) in this section
        /// looking towards (1,0,0).
        /// </summary>
        public Section Camera
        {
            get
            {
                return this._Camera;
            }
        }

        /// <summary>
        /// Renders the volume to the current GL graphics context.
        /// </summary>
        public void Render()
        {

        }

        private IVolume _Volume;
        private Section _Camera;
    }
}