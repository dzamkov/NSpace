//----------------------------------------
// Copyright (c) 2010, Dmitry Zamkov 
// Open source under the new BSD License
//----------------------------------------
using System;
using System.Collections.Generic;
using NSpace.Physics;

using OpenTK;
using OpenTK.Graphics.OpenGL;

namespace NSpace.Visual
{
    /// <summary>
    /// A surface material that has visual properties that determine how it affects light
    /// that hits it.
    /// </summary>
    public interface IVisualMaterial : ISurfaceMaterial
    {
        /// <summary>
        /// A very, very complicated function describing how light affects the material in a certain situation. Hopefully
        /// this function is complicated enough to scare you from creating your own visual material class and not just using
        /// one of the classes that are already there and are optimized.
        /// </summary>
        /// <param name="LightInRay">The unit vector that describes the direction towards which the light is coming from.</param>
        /// <param name="LightOutRay">The unit vector that describes where the light is going to.</param>
        /// <param name="SurfaceNormal">The normal of the surface(of this material) that the light is hitting.</param>
        /// <param name="LightColor">The color of the incoming light.</param>
        /// <param name="Irradiance">The intensity of the incoming light in watts per square meter.</param>
        /// <param name="ResultColor">The color of the outgoing light due to effects from the material.</param>
        /// <param name="Radiosity">The total energy of the light leaving the surface in watts per square meter.</param>
        /// 
        void CalculateLightRay(
            Vector LightInRay, 
            Vector LightOutRay, 
            Vector SurfaceNormal, 
            Color LightColor,
            double Irradiance,
            out Color ResultColor, 
            out double Radiosity);
    }

    /// <summary>
    /// A visual material that will always be the same color with the same intensity and completely ignore all
    /// incoming light. It might emit light, if there is no incoming light, or it might end up absorbing light
    /// if there is a large amount of incoming light.
    /// </summary>
    public class SolidColorMaterial : IVisualMaterial
    {
        public SolidColorMaterial(Color Color, double Radiosity)
        {
            this._Color = Color;
            this._Radiosity = Radiosity;
        }

        /// <summary>
        /// The color that this material is and will always be.
        /// </summary>
        public Color Color
        {
            get
            {
                return this._Color;
            }
        }

        /// <summary>
        /// The amount of light that comes out of this material in watts per square meter.
        /// </summary>
        /// <remarks>A light bulb has a radiosity of 0.1w/(m^2)</remarks>
        public double Radiosity
        {
            get
            {
                return this._Radiosity;
            }
        }

        void IVisualMaterial.CalculateLightRay(
            Vector LightInRay, 
            Vector LightOutRay, 
            Vector SurfaceNormal, 
            Color LightColor, 
            double Irradiance, 
            out Color ResultColor, 
            out double Radiosity)
        {
            // SCREW THE INPUTS! We already know exactly what we need to.
            ResultColor = this._Color;
            Radiosity = this._Radiosity;
        }

        private Color _Color;
        private double _Radiosity;
    }
}