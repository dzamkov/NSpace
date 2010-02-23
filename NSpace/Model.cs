//----------------------------------------
// Copyright (c) 2010, Dmitry Zamkov 
// Open source under the new BSD License
//----------------------------------------
using System;
using System.Collections.Generic;
using OpenTK;
using OpenTK.Graphics.OpenGL;

namespace NSpace
{
    /// <summary>
    /// A representation of an object made from a mesh that has a material attached.
    /// </summary>
    public class Model : IRenderable
    {
        public Model()
        {
 
        }

        public Model(Mesh Mesh, Material Material)
        {
            this.Mesh = Mesh;
            this.Material = Material;
        }

        /// <summary>
        /// Gets or sets the mesh used for this model.
        /// </summary>
        public Mesh Mesh
        {
            get
            {
                return this._Mesh;
            }
            set
            {
                this._Mesh = value;
                if (this._Material != null)
                {
                    this._Material.Mesh = this._Mesh;
                }
            }
        }

        /// <summary>
        /// Gets or sets the material to be used for this model. When setting a material, it
        /// must not currently be in use (Mesh should be null).
        /// </summary>
        public Material Material
        {
            get
            {
                return this._Material;
            }
            set
            {
                if (value.Mesh == null)
                {
                    if (this._Material != null)
                    {
                        this._Material.Mesh = null;
                    }
                    this._Material = value;
                    if(this._Mesh != null)
                    {
                        this._Material.Mesh = this._Mesh;
                    }
                }
                else
                {
                    throw new Exception("Material must not be in use.");
                }
            }
        }

        /// <summary>
        /// Creates a new model with the specified mesh and material.
        /// </summary>
        public static Model Create(Mesh Mesh, Material Material)
        {
            return new Model(Mesh, Material);
        }

        public void Render()
        {
            this._Material.Render();
        }

        private Mesh _Mesh;
        private Material _Material;
    }
}
