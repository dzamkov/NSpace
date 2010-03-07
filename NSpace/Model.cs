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
    public class Model : IVisual, IVisualContext
    {
        public Model(Mesh Mesh, Material Material, Section Section)
        {
            this.Mesh = Mesh;
            this.Material = Material;
            this._Section = Section;
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
        /// Creates a new model with the specified mesh and material in the
        /// specified section.
        /// </summary>
        public static Model Create(Mesh Mesh, Material Material, Section Section)
        {
            return new Model(Mesh, Material, Section);
        }

        public Section Section
        {
            get 
            {
                return this._Section;
            }
            set
            {
                this._Section = value;
            }
        }

        public Bound Bound
        {
            get 
            {
                return this._Mesh.Bound; 
            }
        }

        public IVisualContext GetContext(Matrix ScreenTransform, Bound ScreenBounds, double Resolution, View View)
        {
            return this;
        }

        public IEnumerable<IVisual> Children
        {
            get 
            {
                return null;
            }
        }

        public IRenderable Renderable
        {
            get 
            {
                return this._Material.Renderable;
            }
        }

        private Mesh _Mesh;
        private Material _Material;
        private Section _Section;
    }
}
