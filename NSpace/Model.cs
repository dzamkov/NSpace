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
    public class Model : Section
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
                if (this._Mesh != null)
                {
                    this.Bound = this._Mesh.Bound;
                }
                else
                {
                    this.Bound = Bound.None;
                }
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
        /// Visual for a model.
        /// </summary>
        public class ModelVisual : IVisual, IVisualContext
        {
            public ModelVisual(Material Material)
            {
                this.Mat = Material;
            }

            IVisualContext IVisual.GetContext(Matrix ScreenTransform, Bound ScreenBounds, double Resolution, View View)
            {
                return this;
            }

            IEnumerable<Section> IVisualContext.RenderSections
            {
                get 
                {
                    return null;
                }
            }

            void IRenderable.Render()
            {
                this.Mat.Render();
            }

            public Material Mat;
        }

        public override IVisual Visual
        {
            get
            {
                if (this._Material != null && this._Material.Mesh != null)
                {
                    return new ModelVisual(this._Material);
                }
                else
                {
                    return null;
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

        private Mesh _Mesh;
        private Material _Material;
    }
}
