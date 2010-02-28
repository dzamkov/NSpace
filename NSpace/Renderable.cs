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
    /// An object that can be rendered, or that can otherwise affect the graphics context directly.
    /// </summary>
    public interface IRenderable
    {
        /// <summary>
        /// Renders to the current graphics context. All settings set within render must be reverted
        /// before the method exits.
        /// </summary>
        void Render();
    }

    /// <summary>
    /// A renderable object that can have other renderables within it. The nested renderables
    /// can be rendered in between setting and restoring of a property of the graphics context.
    /// </summary>
    public abstract class NestedRenderable : IRenderable
    {
        /// <summary>
        /// Does rendering before the nested renderables.
        /// </summary>
        public virtual void PreRender()
        {
        }

        /// <summary>
        /// Does rendering after the nested renderables.
        /// </summary>
        public virtual void PostRender()
        {
        }

        /// <summary>
        /// Gets the renderables within this nested renderable in order of rendering.
        /// </summary>
        public abstract IEnumerable<IRenderable> Nested { get; }

        public void Render()
        {
            this.PreRender();
            foreach (IRenderable ren in this.Nested)
            {
                ren.Render();
            }
            this.PostRender();
        }
    }

    /// <summary>
    /// A renderable that renders a set of other renderables. Renderables do not
    /// need to be rendered in a particular order.
    /// </summary>
    public class SetRenderable : NestedRenderable
    {
        public SetRenderable()
        {
            this._Renderables = new List<IRenderable>();
        }

        /// <summary>
        /// Adds a renderable to this set.
        /// </summary>
        public void Add(IRenderable Renderable)
        {
            this._Renderables.Add(Renderable);
        }

        /// <summary>
        /// Removes a renderable from this set.
        /// </summary>
        public void Remove(IRenderable Renderable)
        {
            this._Renderables.Remove(Renderable);
        }

        public override IEnumerable<IRenderable> Nested
        {
            get 
            {
                return this._Renderables;
            }
        }

        private List<IRenderable> _Renderables;
    }

    /// <summary>
    /// Sets some graphics capabilities for the renderables within it.
    /// </summary>
    public class CapabilityRenderable : NestedRenderable
    {
        public CapabilityRenderable(IEnumerable<EnableCap> Caps, IRenderable Inner)
        {
            this._Inner = Inner;
            this._Caps = new List<EnableCap>();
            foreach (EnableCap ec in Caps)
            {
                this._Caps.Add(ec);
            }
        }

        /// <summary>
        /// Gets or sets the renderable rendered with the specified capibilities.
        /// </summary>
        public IRenderable Inner
        {
            get
            {
                return this._Inner;
            }
        }

        public override IEnumerable<IRenderable> Nested
        {
            get 
            {
                yield return this._Inner;
            }
        }

        /// <summary>
        /// Gets the capabilities set within this renderable.
        /// </summary>
        public IEnumerable<EnableCap> Capabilities
        {
            get
            {
                return this._Caps;
            }
        }

        public override void PreRender()
        {
            foreach (EnableCap ec in this._Caps)
            {
                GL.Enable(ec);
            }
        }

        public override void PostRender()
        {
            foreach (EnableCap ec in this._Caps)
            {
                GL.Disable(ec);
            }
        }

        private IRenderable _Inner;
        private List<EnableCap> _Caps;
    }
}
