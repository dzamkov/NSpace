//----------------------------------------
// Copyright (c) 2010, Dmitry Zamkov 
// Open source under the new BSD License
//----------------------------------------
using System;
using System.Collections.Generic;
using System.Collections;

namespace NSpace
{
    /// <summary>
    /// Renderable object within a section that can be rendered by a view.
    /// </summary>
    public interface IVisual
    {
        /// <summary>
        /// Gets the section this visual is rendered in.
        /// </summary>
        Section Section { get; }

        /// <summary>
        /// Gets the area in which the visual can affect during rendering. The area is local
        /// to the section.
        /// </summary>
        Bound Bound { get; }

        /// <summary>
        /// Gets a context for this visual based on the supplied render information. ScreenTransform
        /// is a transform from the visual's local coordinates to screen coordinates. ScreenBounds is
        /// the bounding area for things draw in screen coordinates. Resolution is the amount of pixels
        /// the screen area occupies. The renderable will be rendered with the correct transformation
        /// applied to model view.
        /// </summary>
        IVisualContext GetContext(Matrix ScreenTransform, Bound ScreenBounds, double Resolution, View View);
    }


    /// <summary>
    /// A particular way to render a visual depending on the view, resolution and bounds of the visual
    /// applied to the view. This can be uniform for all types of views, giving a simple method of rendering
    /// that doesn't change depending on the way its viewed, or it can be varied and use LOD techinques.
    /// </summary>
    public interface IVisualContext
    {
        /// <summary>
        /// Gets the child visuals to be rendered alongside this visual. The visual system will go on 
        /// recursively through children when rendering until all visuals have been added to the render queue.
        /// </summary>
        IEnumerable<IVisual> Children { get; }

        /// <summary>
        /// Gets the renderable that can be used to render this visual context, or null if the visual context
        /// requires no rendering.
        /// </summary>
        IRenderable Renderable { get; }
    }

    /// <summary>
    /// A visual which renders the set of child visuals its given. This visual has
    /// no way of rendering anything on its own.
    /// </summary>
    public class MultiVisual : IVisual, IVisualContext
    {
        public MultiVisual(Section Section)
        {
            this._Section = Section;
            this._Visuals = new Dictionary<IVisual, object>();
        }

        /// <summary>
        /// Gets the visual's this multivisual renders. Because of the way this visual works,
        /// this is the same as Children.
        /// </summary>
        public IEnumerable<IVisual> Visuals
        {
            get
            {
                return this._Visuals.Keys;
            }
        }

        /// <summary>
        /// Adds a visual to be included when this visual is rendered.
        /// </summary>
        public void Add(IVisual Visual)
        {
            this._Visuals.Add(Visual, null);
        }

        /// <summary>
        /// Removes a previously added visual.
        /// </summary>
        public void Remove(IVisual Visual)
        {
            this._Visuals.Remove(Visual);
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
                Bound b = Bound.None;
                foreach (IVisual vis in this.Visuals)
                {
                    Matrix trans = vis.Section.GetRelation(this._Section).Inverse.SpaceTransform;
                    b = Bound.Union(b, Bound.Transform(trans, vis.Bound));
                }
                return b;
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
                return this.Visuals;
            }
        }

        public IRenderable Renderable
        {
            get
            {
                return null;
            }
        }

        private Dictionary<IVisual, object> _Visuals; // Visuals paired with local bounds.
        private Section _Section;
    }
}
