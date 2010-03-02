//----------------------------------------
// Copyright (c) 2010, Dmitry Zamkov 
// Open source under the new BSD License
//----------------------------------------
using System;
using System.Collections.Generic;
using OpenTK;
using Color4 = OpenTK.Graphics.Color4;
using OpenTK.Graphics.OpenGL;

namespace NSpace
{
    /// <summary>
    /// Contains visuals that are useful in debugging.
    /// </summary>
    public abstract class DebugVisual : IVisual, IVisualContext
    {
        public DebugVisual(Section Section)
        {
            this._Section = Section;
        }

        public abstract IRenderable Renderable { get; }

        public abstract Bound Bound { get; }

        public Section Section 
        {
            get
            {
                return this._Section;
            }
        }

        public IVisualContext GetContext(Matrix ScreenTransform, Bound ScreenBounds, double Resolution, View View)
        {
            return this;
        }

        public virtual IEnumerable<IVisual> Children
        {
            get
            {
                return null;
            }
        }

        /// <summary>
        /// Draws a line between two points.
        /// </summary>
        public class Line : DebugVisual, IRenderable
        {
            public Line(Vector PointA, Vector PointB, Section Section) : base(Section)
            {
                this._PointA = PointA;
                this._PointB = PointB;
                this._Section = Section;
                this._Color = Color.RGB(1.0, 0.0, 0.0);
            }

            public override IRenderable Renderable
            {
                get 
                {
                    return this;
                }
            }

            public override Bound Bound
            {
                get 
                {
                    return new Bound(new Vector[] { this._PointA, this._PointB });
                }
            }

            public void Render()
            {
                GL.Begin(BeginMode.Lines);
                GL.Color4(this._Color);
                GL.Vertex3(this._PointA);
                GL.Vertex3(this._PointB);
                GL.End();
            }

            private Vector _PointA;
            private Vector _PointB;
            private Color _Color;
        }

        /// <summary>
        /// Creates a line between the specified positions with the specified parent section.
        /// </summary>
        public static DebugVisual CreateLine(Vector PointA, Vector PointB, Section Parent)
        {
            Line l = new Line(PointA, PointB, Parent);
            return l;
        }

        private Section _Section;
    }
}
