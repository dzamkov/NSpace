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
    public abstract class DebugVisual : Section, IVisual, IVisualContext
    {
        public DebugVisual()
        {

        }

        public abstract IRenderable Renderable { get; }

        public override IVisual Visual
        {
            get
            {
                return this;
            }
        }

        public IEnumerable<Section> RenderSections
        {
            get 
            {
                return null;
            }
        }

        public IVisualContext GetContext(Matrix ScreenTransform, Bound ScreenBounds, double Resolution, View View)
        {
            return this;
        }

        /// <summary>
        /// Draws a line between two points.
        /// </summary>
        public class Line : DebugVisual, IRenderable
        {
            public Line(Vector PointA, Vector PointB)
            {
                this.PointA = PointA;
                this.PointB = PointB;
                this.Color = Color.RGB(1.0, 0.0, 0.0);
                this.Bound = new Bound(new Vector[] { this.PointA, this.PointB });
            }

            public override IRenderable Renderable
            {
                get 
                {
                    return this;
                }
            }

            public void Render()
            {
                GL.Begin(BeginMode.Lines);
                GL.Color4(this.Color);
                GL.Vertex3(this.PointA);
                GL.Vertex3(this.PointB);
                GL.End();
            }

            public Vector PointA;
            public Vector PointB;
            public Color Color;
        }

        /// <summary>
        /// Creates a line between the specified positions with the specified parent section.
        /// </summary>
        public static DebugVisual CreateLine(Vector PointA, Vector PointB, ComplexSection Parent)
        {
            Line l = new Line(PointA, PointB);
            Parent.AddChild(l, Matrix.Identity);
            return l;
        }
    }
}
