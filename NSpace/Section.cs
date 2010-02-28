//----------------------------------------
// Copyright (c) 2010, Dmitry Zamkov 
// Open source under the new BSD License
//----------------------------------------
using System;
using System.Collections.Generic;

namespace NSpace
{
    /// <summary>
    /// A volume of space that can have visual and dynamic properties. Sections
    /// can be parented to other sections to establish a relation between the parents
    /// and sibblings in coordinate space.
    /// </summary>
    public class Section
    {
        public Section()
        {
            this._Bound = Bound.None;
        }

        /// <summary>
        /// Gets or sets a transform that, when applied to local space, will transform to the coordinate
        /// space of the parent.
        /// </summary>
        public Matrix ParentTransform
        {
            get
            {
                return this._ParentTransform;
            }
            set
            {
                this._ParentTransform = value;
                this._IParentTransform = value.Inverse();
                if (this._Parent != null)
                {
                    this._Parent._UpdateRelation(this);
                }
            }
        }

        /// <summary>
        /// Gets or sets a transform that, when applied to the coordinate space of the parent, will transform to local space.
        /// </summary>
        public Matrix InverseParentTransform
        {
            get
            {
                return this._IParentTransform;
            }
            set
            {
                this._ParentTransform = value.Inverse();
                this._IParentTransform = value;
                if (this._Parent != null)
                {
                    this._Parent._UpdateRelation(this);
                }
            }
        }

        /// <summary>
        /// Gets the matrix that, when multiplied by local space, will get the space of
        /// the specified section.
        /// </summary>
        public Matrix GetRelation(Section To)
        {
            if (this == To)
            {
                return Matrix.Identity;
            }
            if (this.Level == To.Level)
            {
                if (this._Parent == To._Parent)
                {
                    return Matrix.Transform(To.InverseParentTransform, this.ParentTransform);
                }
                else
                {
                    return Matrix.Transform(Matrix.Transform(To.InverseParentTransform, this.Parent.GetRelation(To.Parent)), this.ParentTransform);
                }
            }
            if (this.Level > To.Level)
            {
                return Matrix.Transform(this.Parent.GetRelation(To), this.ParentTransform);
            }
            else
            {
                return Matrix.Transform(To.InverseParentTransform, this.GetRelation(To.Parent));
            }
        }

        /// <summary>
        /// Gets how many levels deep into the section tree this section is. This will be
        /// 0 at the root section, 1 at a child of the root section, 2 if its the child of
        /// a child of the root section and so on.
        /// </summary>
        public int Level
        {
            get
            {
                if (this._Parent == null)
                {
                    return 0;
                }
                else
                {
                    return this._Parent.Level + 1;
                }
            }
        }

        /// <summary>
        /// Gets the root parent of this section. The is the lowest-level indirect or direct parent
        /// of this section.
        /// </summary>
        public Section RootParent
        {
            get
            {
                if (this._Parent == null)
                {
                    return this;
                }
                else
                {
                    return this._Parent.RootParent;
                }
            }
        }

        /// <summary>
        /// Gets the parent section for this section. The parent
        /// should be overall larger then the child and have siblings of
        /// about the same size. This system is used to organize objects
        /// in space.
        /// </summary>
        public ComplexSection Parent
        {
            get
            {
                return this._Parent;
            }
        }

        /// <summary>
        /// Gets or sets the bounds of the section, which contains all things and volume it can
        /// affect.
        /// </summary>
        public Bound Bound 
        {
            get
            {
                return this._Bound;
            }
            set
            {
                this._Bound = value;
                if (this._Parent != null)
                {
                    this._Parent._UpdateBound(this);
                }
            }
        }

        /// <summary>
        /// Gets the visual of this section, which renders the contents of the
        /// section. If null is returned, this section cannot be rendered.
        /// </summary>
        public virtual IVisual Visual
        {
            get
            {
                return null;
            }
        }

        internal ComplexSection _Parent;
        internal Bound _ParentBound;
        internal Matrix _ParentTransform;
        internal Matrix _IParentTransform;
        private Bound _Bound;
    }

    /// <summary>
    /// A section that can have child sections.
    /// </summary>
    public class ComplexSection : Section
    {
        public ComplexSection()
        {
            this._Children = new Dictionary<Section, object>();
        }

        /// <summary>
        /// Gets the little children sections of this section.
        /// </summary>
        public IEnumerable<Section> Children
        {
            get
            {
                return this._Children.Keys;
            }
        }

        /// <summary>
        /// Adds a section as a child to this section. The child must
        /// not yet have a parent section. The child is set to have the
        /// specified transform offset from this section.
        /// </summary>
        public void AddChild(Section Child, Matrix Transform)
        {
            if (Child.Parent == null)
            {
                Child._Parent = this;
                Child._IParentTransform = Transform;
                Child._ParentTransform = Transform.Inverse();
                this._Children.Add(Child, null);
                this._UpdateBound(Child);
            }
        }

        /// <summary>
        /// Updates the bounds of a child section.
        /// </summary>
        internal void _UpdateBound(Section Child)
        {
            this._Children[Child] = this._GetBound(Child);
            this._CreateBound();
        }
        
        /// <summary>
        /// Updates the relation of a child section.
        /// </summary>
        internal void _UpdateRelation(Section Child)
        {
            this._UpdateBound(Child);
        }

        /// <summary>
        /// Gets the bounds for a child section.
        /// </summary>
        private Bound _GetBound(Section Child)
        {
            return Bound.Transform(Child._ParentTransform, Child.Bound);
        }

        /// <summary>
        /// Creates the bound for this complex section based on child sections.
        /// </summary>
        private void _CreateBound()
        {
            Bound a = Bound.None;
            foreach (Section c in this.Children)
            {
                a = Bound.Union(a, c._ParentBound);
            }
            this.Bound = a;
        }

        /// <summary>
        /// Visual for complex sections, which by default renders child sections.
        /// </summary>
        private class ComplexVisual : IVisual, IVisualContext
        {
            public ComplexVisual(ComplexSection For)
            {
                this.For = For;
            }

            IVisualContext IVisual.GetContext(Matrix ScreenTransform, Bound ScreenBounds, double Resolution, View View)
            {
                return this;
            }

            IEnumerable<Section> IVisualContext.RenderSections
            {
                get 
                {
                    return this.For.Children;
                }
            }

            IRenderable IVisualContext.Renderable
            {
                get
                {
                    return null;
                }
            }

            public ComplexSection For;
        }

        public override IVisual Visual
        {
            get
            {
                return new ComplexVisual(this);
            }
        }

        private Dictionary<Section, object> _Children;
    }

    /// <summary>
    /// Renerable within a section that can be rendered by a view.
    /// </summary>
    public interface IVisual
    {
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
        /// Gets the sections that must be rendered alongside this visual context. These must be some subset
        /// of child sections of the section this visual is for.
        /// </summary>
        IEnumerable<Section> RenderSections { get; }

        /// <summary>
        /// Gets the renderable that can be used to render this visual context, or null if the visual context
        /// requires no rendering.
        /// </summary>
        IRenderable Renderable { get; }
    }

    /// <summary>
    /// A 3d orthographical rectangular volume that specifies the location where
    /// items may be contained.
    /// </summary>
    public struct Bound
    {
        public Bound(Vector Min, Vector Max)
        {
            this.Min = Min;
            this.Max = Max;
        }

        public Bound(Vector[] Points)
        {
            this.Min = Points[0];
            this.Max = Points[0];
            for (int t = 1; t < Points.Length; t++)
            {
                this.AddVector(Points[t]);
            }
        }

        public Bound(Bound Source)
        {
            this.Min = Source.Min;
            this.Max = Source.Max;
        }

        /// <summary>
        /// Minimum value for the bounds. The x, y and z values in this should specify
        /// the corner of the bounds with the lowest values.
        /// </summary>
        public Vector Min;

        /// <summary>
        /// Maximum value for the bounds. The x, y and z values in this should specify
        /// the corner of the bounds with the highest values.
        /// </summary>
        public Vector Max;

        /// <summary>
        /// Adds a vector to be bounded by this bounds. This will not have an
        /// effect if the vector is already in the bounds.
        /// </summary>
        public void AddVector(Vector Vector)
        {
            this.Min.X = Vector.X < this.Min.X ? Vector.X : this.Min.X;
            this.Min.Y = Vector.Y < this.Min.Y ? Vector.Y : this.Min.Y;
            this.Min.Z = Vector.Z < this.Min.Z ? Vector.Z : this.Min.Z;
            this.Max.X = Vector.X > this.Max.X ? Vector.X : this.Max.X;
            this.Max.Y = Vector.Y > this.Max.Y ? Vector.Y : this.Max.Y;
            this.Max.Z = Vector.Z > this.Max.Z ? Vector.Z : this.Max.Z;
        }

        /// <summary>
        /// Transforms a bound by a matrix. The new bound will be in the coordinate space
        /// specified by the matrix and will contain everything within the old bound. This may
        /// cause the bound to grow larger then its initial area.
        /// </summary>
        public static Bound Transform(Matrix A, Bound B)
        {
            return new Bound(new Vector[] {
                A * new Vector(B.Min.X, B.Min.Y, B.Min.Z),
                A * new Vector(B.Min.X, B.Min.Y, B.Max.Z),
                A * new Vector(B.Min.X, B.Max.Y, B.Min.Z),
                A * new Vector(B.Min.X, B.Max.Y, B.Max.Z),
                A * new Vector(B.Max.X, B.Min.Y, B.Min.Z),
                A * new Vector(B.Max.X, B.Min.Y, B.Max.Z),
                A * new Vector(B.Max.X, B.Max.Y, B.Min.Z),
                A * new Vector(B.Max.X, B.Max.Y, B.Max.Z),
            });
        }

        /// <summary>
        /// Gets the union between two bounds, which encompasses both completely.
        /// </summary>
        public static Bound Union(Bound A, Bound B)
        {
            return new Bound(
                new Vector(
                    A.Min.X < B.Min.X ? A.Min.X : B.Min.X,
                    A.Min.Y < B.Min.Y ? A.Min.Y : B.Min.Y,
                    A.Min.Z < B.Min.Z ? A.Min.Z : B.Min.Z),
                new Vector(
                    A.Max.X > B.Max.X ? A.Max.X : B.Max.X,
                    A.Max.Y > B.Max.Y ? A.Max.Y : B.Max.Y,
                    A.Max.Z > B.Max.Z ? A.Max.Z : B.Max.Z));
        }

        /// <summary>
        /// Gets a bound that encompasses everything in all coordinate spaces.
        /// </summary>
        public static Bound Huge
        {
            get
            {
                double inf = double.PositiveInfinity;
                double ninf = double.NegativeInfinity;
                return new Bound(
                    new Vector(ninf, ninf, ninf),
                    new Vector(inf, inf, inf));
            }
        }

        /// <summary>
        /// Gets a bound that contains no volume in it.
        /// </summary>
        public static Bound None
        {
            get
            {
                double inf = double.PositiveInfinity;
                double ninf = double.NegativeInfinity;
                return new Bound(
                    new Vector(inf, inf, inf),
                    new Vector(ninf, ninf, ninf));
            }
        }
    }
}
