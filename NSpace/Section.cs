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
    public abstract class Section
    {
        public Section()
        {
            this._ParentRelation = Matrix.Identity;
            this._InverseParentRelation = Matrix.Identity;
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
        /// Gets or sets the relation this section has to its parent. This transform goes
        /// from the parent to the child.
        /// </summary>
        public Matrix ParentRelation
        {
            get
            {
                return this._ParentRelation;
            }
            set
            {
                this._InverseParentRelation = value.Inverse();
                this._ParentRelation = value;
                this._UpdateRelation();
            }
        }

        /// <summary>
        /// Gets or sets the relation this section's parent has to itself. This transform goes
        /// from the child to the parent.
        /// </summary>
        public Matrix InverseParentRelation
        {
            get
            {
                return this._InverseParentRelation;
            }
            set
            {
                this._InverseParentRelation = value;
                this._ParentRelation = value.Inverse();
                this._UpdateRelation();
            }
        }

        /// <summary>
        /// Gets the bounds of the section, which contains all things and volume it can
        /// affect.
        /// </summary>
        public virtual Bound Bound
        {
            get
            {
                return Bound.None;
            }
        }

        /// <summary>
        /// Commits changes to the bound of this section.
        /// </summary>
        public void UpdateBound()
        {
            this._UpdateBound();
        }

        /// <summary>
        /// Updates the bounds of this section with new values from ContentBound and ChildrenBound. This
        /// is called automatically with any change to either.
        /// </summary>
        private void _UpdateBound()
        {
            if (this._Parent != null)
            {
                this._Parent._UpdateBound(this);
            }
        }

        /// <summary>
        /// Updates the relation this section has to its parent.
        /// </summary>
        private void _UpdateRelation()
        {
            if (this._Parent != null)
            {
                this._Parent._UpdateRelation(this);
            }
        }

        internal ComplexSection _Parent;
        private Matrix _ParentRelation;
        private Matrix _InverseParentRelation;
    }

    /// <summary>
    /// A section that can have child sections.
    /// </summary>
    public abstract class ComplexSection : Section
    {
        public ComplexSection()
        {
            this._Children = new Dictionary<Section, Bound>();
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

        public override Bound Bound
        {
            get
            {
                return this._Bound;
            }
        }

        /// <summary>
        /// Adds a section as a child to this section. The child must
        /// not yet have a parent section.
        /// </summary>
        protected void AddChild(Section Child)
        {
            if (Child.Parent == null)
            {
                Child._Parent = this;
                this._Children.Add(Child, this._GetBound(Child));
            }
        }

        /// <summary>
        /// Updates the bounds of a child section.
        /// </summary>
        internal void _UpdateBound(Section Child)
        {
            this._Children[Child] = this._GetBound(Child);
        }
        
        /// <summary>
        /// Updates the relation of a child section.
        /// </summary>
        internal void _UpdateRelation(Section Child)
        {
            this._Children[Child] = this._GetBound(Child);
        }

        /// <summary>
        /// Gets the bounds for a child section.
        /// </summary>
        private Bound _GetBound(Section Child)
        {
            return Bound.Transform(Child.InverseParentRelation, Child.Bound);
        }

        /// <summary>
        /// Creates the bound for this complex section based on child sections.
        /// </summary>
        private void _CreateBound()
        {
            this._Bound = Bound.None;
            foreach (Bound b in this._Children.Values)
            {
                this._Bound = Bound.Union(this._Bound, b);
            }
        }

        private Bound _Bound;
        private Dictionary<Section, Bound> _Children;
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
