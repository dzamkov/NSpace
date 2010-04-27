﻿//----------------------------------------
// Copyright (c) 2010, Dmitry Zamkov 
// Open source under the new BSD License
//----------------------------------------
using System;
using System.Collections.Generic;

namespace NSpace
{
    /// <summary>
    /// A time and coordinate space that can be related to othe sections. Measures of time
    /// and location are in terms of sections, that is, all measures within a section are directly
    /// related to each other. If two coordinates have the same value, and are in the same section,
    /// they are in the same place. Sections may have a parent-child relation with another section, from
    /// which the relations to the siblings and ancestor relations can be caluclated.
    /// </summary>
    public class Section : IImmutable
    {
        public Section()
        {

        }

        /// <summary>
        /// Gets the relation this section has with its parent.
        /// </summary>
        public Relation ParentRelation
        {
            get
            {
                return this._ParentRelation;
            }
        }

        /// <summary>
        /// Gets the relation this section has with another section.
        /// </summary>
        public Relation GetRelation(Section Other)
        {
            throw new NotImplementedException();
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
        /// Gets the parent section for this section.
        /// </summary>
        public Section Parent
        {
            get
            {
                return this._Parent;
            }
        }

        /// <summary>
        /// Represents a relation from one section to another. These can
        /// be used to convert measurements between sections.
        /// </summary>
        public struct Relation
        {
            /// <summary>
            /// Gets a transform that, when applied to local space, will transform to the coordinate
            /// space of the other section.
            /// </summary>
            public Matrix SpaceTransform;

            /// <summary>
            /// The local time offset from this section to the other.
            /// </summary>
            public Time TimeOffset;

            /// <summary>
            /// The local time length scale from this section to the other.
            /// </summary>
            public TimeSpan TimeScale;

            /// <summary>
            /// Transforms a vector from local space to the coordinate space of the other section.
            /// </summary>
            public Vector TransformVector(Vector V)
            {
                return this.SpaceTransform * V;
            }

            /// <summary>
            /// Transforms a time in this section to a time in the other.
            /// </summary>
            public Time TransformTime(Time T)
            {
                return new Time((T - this.TimeOffset).Amount / this.TimeScale.Amount);
            }

            /// <summary>
            /// Transforms a timespan in this section to a timespan in the other.
            /// </summary>
            public TimeSpan TransformTimeSpan(TimeSpan T)
            {
                return new TimeSpan(T.Amount * this.TimeScale.Amount);
            }

            /// <summary>
            /// Gets the inverse of this relation, which effectively switchs the section
            /// this relation is from and the section this relation is to.
            /// </summary>
            public Relation Inverse
            {
                get
                {
                    return new Relation
                    {
                        SpaceTransform = this.SpaceTransform.Inverse(),
                        TimeScale = new TimeSpan(1.0 / this.TimeScale.Amount),
                        TimeOffset = new Time(-this.TimeOffset.Amount / this.TimeScale.Amount)
                    };
                }
            }

            /// <summary>
            /// Combines this relation with another.
            /// </summary>
            /// <remarks>If the section this relation is from is X, the section it is to is Y, the
            /// section Other is from is Y, and the section Other is to is Z, then the result of this
            /// method is a relation from X to Z.</remarks>
            public Relation Combine(Relation Other)
            {
                return new Relation
                {
                    SpaceTransform = Matrix.Transform(this.SpaceTransform, Other.SpaceTransform),
                    TimeScale = new TimeSpan(this.TimeScale.Amount * Other.TimeScale.Amount),
                    TimeOffset = new Time(this.TimeOffset.Amount + (Other.TimeOffset.Amount * this.TimeScale.Amount))
                };
            }
        }

        private Section _Parent;
        private Relation _ParentRelation;
    }
}
