//----------------------------------------
// Copyright (c) 2010, Dmitry Zamkov 
// Open source under the new BSD License
//----------------------------------------
using System;
using System.Collections.Generic;

namespace NSpace.Physics
{
    /// <summary>
    /// Input into a gravitational system.
    /// </summary>
    public interface IGravityInput : IImmutable
    {
        /// <summary>
        /// Gets the section the input and results are in terms of.
        /// </summary>
        Section Section { get; }
    }

    /// <summary>
    /// Result from the interactions of a gravitational system.
    /// </summary>
    public interface IGravityResult : ISystemResult<IGravityInput, IGravityResult>
    {
        /// <summary>
        /// Gets the force vector gravity applies on the system.
        /// </summary>
        Vector Force { get; }
    }

    /// <summary>
    /// A system that uniformly applies a force to the entities given to it.
    /// </summary>
    public class GravitySystem : ISystem<IGravityInput, IGravityResult>
    {
        public GravitySystem(Vector Force, Section Section)
        {
            this._Force = Force;
            this._Section = Section;
        }

        /// <summary>
        /// Gets the force gravity applies relative to its section.
        /// </summary>
        public Vector Force
        {
            get
            {
                return this._Force;
            }
        }

        /// <summary>
        /// Gets the section that the force is relative to.
        /// </summary>
        public Section Section
        {
            get
            {
                return this._Section;
            }
        }

        /// <summary>
        /// Gets the force vector of the force applied to a specific section.
        /// </summary>
        public Vector ForceAtSection(Section Section)
        {
            return this._Section.GetRelation(Section).LinearTransform(this._Force);
        }

        IGravityResult ISystem<IGravityInput, IGravityResult>.Apply(IGravityInput Input)
        {
            return new Result(this, Input);
        }

        IGravityInput ISystem<IGravityInput, IGravityResult>.Combine(IEnumerable<IGravityInput> Inputs)
        {
            // Subresults can be calculated directly from input information. No need to make a different
            // input.
            return Inputs.GetEnumerator().Current;
        }

        /// <summary>
        /// Concrete result for a gravitational system.
        /// </summary>
        private class Result : IGravityResult
        {
            public Result(GravitySystem System, IGravityInput Input)
            {
                this._System = System;
                this._Input = Input;
            }

            Vector IGravityResult.Force
            {
                get 
                {
                    return this._System.ForceAtSection(this._Input.Section);
                }
            }

            IGravityInput ISystemResult<IGravityInput, IGravityResult>.Input
            {
                get 
                {
                    return this._Input;
                }
            }

            void ISystemResult<IGravityInput, IGravityResult>.GetSubResult(IGravityInput Input, out IGravityResult Result)
            {
                Result = new Result(this._System, Input);
            }

            private GravitySystem _System;
            private IGravityInput _Input;
        }

        private Vector _Force;
        private Section _Section;
    }
}
