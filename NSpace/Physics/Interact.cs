//----------------------------------------
// Copyright (c) 2010, Dmitry Zamkov 
// Open source under the new BSD License
//----------------------------------------
using System;
using System.Collections.Generic;

namespace NSpace.Physics
{
    /// <summary>
    /// Procedure that allows bodies in a spacetime to interact with each other.
    /// </summary>
    public interface IInteractProcedure
    {
        /// <summary>
        /// Applies the interactions known by the procedure. This may apply
        /// only partial interactions(apply gravity to rigid bodies but not handle
        /// collisions between bodies), a full set of interactions, or nothing at
        /// all. If an interaction is not handled by a procedure, it is ignored.
        /// </summary>
        void ApplyInteractions(ISpaceTime SpaceTime);
    }

    /// <summary>
    /// An interact procedure made up of multiple other interact procedures
    /// applied in order.
    /// </summary>
    public class CompoundInteractProcedure : IInteractProcedure
    {
        public CompoundInteractProcedure()
        {

        }

        public CompoundInteractProcedure(IEnumerable<IInteractProcedure> Components)
        {
            this._Components = Components;
        }

        /// <summary>
        /// Gets or sets the list of procedures that make up the compound procedure.
        /// </summary>
        public IEnumerable<IInteractProcedure> Components
        {
            get
            {
                return this._Components;
            }
            set
            {
                this._Components = value;
            }
        }

        public void ApplyInteractions(ISpaceTime SpaceTime)
        {
            foreach (IInteractProcedure proc in this._Components)
            {
                proc.ApplyInteractions(SpaceTime);
            }
        }

        private IEnumerable<IInteractProcedure> _Components;
    }

    /// <summary>
    /// A spacetime where all bodies listed have interacted with each other
    /// with a procedure.
    /// </summary>
    public interface IDynamicSpaceTime : ISpaceTime
    {
        /// <summary>
        /// The procedure used for interaction within the spacetime.
        /// </summary>
        IInteractProcedure InteractProcedure { get; }
    }

    /// <summary>
    /// A simple unoptimized implementation of a dynamic space time that
    /// </summary>
    public class DynamicSpaceTime : IDynamicSpaceTime
    {
        public DynamicSpaceTime(IInteractProcedure Procedure, SpaceTime SpaceTime)
        {
            this._Procedure = Procedure;
            this._SpaceTime = SpaceTime;
        }

        public IInteractProcedure InteractProcedure
        {
            get 
            {
                return this._Procedure;
            }
            set
            {
                this._Procedure = value;
            }
        }

        /// <summary>
        /// Gets or sets the spacetime in which objects for this dynamic spacetime are stored.
        /// </summary>
        public SpaceTime SpaceTime
        {
            get
            {
                return this._SpaceTime;
            }
            set
            {
                this._SpaceTime = new SpaceTime();
            }
        }

        public IEnumerable<IBody> Bodies
        {
            get 
            {
                return this._SpaceTime.Bodies;
            }
        }

        public void Add(IBody Body)
        {
            this._SpaceTime.Add(Body);
            this._Procedure.ApplyInteractions(this._SpaceTime);
        }

        public void FindByType<T>(out IEnumerable<T> Results) where T : IBody
        {
            this._SpaceTime.FindByType<T>(out Results);
        }

        private IInteractProcedure _Procedure;
        private SpaceTime _SpaceTime;
    }
}