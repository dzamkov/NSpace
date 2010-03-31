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
    /// applied in order. The compound procedure will continue looping through
    /// all its procedures until no more changes are made by the sub-procedures.
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
            throw new NotImplementedException();
        }

        private IEnumerable<IInteractProcedure> _Components;
    }
}