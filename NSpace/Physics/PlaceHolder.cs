//----------------------------------------
// Copyright (c) 2010, Dmitry Zamkov 
// Open source under the new BSD License
//----------------------------------------
using System;
using System.Collections.Generic;

namespace NSpace.Physics
{
    /// <summary>
    /// A place holder for another body that is yet to be fully evaluated.
    /// </summary>
    public abstract class PlaceHolder : ICompoundBody
    {
        public PlaceHolder(TimeBound TimeBound)
        {
            this._TimeBound = TimeBound;
        }

        public TimeBound TimeBound
        {
            get
            {
                return this._TimeBound;
            }
        }

        public IEnumerable<IBody> Bodies
        {
            get 
            { 
                throw new NotImplementedException();
            }
        }

        public void Interact(IBody Other)
        {
            throw new NotImplementedException();
        }

        public void Attach(IBodyEventHandler EventHandler)
        {
            throw new NotImplementedException();
        }

        public void Detach(IBodyEventHandler EventHandler)
        {
            throw new NotImplementedException();
        }

        /// <summary>
        /// Evaluates and returns the body that is in this placeholder.
        /// </summary>
        protected abstract IBody Evaluate();

        private IBody _Body;
        private TimeBound _TimeBound; 
    }
}