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
    public abstract class PlaceHolder : IBody
    {
        public PlaceHolder(TimeBound TimeBound)
        {
            this._EventHandlers = new List<IBodyEventHandler>();
            this._TimeBound = TimeBound;
        }

        public TimeBound TimeBound
        {
            get
            {
                return this._TimeBound;
            }
        }

        public void Interact(IBody Other)
        {

        }

        public void Attach(IBodyEventHandler EventHandler)
        {
            this._EventHandlers.Add(EventHandler);
        }

        public void Detach(IBodyEventHandler EventHandler)
        {
            this._EventHandlers.Remove(EventHandler);
        }

        /// <summary>
        /// Evaluates, stores and returns the body for the place holder.
        /// </summary>
        public IBody Evaluate()
        {
            if (this._Body == null)
            {
                IBody val = this.EvaluatedBody;
                foreach (IBodyEventHandler beh in this._EventHandlers)
                {
                    beh.OnReassign(this, val);
                    val.Attach(beh);
                }
                this._EventHandlers.Clear();
                return val;
            }
            else
            {
                return this._Body;
            }
        }

        /// <summary>
        /// Gets the body to use for the evaluated placeholder.
        /// </summary>
        protected abstract IBody EvaluatedBody { get; }

        private IBody _Body;
        private TimeBound _TimeBound;
        private List<IBodyEventHandler> _EventHandlers;
    }
}