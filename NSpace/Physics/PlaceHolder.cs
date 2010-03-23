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
        public PlaceHolder(Time Time)
        {
            this._EventHandlers = new List<IBodyEventHandler>();
            this._Time = Time;
        }

        public TimeBound TimeBound
        {
            get
            {
                return new TimeBound(this._Time, this._Time);
            }
        }

        /// <summary>
        /// Gets the real-world time at which the placeholder needs to be
        /// evaluated.
        /// </summary>
        public Time Time
        {
            get
            {
                return this._Time;
            }
        }

        public void Interact(IBody Other)
        {

        }

        public void Attach(IBodyEventHandler EventHandler)
        {
            if (this._Body == null)
            {
                this._EventHandlers.Add(EventHandler);
            }
            else
            {
                EventHandler.OnReassign(this, this._Body);
                this._Body.Attach(EventHandler);
            }
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
                    beh.OnModified(val);
                }
                this._EventHandlers = null; // Insure no more event handlers are added.
                this._Body = val;
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
        private Time _Time;
        private List<IBodyEventHandler> _EventHandlers;
    }
}