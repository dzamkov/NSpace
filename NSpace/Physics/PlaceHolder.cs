//----------------------------------------
// Copyright (c) 2010, Dmitry Zamkov 
// Open source under the new BSD License
//----------------------------------------
using System;
using System.Collections.Generic;

namespace NSpace.Physics
{
    /// <summary>
    /// A body that takes the place of another body until it is evaluated, at which time
    /// the placeholder is replaced with the actual body. Placeholders use worlds to control
    /// the time they are evaluated.
    /// </summary>
    public interface IPlaceHolder : IBody
    {
        /// <summary>
        /// Gets the real-world time at which the placeholder needs to be
        /// evaluated.
        /// </summary>
        Time Time { get; }

        /// <summary>
        /// Evaluates and returns the body for the place holder.
        /// </summary>
        IBody Evaluate(World World);
    }

    /// <summary>
    /// A place holder for another body that is yet to be fully evaluated.
    /// </summary>
    public abstract class PlaceHolder : IPlaceHolder
    {
        public PlaceHolder(Time Time, World World)
        {
            this._EventHandlers = new List<IBodyEventHandler>();
            this._Time = Time;
            World.AddPlaceHolder(this);
        }

        public TimeBound TimeBound
        {
            get
            {
                return new TimeBound(this._Time, this._Time);
            }
        }

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
        public IBody Evaluate(World World)
        {
            if (this._Body == null)
            {
                IBody val = this.OnEvaluate(World);
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
        /// Gets the body to use for the evaluated placeholder given the world the
        /// body is created in.
        /// </summary>
        protected abstract IBody OnEvaluate(World World);

        private IBody _Body;
        private Time _Time;
        private List<IBodyEventHandler> _EventHandlers;
    }
}