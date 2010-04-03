//----------------------------------------
// Copyright (c) 2010, Dmitry Zamkov 
// Open source under the new BSD License
//----------------------------------------
using System;
using System.Collections.Generic;

namespace NSpace
{
    /// <summary>
    /// A curved path that has interpolated values over time.
    /// </summary>
    public interface ICurve
    {
        /// <summary>
        /// Gets the point at the specified time in the curve.
        /// </summary>
        Vector GetPoint(double Time);

        /// <summary>
        /// Gets a curve that represents the derivative or hodograph of this curve. Do not
        /// touch this unless you know what you are doing.
        /// </summary>
        ICurve Derivative();

        /// <summary>
        /// Gets the integral of this curve with the first point in the curve starting at
        /// C. This is the inverse operation of derivative.
        /// </summary>
        ICurve Integral(Vector C);

        /// <summary>
        /// Multiplies all points on the curve with a scalar value and returns the resulting curve.
        /// </summary>
        ICurve Multiply(double Scalar);

        /// <summary>
        /// Returns the negative of this curve.
        /// </summary>
        ICurve Negate();
    }

    /// <summary>
    /// Represents a bezier curve with a set amount of control points. Once again, if you
    /// dont understand this, don't use it or google it or something.
    /// </summary>
    public struct BezierCurve : ICurve
    {
        public BezierCurve(Vector[] ControlPoints)
        {
            if(ControlPoints.Length >= 2)
            {
                this._ControlPoints = new Vector[ControlPoints.Length];
                for (int t = 0; t < ControlPoints.Length; t++)
                {
                    this._ControlPoints[t] = ControlPoints[t];
                }
            }
            else
            {
                throw new Exception(); // This isnt a curve.
            }
        }

        /// <summary>
        /// Gets the point on a bezier curve at the specified time between 0.0 and 1.0.
        /// </summary>
        private static Vector _Bezier(Vector[] Points, double T)
        {
            Vector[] npoints = new Vector[Points.Length - 1];
            for (int t = 0; t < Points.Length - 1; t++)
            {
                Vector a = Points[t];
                Vector b = Points[t + 1];
                Vector r = (a * (1.0 - T)) + (b * T);
                npoints[t] = r;
            }
            if (npoints.Length == 1)
            {
                return npoints[0];
            }
            return _Bezier(npoints, T);
        }

        public Vector GetPoint(double Time)
        {
            return _Bezier(this._ControlPoints, Time);
        }

        public ICurve Derivative()
        {
            Vector[] npoints = new Vector[this._ControlPoints.Length - 1];
            for (int t = 0; t < this._ControlPoints.Length - 1; t++)
            {
                npoints[t] = (this._ControlPoints[t + 1] - this._ControlPoints[t]) * (double)(this._ControlPoints.Length - 1);
            }
            return new BezierCurve(npoints);
        }

        public ICurve Integral(Vector C)
        {
            Vector[] npoints = new Vector[this._ControlPoints.Length + 1];
            npoints[0] = C;
            for (int t = 0; t < this._ControlPoints.Length; t++)
            {
                npoints[t + 1] = npoints[t] + this._ControlPoints[t] / (double)(this._ControlPoints.Length);
            }
            return new BezierCurve(npoints);
        }

        public ICurve Multiply(double Scalar)
        {
            BezierCurve bc = new BezierCurve();
            bc._ControlPoints = new Vector[this._ControlPoints.Length];
            for (int t = 0; t < this._ControlPoints.Length; t++)
            {
                bc._ControlPoints[t] = this._ControlPoints[t] * Scalar;
            }
            return bc;
        }

        public ICurve Negate()
        {
            return this.Multiply(-1);
        }

        private Vector[] _ControlPoints;
    }

    /// <summary>
    /// A curve that always returns the same points no matter what time is given.
    /// </summary>
    public struct ConstantCurve : ICurve
    {
        public ConstantCurve(Vector Value)
        {
            this._Value = Value;
        }

        public Vector GetPoint(double Time)
        {
            return this._Value;
        }

        public ICurve Derivative()
        {
            return new ConstantCurve(new Vector(0.0, 0.0, 0.0));
        }

        public ICurve Integral(Vector C)
        {
            return new BezierCurve(new Vector[] { C, C + this._Value });
        }

        public ICurve Multiply(double Scalar)
        {
            return new ConstantCurve(this._Value * Scalar);
        }

        public ICurve Negate()
        {
            return new ConstantCurve(this._Value * -1);
        }

        private Vector _Value;
    }
    
    /// <summary>
    /// A curve that acts as the sum of two other curves.
    /// </summary>
    public struct SumCurve : ICurve
    {
        public SumCurve(ICurve A, ICurve B)
        {
            this._A = A;
            this._B = B;
        }

        public Vector GetPoint(double Time)
        {
            return this._A.GetPoint(Time) + this._B.GetPoint(Time);
        }

        public ICurve Derivative()
        {
            return new SumCurve(this._A.Derivative(), this._B.Derivative());
        }

        public ICurve Integral(Vector C)
        {
            return new SumCurve(this._A.Integral(C), this._B.Integral(new Vector()));
        }

        public ICurve Multiply(double Scalar)
        {
            return new SumCurve(this._A.Multiply(Scalar), this._B.Multiply(Scalar));
        }

        public ICurve Negate()
        {
            return new SumCurve(this._A.Negate(), this._B.Negate());
        }

        private ICurve _A;
        private ICurve _B;
    }
}
