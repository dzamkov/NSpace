//----------------------------------------
// Copyright (c) 2010, Dmitry Zamkov 
// Open source under the new BSD License
//----------------------------------------
using System;
using System.Collections.Generic;
using OpenTK;

namespace NSpace
{
    /// <summary>
    /// Mathematical quaternion that can represent rotations.
    /// </summary>
    public struct Quaternion
    {
        public Quaternion(double A, double B, double C, double D)
        {
            this.A = A;
            this.B = B;
            this.C = C;
            this.D = D;
        }

        public double A;
        public double B;
        public double C;
        public double D;

        /// <summary>
        /// Normalizes the quaternion, making (A^2)+(B^2)+(C^2)+(D^2) = 1. I have no clue
        /// what this is supposed to do, so im gonna assume you do by using this.
        /// </summary>
        public void Normalize()
        {
            double length = (this.A * this.A) + (this.B * this.B) + (this.C * this.C) + (this.D * this.D);
            this.A /= length;
            this.B /= length;
            this.C /= length;
            this.D /= length;
        }

        /// <summary>
        /// Converts the rotation represented by this quaternion to
        /// a transformation matrix.
        /// </summary>
        public Matrix ToMatrix()
        {
            double asqr = this.A * this.A;
            double bsqr = this.B * this.B;
            double csqr = this.C * this.C;
            double dsqr = this.D * this.D;
            double ab = 2 * this.A * this.B;
            double ac = 2 * this.A * this.C;
            double ad = 2 * this.A * this.D;
            double bc = 2 * this.B * this.C;
            double bd = 2 * this.B * this.D;
            double cd = 2 * this.C * this.D;
            return new Matrix(
                asqr + bsqr - csqr - dsqr, bc - ad, bd + ac, 0.0,
                bc + ad, asqr - bsqr + csqr - dsqr, cd - ab, 0.0,
                bd - ac, cd + ab, asqr - bsqr - csqr + dsqr, 0.0,
                0.0, 0.0, 0.0, 1.0);
        }

        /// <summary>
        /// Creates a rotation quaternion around the specified axis by
        /// the specified angle in radians. Please normalize either the
        /// axis vector or quaternion with this operation unless you are
        /// entirely sure what you're doing.
        /// </summary>
        public static Quaternion AxisRotate(Vector Axis, double Angle)
        {
            double sinang = Math.Sin(Angle / 2.0);
            double cosang = Math.Cos(Angle / 2.0);
            return new Quaternion(
                cosang, 
                Axis.X * sinang, 
                Axis.Y * sinang, 
                Axis.Z * sinang);
        }
    }
}
