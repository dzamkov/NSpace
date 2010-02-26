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
    /// Transformation matrix used to specify a linear transform from one coordinate system
    /// to another.
    /// </summary>
    public struct Matrix
    {
        public Matrix(
            double M11, double M12, double M13, double M14,
            double M21, double M22, double M23, double M24,
            double M31, double M32, double M33, double M34,
            double M41, double M42, double M43, double M44)
        {
            this.M11 = M11; this.M12 = M12; this.M13 = M13; this.M14 = M14;
            this.M21 = M21; this.M22 = M22; this.M23 = M23; this.M24 = M24;
            this.M31 = M31; this.M32 = M32; this.M33 = M33; this.M34 = M34;
            this.M41 = M41; this.M42 = M42; this.M43 = M43; this.M44 = M44;
        }

        public double M11; public double M12; public double M13; public double M14; // Arrays require an extra dereference.
        public double M21; public double M22; public double M23; public double M24; // not worth it.
        public double M31; public double M32; public double M33; public double M34;
        public double M41; public double M42; public double M43; public double M44;

        /// <summary>
        /// Gets or sets the value in the matrix specified by the 0-base row major index.
        /// </summary>
        public double this[int Index]
        {
            get
            {
                switch (Index)
                {
                    case 0: return this.M11;
                    case 1: return this.M12;
                    case 2: return this.M13;
                    case 3: return this.M14;
                    case 4: return this.M21;
                    case 5: return this.M22;
                    case 6: return this.M23;
                    case 7: return this.M24;
                    case 8: return this.M31;
                    case 9: return this.M32;
                    case 10: return this.M33;
                    case 11: return this.M34;
                    case 12: return this.M41;
                    case 13: return this.M42;
                    case 14: return this.M43;
                    case 15: return this.M44;
                }
                return 0.0;
            }
            set
            {
                switch (Index)
                {
                    case 0: this.M11 = value; break;
                    case 1: this.M12 = value; break;
                    case 2: this.M13 = value; break;
                    case 3: this.M14 = value; break;
                    case 4: this.M21 = value; break;
                    case 5: this.M22 = value; break;
                    case 6: this.M23 = value; break;
                    case 7: this.M24 = value; break;
                    case 8: this.M31 = value; break;
                    case 9: this.M32 = value; break;
                    case 10: this.M33 = value; break;
                    case 11: this.M34 = value; break;
                    case 12: this.M41 = value; break;
                    case 13: this.M42 = value; break;
                    case 14: this.M43 = value; break;
                    case 15: this.M44 = value; break;
                }
            }
        }

        /// <summary>
        /// Gets the inverse of this matrix. Any transform applied to this matrix can
        /// be canceled out with the inverse.
        /// </summary>
        public Matrix Inverse()
        {
            Matrix m = new Matrix();
            m.M11 = (this.M22 * this.M33 * this.M44) + (this.M23 * this.M34 * this.M42) + (this.M24 * this.M32 * this.M43) -
                (this.M22 * this.M34 * this.M43) - (this.M23 * this.M32 * this.M44) - (this.M24 * this.M33 * this.M42);
            m.M12 = (this.M12 * this.M34 * this.M43) + (this.M13 * this.M32 * this.M44) + (this.M14 * this.M33 * this.M42) -
                (this.M12 * this.M33 * this.M44) - (this.M13 * this.M34 * this.M42) - (this.M14 * this.M32 * this.M43);
            m.M13 = (this.M12 * this.M23 * this.M44) + (this.M13 * this.M24 * this.M42) + (this.M14 * this.M22 * this.M43) -
                (this.M12 * this.M24 * this.M43) - (this.M13 * this.M22 * this.M44) - (this.M14 * this.M23 * this.M42);
            m.M14 = (this.M12 * this.M24 * this.M33) + (this.M13 * this.M22 * this.M34) + (this.M14 * this.M23 * this.M32) -
                (this.M12 * this.M23 * this.M34) - (this.M13 * this.M24 * this.M32) - (this.M14 * this.M22 * this.M33);

            m.M21 = (this.M21 * this.M34 * this.M43) + (this.M23 * this.M31 * this.M44) + (this.M24 * this.M33 * this.M41) -
               (this.M21 * this.M33 * this.M44) - (this.M23 * this.M34 * this.M41) - (this.M24 * this.M31 * this.M43);
            m.M22 = (this.M11 * this.M33 * this.M44) + (this.M13 * this.M34 * this.M41) + (this.M14 * this.M31 * this.M43) -
                (this.M11 * this.M34 * this.M43) - (this.M13 * this.M31 * this.M44) - (this.M14 * this.M33 * this.M41);
            m.M23 = (this.M11 * this.M24 * this.M43) + (this.M13 * this.M21 * this.M44) + (this.M14 * this.M23 * this.M41) -
                (this.M11 * this.M23 * this.M44) - (this.M13 * this.M24 * this.M41) - (this.M14 * this.M21 * this.M43);
            m.M24 = (this.M11 * this.M23 * this.M34) + (this.M13 * this.M24 * this.M31) + (this.M14 * this.M21 * this.M33) -
                (this.M11 * this.M24 * this.M33) - (this.M13 * this.M21 * this.M34) - (this.M14 * this.M23 * this.M31);

            m.M31 = (this.M21 * this.M32 * this.M44) + (this.M22 * this.M34 * this.M41) + (this.M24 * this.M31 * this.M42) -
               (this.M21 * this.M34 * this.M42) - (this.M22 * this.M31 * this.M44) - (this.M24 * this.M32 * this.M41);
            m.M32 = (this.M11 * this.M34 * this.M42) + (this.M12 * this.M31 * this.M44) + (this.M14 * this.M32 * this.M41) -
                (this.M11 * this.M32 * this.M44) - (this.M12 * this.M34 * this.M41) - (this.M14 * this.M31 * this.M42);
            m.M33 = (this.M11 * this.M22 * this.M44) + (this.M12 * this.M24 * this.M41) + (this.M14 * this.M21 * this.M42) -
                (this.M11 * this.M24 * this.M42) - (this.M12 * this.M21 * this.M44) - (this.M14 * this.M22 * this.M41);
            m.M34 = (this.M11 * this.M24 * this.M32) + (this.M12 * this.M21 * this.M34) + (this.M14 * this.M22 * this.M31) -
                (this.M11 * this.M22 * this.M34) - (this.M12 * this.M24 * this.M31) - (this.M14 * this.M21 * this.M32);

            m.M41 = (this.M21 * this.M33 * this.M42) + (this.M22 * this.M31 * this.M43) + (this.M23 * this.M32 * this.M41) -
               (this.M21 * this.M32 * this.M43) - (this.M22 * this.M33 * this.M41) - (this.M23 * this.M31 * this.M42);
            m.M42 = (this.M11 * this.M32 * this.M43) + (this.M12 * this.M33 * this.M41) + (this.M13 * this.M31 * this.M42) -
                (this.M11 * this.M33 * this.M42) - (this.M12 * this.M31 * this.M43) - (this.M13 * this.M32 * this.M41);
            m.M43 = (this.M11 * this.M23 * this.M42) + (this.M12 * this.M21 * this.M43) + (this.M13 * this.M22 * this.M41) -
                (this.M11 * this.M22 * this.M43) - (this.M12 * this.M23 * this.M41) - (this.M13 * this.M21 * this.M42);
            m.M44 = (this.M11 * this.M22 * this.M33) + (this.M12 * this.M23 * this.M31) + (this.M13 * this.M21 * this.M32) -
                (this.M11 * this.M23 * this.M32) - (this.M12 * this.M21 * this.M33) - (this.M13 * this.M22 * this.M31);

            double det = (this.M11 * m.M11) + (this.M12 * m.M21) + (this.M13 * m.M31) + (this.M14 * m.M41);

            m.M11 /= det; m.M12 /= det; m.M13 /= det; m.M14 /= det;
            m.M21 /= det; m.M22 /= det; m.M23 /= det; m.M24 /= det;
            m.M31 /= det; m.M32 /= det; m.M33 /= det; m.M34 /= det;
            m.M41 /= det; m.M42 /= det; m.M43 /= det; m.M44 /= det;

            return m;
        }

        /// <summary>
        /// Gets the identity matrix.
        /// </summary>
        public static Matrix Identity
        {
            get
            {
                return new Matrix(
                    1.0, 0.0, 0.0, 0.0,
                    0.0, 1.0, 0.0, 0.0,
                    0.0, 0.0, 1.0, 0.0,
                    0.0, 0.0, 0.0, 1.0);
            }
        }

        /// <summary>
        /// Creates a frustum projection matrix, which can act as a perspective matrix.
        /// </summary>
        public static Matrix Frustum(double Left, double Right, double Top, double Bottom, double Near, double Far)
        {
            Matrix m = new Matrix();
            double dne = Near * 2.0;
            double rml = Right - Left;
            double tmb = Top - Bottom;
            double fmn = Far - Near;
            m.M11 = dne / rml;
            m.M13 = (Right + Left) / rml;
            m.M22 = -dne / tmb;
            m.M23 = (Top + Bottom) / tmb;
            m.M33 = -(Far + Near) / fmn;
            m.M34 = -(2.0 * Far * Near) / fmn;
            m.M43 = -1.0;
            return m;
        }

        /// <summary>
        /// Creates a perspective projection matrix with the specified field of view and aspect ratio.
        /// </summary>
        public static Matrix Perspective(double FOV, double Aspect, double Near, double Far)
        {
            double top = Near * Math.Tan(FOV * 0.5);
            double right = top * Aspect;
            double bottom = -top;
            double left = -right;
            return Frustum(left, right, -top, -bottom, Near, Far);
        }

        /// <summary>
        /// Multiplies a vector by a matrix, effectively transforming it.
        /// </summary>
        public static Vector operator *(Matrix A, Vector B)
        {
            return new Vector(
                    (A.M11 * B.X) + (A.M12 * B.Y) + (A.M13 * B.Z) + A.M14,
                    (A.M21 * B.X) + (A.M22 * B.Y) + (A.M23 * B.Z) + A.M24,
                    (A.M31 * B.X) + (A.M32 * B.Y) + (A.M33 * B.Z) + A.M34);
        }

        /// <summary>
        /// Converts a matrix into a matrix usable by OpenTK.
        /// </summary>
        public static implicit operator Matrix4d(Matrix Matrix)
        {
            return new Matrix4d(
                Matrix.M11, Matrix.M21, Matrix.M31, Matrix.M41,
                Matrix.M12, Matrix.M22, Matrix.M32, Matrix.M42,
                Matrix.M13, Matrix.M23, Matrix.M33, Matrix.M43,
                Matrix.M14, Matrix.M24, Matrix.M34, Matrix.M44);
        }
    }
}
