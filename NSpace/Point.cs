//----------------------------------------
// Copyright (c) 2010, Dmitry Zamkov 
// Open source under the new BSD License
//----------------------------------------
using System;
using System.Collections.Generic;

namespace NSpace
{
    /// <summary>
    /// Methods and classes for the manipulation of points.
    /// </summary>
    public static class Point
    {
        /// <summary>
        /// A type of data within a point that can be mixed with other data of the same
        /// type.
        /// </summary>
        public interface IMixable : IData
        {
            /// <summary>
            /// Mixes this data with the other data by the specified
            /// amount. If Amount is 0.0, no change is made. If Amount
            /// is 1.0, this data will become equivalent to Other.
            /// </summary>
            void Mix(IData Other, double Amount);
        }

        /// <summary>
        /// A single point in a mesh that represents a location relative to other
        /// points in the same mesh. A single point can be used multiple times within
        /// a mesh by multiple triangles.
        /// </summary>
        public class Data : IData, IMixable
        {
            public Vector Position;

            public IData Copy()
            {
                return new Data() { Position = this.Position };
            }

            public void Mix(IData Other, double Amount)
            {
                Data l = (Data)Other;
                this.Position += (l.Position - this.Position) * Amount;
            }

            /// <summary>
            /// Gets the point data from the geometry.
            /// </summary>
            public static explicit operator Data(Geometry Geom)
            {
                return Geom.GetData<Data>();
            }
        }

        /// <summary>
        /// Color data for a point.
        /// </summary>
        public class ColorData : IData, IMixable
        {
            public Color Color;

            public IData Copy()
            {
                return new ColorData() { Color = this.Color };
            }

            public void Mix(IData Other, double Amount)
            {
                ColorData cd = (ColorData)Other;
                this.Color = Color.Mix(this.Color, cd.Color, Amount);
            }
        }

        /// <summary>
        /// UV, or texture mapping data for a point.
        /// </summary>
        public class UVData : IData, IMixable
        {
            public double U;
            public double V;

            public IData Copy()
            {
                return new UVData() { U = this.U, V = this.V };
            }

            public void Mix(IData Other, double Amount)
            {
                UVData ud = (UVData)Other;
                this.U += (ud.U - this.U) * Amount;
                this.V += (ud.V - this.V) * Amount;
            }
        }

        /// <summary>
        /// Mixes a point with another by the specified amount. If the amount is
        /// 0.0, the point will remain unchanged; if the amount is 1.0, the point
        /// will take the state of the other point. Values between the two are
        /// interpolated.
        /// </summary>
        public static void Mix(Geometry A, Geometry B, double Amount)
        {
            List<KeyValuePair<Type, IData>> changed = new List<KeyValuePair<Type, IData>>();
            foreach (KeyValuePair<Type, IData> kvp in A.FullData)
            {
                IData o = B.GetData(kvp.Key);
                IMixable l = kvp.Value as IMixable;
                l.Mix(o, Amount);
                changed.Add(new KeyValuePair<Type, IData>(kvp.Key, l));
            }
            foreach (KeyValuePair<Type, IData> kvp in changed)
            {
                A.SetData(kvp.Key, kvp.Value);
            }
        }

        /// <summary>
        /// Creates a new point with only position data.
        /// </summary>
        public static Geometry Create(Vector Position)
        {
            ElasticGeometry geom = new ElasticGeometry(null);
            geom.SetData<Data>(new Data() { Position = Position });
            return geom;
        }

        /// <summary>
        /// Creates a new point with UV data.
        /// </summary>
        public static Geometry Create(Vector Position, double U, double V)
        {
            Geometry geom = Create(Position);
            geom.SetData<UVData>(new UVData() { U = U, V = V });
            return geom;
        }

        /// <summary>
        /// Creates a new point with color data.
        /// </summary>
        public static Geometry Create(Vector Position, Color Color)
        {
            Geometry geom = Create(Position);
            geom.SetData<ColorData>(new ColorData() { Color = Color });
            return geom;
        }

        /// <summary>
        /// Gets the position of the specified point.
        /// </summary>
        public static Vector Position(Geometry Point)
        {
            return Point.GetData<Data>().Position;
        }
    }
}
