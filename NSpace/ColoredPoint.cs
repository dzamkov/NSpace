//----------------------------------------
// Copyright (c) 2010, Dmitry Zamkov 
// Open source under the new BSD License
//----------------------------------------

namespace NSpace
{
    /// <summary>
    /// A point that is displayed with a color.
    /// </summary>
    public class ColoredPoint : Point
    {

        public override void Mix(Point Other, double Amount)
        {
            ColoredPoint cp = Other as ColoredPoint;
            if (cp != null)
            {
                this.Color = Color.Mix(this.Color, cp.Color, Amount);
            }
            base.Mix(Other, Amount);
        }

        public override Point Copy()
        {
            ColoredPoint cp = new ColoredPoint();
            this.Clone(cp);
            return cp;
        }

        protected override void Clone(Point Point)
        {
            (Point as ColoredPoint).Color = this.Color;
            base.Clone(Point);
        }

        public Color Color;
    }
}
