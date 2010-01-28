//----------------------------------------
// Copyright (c) 2010, Dmitry Zamkov 
// Open source under the new BSD License
//----------------------------------------
using System.Collections.Generic;

namespace NSpace
{
    /// <summary>
    /// A triangle that can be subdivide and have its relationships with
    /// other triangles stored. The triangle is subdivided into four parts
    /// with equal area.
    /// </summary>
    /// <typeparam name="P">The type of points to act as corners of the triangle.</typeparam>
    public class DivTriangle : Triangle
    {
        public DivTriangle()
        {
            this.Borders = new Triangle[3];
        }

        /// <summary>
        /// Splits the triangle and outputs its children to the sink target. If the target is
        /// null, the new children are stored but not added to a sink. If children are already
        /// stored for this triangle, it simply outputs them. Optionally, new points created by
        /// this split can be outputted to the specified point sink.
        /// </summary>
        public void Split(ISink<Triangle> Target, ISink<Point> NewPoints)
        {
            // Create children if needed
            if (this.Children == null)
            {
                // Create an array of the four children.
                this.Children = new DivTriangle[4];

                // Initialize the children and set the parent.
                for (int t = 0; t < 4; t++)
                {
                    this.Children[t] = new DivTriangle();
                    this.Children[t].Parent = this;
                }

                // Create inner three points and setup outside borders.
                Point[] inner = new Point[3];
                for (int t = 0; t < 3; t++)
                {
                    DivTriangle border = this.Borders[t] as DivTriangle;
                    if (border != null && border.Children != null)
                    {
                        // Get point already created by subdivided border
                        for (int l = 0; l < 3; l++)
                        {
                            if (border.Borders[l] == this)
                            {
                                inner[t] = border.Children[3].Points[l];
                                this.Children[t].Borders[0] = border.Children[(l + 1) % 3];
                                this.Children[(t + 1) % 3].Borders[2] = border.Children[l];
                                border.Children[(l + 1) % 3].Borders[2] = this.Children[t];
                                border.Children[l].Borders[0] = this.Children[(t + 1) % 3];
                                break;
                            }
                        }
                    }
                    else
                    {
                        // Create new midpoint
                        inner[t] = this.Points[t].Copy();
                        inner[t].Mix(this.Points[(t + 1) % 3], 0.5);
                        if (NewPoints != null)
                        {
                            NewPoints.Add(inner[t]);
                        }
                    }
                }

                // Set up outside triangles
                for (int t = 0; t < 3; t++)
                {
                    this.Children[t].Points[0] = this.Points[t];
                    this.Children[t].Points[1] = inner[t];
                    this.Children[t].Points[2] = inner[(t + 2) % 3];
                    this.Children[t].Borders[1] = this.Children[3];
                }

                // Set up inside triangle
                for(int t = 0; t < 3; t++)
                {
                    this.Children[3].Points[t] = inner[t];
                    this.Children[3].Borders[t] = this.Children[(t + 1) % 3];
                }
            }

            // Load children into mesh
            if (Target != null)
            {
                foreach (DivTriangle tri in this.Children)
                {
                    Target.Add(tri);
                }
            }
        }

        public DivTriangle[] Children;
        public DivTriangle Parent;
        public Triangle[] Borders;
    }
}
