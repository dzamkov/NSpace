//----------------------------------------
// Copyright (c) 2010, Dmitry Zamkov 
// Open source under the new BSD License
//----------------------------------------
using System;
using System.Drawing;
using System.Drawing.Imaging;
using System.Collections.Generic;
using OpenTK;
using OpenTK.Graphics.OpenGL;

namespace NSpace
{
    /// <summary>
    /// Represents an image used for graphics.
    /// </summary>
    public sealed class Texture
    {
        private Texture()
        {
            this._ID = GL.GenTexture();
        }

        /// <summary>
        /// Loads a texture from the specified image file. Any file that can be loaded
        /// into a bitmap can be loaded as a texture.
        /// </summary>
        public static Texture LoadFromFile(string FileName)
        {
            Texture t = new Texture();
            Bitmap bmp = new Bitmap(FileName);
            BitmapData bdata = bmp.LockBits(
                new Rectangle(0, 0, bmp.Width, bmp.Height), 
                ImageLockMode.ReadOnly, 
                System.Drawing.Imaging.PixelFormat.Format32bppArgb);

            t.BindTexture();
            GL.TexImage2D(TextureTarget.Texture2D, 0, PixelInternalFormat.Rgba, bdata.Width, bdata.Height,
               0, OpenTK.Graphics.OpenGL.PixelFormat.Bgra, PixelType.UnsignedByte, bdata.Scan0);

            bmp.UnlockBits(bdata);

            GL.TexParameter(TextureTarget.Texture2D, TextureParameterName.TextureMinFilter, (int)TextureMinFilter.Linear);
            GL.TexParameter(TextureTarget.Texture2D, TextureParameterName.TextureMagFilter, (int)TextureMagFilter.Linear);

            return t;
        }

        /// <summary>
        /// Binds the texture to the graphics context, making it the current texture.
        /// </summary>
        public void BindTexture()
        {
            GL.BindTexture(TextureTarget.Texture2D, this._ID);
        }

        private int _ID;
    }

    /// <summary>
    /// Point containing uv and texture data.
    /// </summary>
    public class TexturedPoint : Point
    {
        public override void Mix(Point Other, double Amount)
        {
            TexturedPoint tp = Other as TexturedPoint;
            if (tp != null)
            {
                this.U += (tp.U - this.U) * Amount;
                this.V += (tp.V - this.V) * Amount;
            }
            base.Mix(Other, Amount);
        }

        public override Point Copy()
        {
            TexturedPoint tp = new TexturedPoint();
            this.Clone(tp);
            return tp;
        }

        protected override void Clone(Point Point)
        {
            TexturedPoint tp = Point as TexturedPoint;
            tp.U = this.U;
            tp.V = this.V;
            base.Clone(Point);
        }

        public double U;
        public double V;
    }

    /// <summary>
    /// A material that maps a texture onto the supplied mesh.
    /// </summary>
    public class TextureNormalMaterial : BufferedMaterial
    {
        public TextureNormalMaterial(Texture Texture)
        {
            this._Texture = Texture;
        }
        
        public override int VertexStride
        {
            get 
            {
                return 8 * sizeof(float); 
            }
        }

        public override unsafe void FillVertexData(void* Vertex, Point Point, List<Triangle> Usages)
        {
            float* vert = (float*)Vertex;
            double u = 0.0;
            double v = 0.0;
            TexturedPoint tp = Point as TexturedPoint;
            if (tp != null)
            {
                u = tp.U;
                v = tp.V;
            }
            Vector pos = Point.Position;
            Vector norm = new Vector();
            foreach (Triangle tri in Usages)
            {
                norm = norm + tri.Normal;
            }
            norm.Normalize();

            vert[0] = (float)u;
            vert[1] = (float)v;
            vert[2] = (float)norm.X;
            vert[3] = (float)norm.Y;
            vert[4] = (float)norm.Z;
            vert[5] = (float)pos.X;
            vert[6] = (float)pos.Y;
            vert[7] = (float)pos.Z;
        }

        public override void SetVertexFormat()
        {
            GL.InterleavedArrays(InterleavedArrayFormat.T2fN3fV3f, 0, IntPtr.Zero);
        }

        public override void PreRender()
        {
            this._Texture.BindTexture();
            GL.TexEnv(TextureEnvTarget.TextureEnv, TextureEnvParameter.TextureEnvMode, (float)TextureEnvMode.Modulate);
        }


        private Texture _Texture;
    }
}
