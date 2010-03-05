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
    /// A renderable that sets the texture for the enclosed renderables.
    /// </summary>
    public class TextureRenderable : NestedRenderable
    {
        public TextureRenderable(Texture Texture, IRenderable Inner)
        {
            this._Texture = Texture;
            this._Inner = Inner;
        }

        /// <summary>
        /// Gets the texture this renders with.
        /// </summary>
        public Texture Texture
        {
            get
            {
                return this._Texture;
            }
        }

        public override IEnumerable<IRenderable> Nested
        {
            get 
            {
                return new IRenderable[] { this._Inner }; 
            }
        }

        public override void PreRender()
        {
            this._Texture.BindTexture();
            GL.TexEnv(TextureEnvTarget.TextureEnv, TextureEnvParameter.TextureEnvMode, (float)TextureEnvMode.Modulate);
        }

        public override void PostRender()
        {
            GL.BindTexture(TextureTarget.Texture2D, 0);
        }

        private Texture _Texture;
        private IRenderable _Inner;
    }
}
