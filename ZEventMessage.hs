module ZEventMessage where

import Graphics.Rendering.OpenGL

data Button = LeftButton | MiddleButton | RightButton

data ZEvent = KeyPress Char
            | KeyRelease Char
            | MouseDown Button (GLint, GLint)
            | MouseUp Button
            | MouseMove (GLint, GLint)
