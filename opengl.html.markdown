---
category: tool
tool: OpenGL
filename: learnopengl.cpp
contributors:
    - ["Simon Deitermann", "s.f.deitermann@t-online.de"]
---

**Open Graphics Library** (**OpenGL**) is a cross-language cross-platform application programming interface
(API) for rendering 2D computer graphics and 3D vector graphics.<sup>[1]</sup> In this tutorial we will be
focusing on modern OpenGL from 3.3 and above, ignoring "immediate-mode", Displaylists and
VBO's without use of Shaders.
I will be using C++ with SFML for window, image and context creation aswell as GLEW
for modern OpenGL extensions, though there are many other librarys available.

```cpp
// Creating an SFML window and OpenGL basic setup.
#include <GL/glew.h>
#include <GL/gl.h>
#include <SFML/Graphics.h>
#include <iostream>

int main() {
    // First we tell SFML how to setup our OpenGL context.
    sf::ContextSettings context{ 24,   // depth buffer bits
                                  8,   // stencil buffer bits
                                  4,   // MSAA samples
                                  3,   // major opengl version
                                  3 }; // minor opengl version
    // Now we create the window, enable VSync
    // and set the window active for OpenGL.
    sf::Window window{ sf::VideoMode{ 1024, 768 },
                       "opengl window",
                       sf::Style::Default,
		       context };
    window.setVerticalSyncEnabled(true);
    window.setActive(true);
    // After that we initialise GLEW and check if an error occurred.
    GLenum error;
    glewExperimental = GL_TRUE;
    if ((err = glewInit()) != GLEW_OK)
        std::cout << glewGetErrorString(err) << std::endl;
    // Here we set the color glClear will clear the buffers with.
    glClearColor(0.0f,    // red
                 0.0f,    // green
                 0.0f,    // blue
                 1.0f);   // alpha
    // Now we can start the event loop, poll for events and draw objects.
    sf::Event event{ };
    while (window.isOpen()) {
        while (window.pollEvent(event)) {
            if (event.type == sf::Event::Closed)
                window.close;
        }
        // Tell OpenGL to clear the color buffer
        // and the depth buffer, this will clear our window.
        glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
        // Flip front- and backbuffer.
        window.display();
    }
    return 0;
}
```

## Loading Shaders

After creating a window and our event loop we should create a function,
that sets up our shader program.

```cpp
GLuint createShaderProgram(const std::string& vertexShaderPath,
                           const std::string& fragmentShaderPath) {
    // Load the vertex shader source.
    std::stringstream ss{ };
    std::string vertexShaderSource{ };
    std::string fragmentShaderSource{ };
    std::ifstream file{ vertexShaderPath };
    if (file.is_open()) {
        ss << file.rdbuf();
        vertexShaderSource = ss.str();
        file.close();
    }
    // Clear the stringstream and load the fragment shader source.
    ss.str(std::string{ });
    file.open(fragmentShaderPath);
    if (file.is_open()) {
        ss << file.rdbuf();
        fragmentShaderSource = ss.str();
        file.close();
    }
    // Create the program.
    GLuint program = glCreateProgram();
    // Create the shaders.
    GLuint vertexShader = glCreateShader(GL_VERTEX_SHADER);
    GLuint fragmentShader = glCreateShader(GL_FRAGMENT_SHADER);
    // Now we can load the shader source into the shader objects and compile them.
    // Because glShaderSource() wants a const char* const*,
    // we must first create a const char* and then pass the reference.
    const char* cVertexSource = vertexShaderSource.c_str();
    glShaderSource(vertexShader,     // shader
                   1,                // number of strings
                   &cVertexSource,   // strings
                   nullptr);         // length of strings (nullptr for 1)
    glCompileShader(vertexShader);
    // Now we have to do the same for the fragment shader.
    const char* cFragmentSource = fragmentShaderSource.c_str();
    glShaderSource(fragmentShader, 1, &cFragmentSource, nullptr);
    glCompileShader(fragmentShader);
    // After attaching the source and compiling the shaders,
    // we attach them to the program;
    glAttachShader(program, vertexShader);
    glAttachShader(program, fragmentShader);
    glLinkProgram(program);
    // After linking the shaders we should detach and delete
    // them to prevent memory leak.
    glDetachShader(program, vertexShader);
    glDetachShader(program, fragmentShader);
    glDeleteShader(vertexShader);
    glDeleteShader(fragmentShader);
    // With everything done we can return the completed program.
    return program;
}
```

If you want to check the compilation log you can add the following between <code>glCompileShader()</code> and <code>glAttachShader()</code>.

```cpp
GLint logSize = 0;
std::vector<GLchar> logText{ };
glGetShaderiv(vertexShader,         // shader
              GL_INFO_LOG_LENGTH,   // requested parameter
              &logSize);            // return object
if (logSize > 0) {
    logText.resize(logSize);
    glGetShaderInfoLog(vertexShader,      // shader
                       logSize,           // buffer length
                       &logSize,          // returned length
                       logText.data());   // buffer
    std::cout << logText.data() << std::endl;
}
```

The same is possible after <code>glLinkProgram()</code>, just replace <code>glGetShaderiv()</code> with <code>glGetProgramiv()</code>
and <code>glGetShaderInfoLog()</code> with <code>glGetProgramInfoLog()</code>.

```cpp
// Now we can create a shader program with a vertex and a fragment shader.
// ...
glClearColor(0.0f, 0.0f, 0.0f, 1.0f);

GLuint program = createShaderProgram("vertex.glsl", "fragment.glsl");

sf::Event event{ };
// ...
// We also have to delete the program at the end of the application.
// ...
    }
    glDeleteProgram(program);	
    return 0;
}
// ...
```

Ofcourse we have to create the vertex and fragment shader before we can load them,
so lets create two basic shaders.

**Vertex Shader**

```glsl
// Declare which version of GLSL we use.
// Here we declare, that we want to use the OpenGL 3.3 version of GLSL.
#version 330 core
// At attribute location 0 we want an input variable of type vec3,
// that contains the position of the vertex.
// Setting the location is optional, if you don't set it you can ask for the
// location with glGetAttribLocation().
layout(location = 0) in vec3 position;
// Every shader starts in it's main function.
void main() {
    // gl_Position is a predefined variable that holds
    // the final vertex position.
    // It consists of a x, y, z and w coordinate.
    gl_Position = vec4(position, 1.0);
}
```

**Fragment Shader**

```glsl
#version 330 core
// The fragment shader does not have a predefined variable for
// the vertex color, so we have to define a output vec4,
// that holds the final vertex color.
out vec4 outColor;

void main() {
    // We simply set the output color to red.
    // The parameters are red, green, blue and alpha.
    outColor = vec4(1.0, 0.0, 0.0, 1.0);
}
```

## VAO and VBO
Now we need to define some vertex position we can pass to our shaders. Lets define a simple 2D quad.

```cpp
// The vertex data is defined in a counter-clockwise way,
// as this is the default front face.
std::vector<float> vertexData {
    -0.5f,  0.5f, 0.0f,
    -0.5f, -0.5f, 0.0f,
     0.5f, -0.5f, 0.0f,
     0.5f,  0.5f, 0.0f
};
// If you want to use a clockwise definition, you can simply call
glFrontFace(GL_CW);
// Next we need to define a Vertex Array Object (VAO).
// The VAO stores the current state while its active.
GLuint vao = 0;
glGenVertexArrays(1, &vao);
glBindVertexArray(vao);
// With the VAO active we can now create a Vertex Buffer Object (VBO).
// The VBO stores our vertex data.
GLuint vbo = 0;
glGenBuffers(1, &vbo);
glBindBuffer(GL_ARRAY_BUFFER, vbo);
// For reading and copying there are also GL_*_READ and GL_*_COPY,
// if your data changes more often use GL_DYNAMIC_* or GL_STREAM_*.
glBufferData(GL_ARRAY_BUFFER,     // target buffer
             sizeof(vertexData[0]) * vertexData.size(),   // size
             vertexData.data(),   // data
             GL_STATIC_DRAW);     // usage
// After filling the VBO link it to the location 0 in our vertex shader,
// which holds the vertex position.
// ...
// To ask for the attribute location, if you haven't set it:
GLint posLocation = glGetAttribLocation(program, "position");
// ..
glEnableVertexAttribArray(0);
glVertexAttribPointer(0, 3,       // location and size
                      GL_FLOAT,   // type of data
                      GL_FALSE,   // normalized (always false for floats)
                      0,          // stride (interleaved arrays)
                      nullptr);   // offset (interleaved arrays)
// Everything should now be saved in our VAO and we can unbind it and the VBO.
glBindVertexArray(0);
glBindBuffer(GL_ARRAY_BUFFER, 0);
// Now we can draw the vertex data in our render loop.
// ...
glClear(GL_COLOR_BUFFER_BIT);
// Tell OpenGL we want to use our shader program.
glUseProgram(program);
// Binding the VAO loads the data we need.
glBindVertexArray(vao);
// We want to draw a quad starting at index 0 of the VBO using 4 indices.
glDrawArrays(GL_QUADS, 0, 4);
glBindVertexArray(0);
window.display();
// ...
// Ofcource we have to delete the allocated memory for the VAO and VBO at
// the end of our application.
// ...
glDeleteBuffers(1, &vbo);
glDeleteVertexArrays(1, &vao);
glDeleteProgram(program);
return 0;
// ...
```

You can find the current code here: [OpenGL - 1](https://pastebin.com/W8jdmVHD).

## More VBO's and Color
Let's create another VBO for some colors.

```cpp
std::vector<float> colorData {
    1.0f, 0.0f, 0.0f,
    0.0f, 1.0f, 0.0f,
    0.0f, 0.0f, 1.0f,
    1.0f, 1.0f, 0.0f
};
```

Next we can simply change some previous parameters to create a second VBO for our colors.

```cpp
// ...
GLuint vbo[2];
glGenBuffers(2, vbo);
glBindBuffer(GL_ARRAY_BUFFER, vbo[0]);
// ...
glDeleteBuffers(2, vbo);
/ ...
// With these changes made we now have to load our color data into the new VBO
// ...
glVertexAttribPointer(0, 3, GL_FLOAT, GL_FALSE, 0, nullptr);

glBindBuffer(GL_ARRAY_BUFFER, vbo[1]);
glBufferData(GL_ARRAY_BUFFER, sizeof(colorData[0]) * colorData.size(),
             colorData.data(), GL_STATIC_DRAW);
glEnableVertexAttribArray(1);
glVertexAttribPointer(1, 3, GL_FLOAT, GL_FALSE, 0, nullptr);

glBindVertexArray(0);  
// ...
```

Next we have to change our vertex shader to pass the color data to the fragment shader.<br>
**Vertex Shader**

```glsl
#version 330 core

layout(location = 0) in vec3 position;
// The new location has to differ from any other input variable.
// It is the same index we need to pass to
// glEnableVertexAttribArray() and glVertexAttribPointer().
layout(location = 1) in vec3 color;

out vec3 fColor;

void main() {
    fColor = color;
    gl_Position = vec4(position, 1.0);
}
```

**Fragment Shader**

```glsl
#version 330 core

in vec3 fColor;

out vec4 outColor;

void main() {
    outColor = vec4(fColor, 1.0);
}
```

We define a new input variable ```color``` which represents our color data, this data
is passed on to ```fColor```, which is an output variable of our vertex shader and
becomes an input variable for our fragment shader.
It is imporatant that variables passed between shaders have the exact same name
and type.

## Handling VBO's

```cpp
// If you want to completely clear and refill a VBO use glBufferData(),
// just like we did before.
// ...
// There are two mains ways to update a subset of a VBO's data.
// To update a VBO with existing data
std::vector<float> newSubData {
	-0.25f, 0.5f, 0.0f
};
glBindBuffer(GL_ARRAY_BUFFER, vbo[0]);
glBufferSubData(GL_ARRAY_BUFFER,      // target buffer
                0,                    // offset
                sizeof(newSubData[0]) * newSubData.size(),   // size
                newSubData.data());   // data
// This would update the first three values in our vbo[0] buffer.
// If you want to update starting at a specific location just set the second
// parameter to that value and multiply by the types size.
// ...
// If you are streaming data, for example from a file,
// it is faster to directly pass the data to the buffer.
// Other access values are GL_READ_ONLY and GL_READ_WRITE.
glBindBuffer(GL_ARRAY_BUFFER, vbo[0]);
// You can static_cast<float*>() the void* to be more safe.
void* Ptr = glMapBuffer(GL_ARRAY_BUFFER,   // buffer to map
                        GL_WRITE_ONLY);    // access to buffer
memcpy(Ptr, newSubData.data(), sizeof(newSubData[0]) * newSubData.size());
// To copy to a specific location add a destination offset to memcpy().
glUnmapBuffer(GL_ARRAY_BUFFER);
// ...
// There is also a way to copy data from one buffer to another,
// If we have two VBO's vbo[0] and vbo[1], we can copy like so
// You can also read from GL_ARRAY_BUFFER.
glBindBuffer(GL_COPY_READ_BUFFER, vbo[0]);
// GL_COPY_READ_BUFFER and GL_COPY_WRITE_BUFFER are specifically for
// copying buffer data.
glBindBuffer(GL_COPY_WRITE_BUFFER, vbo[1]);
glCopyBufferSubData(GL_COPY_READ_BUFFER,    // read buffer
                    GL_COPY_WRITE_BUFFER,   // write buffer
                    0, 0,                   // read and write offset
                    sizeof(vbo[0]) * 3);    // copy size
// This will copy the first three elements from vbo[0] to vbo[1].
```

## Uniforms

**Fragment Shader**

```glsl
// Uniforms are variables like in and out, however,
// we can change them easily by passing new values with glUniform().
// Lets define a time variable in our fragment shader.
#version 330 core
// Unlike a in/out variable we can use a uniform in every shader,
// without the need to pass it to the next one, they are global.
// Don't use locations already used for attributes!
// Uniform layout locations require OpenGL 4.3!
layout(location = 10) uniform float time;

in vec3 fColor;

out vec4 outColor;

void main() {
    // Create a sine wave from 0 to 1 based on the time passed to the shader.
    float factor = (sin(time * 2) + 1) / 2;
    outColor = vec4(fColor.r * factor, fColor.g * factor, fColor.b * factor, 1.0);
}
```

Back to our source code.

```cpp
// If we haven't set the layout location, we can ask for it.
GLint timeLocation = glGetUniformLocation(program, "time");
// ...
// Also we should define a Timer counting the current time.
sf::Clock clock{ };
// In out render loop we can now update the uniform every frame.
    // ...
    window.display();
    glUniform1f(10,   // location
                clock.getElapsedTime().asSeconds());   // data
}
// ...
```

With the time getting updated every frame the quad should now be changing from
fully colored to pitch black.
There are different types of glUniform() you can find simple documentation here:
[glUniform - OpenGL Refpage](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glUniform.xhtml)

## Indexing and IBO's

Element Array Buffers or more commonly Index Buffer Objects (IBO) allow us to use the
same vertex data again which makes drawing a lot easier and faster. here's an example:

```cpp
// Lets create a quad from two rectangles.
// We can simply use the old vertex data from before.
// First, we have to create the IBO.
// The index is referring to the first declaration in the VBO.
std::vector<unsigned int> iboData {
    0, 1, 2,
    0, 2, 3
};
// That's it, as you can see we could reuse 0 - the top left
// and 2 - the bottom right.
// Now that we have our data, we have to fill it into a buffer.
// Note that this has to happen between the two glBindVertexArray() calls,
// so it gets saved into the VAO.
GLuint ibo = 0;
glGenBufferrs(1, &ibo);
glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, ibo);
glBufferData(GL_ELEMENT_ARRAY_BUFFER, sizeof(iboData[0]) * iboData.size(),
             iboData.data(), GL_STATIC_DRAW);
// Next in our render loop, we replace glDrawArrays() with:
glDrawElements(GL_TRIANGLES, iboData.size(), GL_UNSIGNED_INT, nullptr);
// Remember to delete the allocated memory for the IBO.
```

You can find the current code here: [OpenGL - 2](https://pastebin.com/R3Z9ACDE).

## Textures

To load out texture we first need a library that loads the data, for simplicity I will be
using SFML, however there are a lot of librarys for loading image data.

```cpp
// Lets save we have a texture called "my_tex.tga", we can load it with:
sf::Image image;
image.loadFromFile("my_tex.tga");
// We have to flip the texture around the y-Axis, because OpenGL's texture
// origin is the bottom left corner, not the top left.
image.flipVertically();
// After loading it we have to create a OpenGL texture.
GLuint texture = 0;
glGenTextures(1, &texture);
glBindTexture(GL_TEXTURE_2D, texture);
// Specify what happens when the coordinates are out of range.
glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT);
glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT);
// Specify the filtering if the object is very large.
glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
// Load the image data to the texture.
glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, image.getSize().x, image.getSize().y,
             0, GL_RGBA, GL_UNSIGNED_BYTE, image.getPixelsPtr());
// Unbind the texture to prevent modifications.
glBindTexture(GL_TEXTURE_2D, 0);
// Delete the texture at the end of the application.
// ...
glDeleteTextures(1, &texture);
```

Ofcourse there are more texture formats than only 2D textures,
You can find further information on parameters here:
[glBindTexture - OpenGL Refpage](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glBindTexture.xhtml)<br>
[glTexImage2D - OpenGL Refpage](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glTexImage2D.xhtml)<br>
[glTexParameter - OpenGL Refpage](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glTexParameter.xhtml)<br>

```cpp
// With the texture created, we now have to specify the UV,
// or in OpenGL terms ST coordinates.
std::vector<float> texCoords {
    // The texture coordinates have to match the triangles/quad
    // definition.
    0.0f, 1.0f,	   // start at top-left
    0.0f, 0.0f,	   // go round counter-clockwise
    1.0f, 0.0f,
    1.0f, 1.0f     // end at top-right
};
// Now we increase the VBO's size again just like we did for the colors.
// ...
GLuint vbo[3];
glGenBuffers(3, vbo);
// ...
glDeleteBuffers(3, vbo);
// ...
// Load the texture coordinates into the new buffer.
glBindBuffer(GL_ARRAY_BUFFER, vbo[2]);
glBufferData(GL_ARRAY_BUFFER, sizeof(texCoords[0]) * texCoords.size(),
             texCoords.data(), GL_STATIC_DRAW);
glEnableVertexAttribArray(2);
glVertexAttribPointer(2, 2, GL_FLOAT, GL_FALSE, 0, nullptr);
// Because the VAO does not store the texture we have to bind it before drawing.
// ...
glBindVertexArray(vao);
glBindTexture(GL_TEXTURE_2D, texture);
glDrawElements(GL_TRIANGLES, iboData.size(), GL_UNSIGNED_INT, nullptr);
// ...
```

Change the shaders to pass the data to the fragment shader.<br>

**Vertex Shader**

```glsl
#version 330 core

layout(location = 0) in vec3 position;
layout(location = 1) in vec3 color;
layout(location = 2) in vec2 texCoords;

out vec3 fColor;
out vec2 fTexCoords;

void main() {
    fColor = color;
    fTexCoords = texCoords;
    gl_Position = vec4(position, 1.0);
}
```

**Fragment Shader**

```glsl
#version 330 core
// sampler2D represents our 2D texture.
uniform sampler2D tex;
uniform float time;

in vec3 fColor;
in vec2 fTexCoords;

out vec4 outColor;

void main() {
    // texture() loads the current texure data at the specified texture coords,
    // then we can simply multiply them by our color.
    outColor = texture(tex, fTexCoords) * vec4(fColor, 1.0);
}
```

You can find the current code here: [OpenGL - 3](https://pastebin.com/u3bcwM6q)

## Matrix Transformation

**Vertex Shader**

```glsl
#version 330 core

layout(location = 0) in vec3 position;
layout(location = 1) in vec3 color;
layout(location = 2) in vec2 texCoords;
// Create 2 4x4 matricies, 1 for the projection matrix
// and 1 for the model matrix.
// Because we draw in a static scene, we don't need a view matrix.
uniform mat4 projection;
uniform mat4 model;

out vec3 fColor;
out vec2 fTexCoords;

void main() {
    fColor = color;
    fTexCoords = texCoords;
    // Multiplay the position by the model matrix and then by the
    // projection matrix.
    // Beware order of multiplication for matricies!
    gl_Position = projection * model * vec4(position, 1.0);
}
```

In our source we now need to change the vertex data, create a model- and a projection matrix.

```cpp
// The new vertex data, counter-clockwise declaration.
std::vector<float> vertexData {  
    0.0f, 1.0f, 0.0f,   // top left
    0.0f, 0.0f, 0.0f,   // bottom left
    1.0f, 0.0f, 0.0f,   // bottom right
    1.0f, 1.0f, 0.0f    // top right
};
// Request the location of our matricies.
GLint projectionLocation = glGetUniformLocation(program, "projection");
GLint modelLocation = glGetUniformLocation(program, "model");
// Declaring the matricies.
// Orthogonal matrix for a 1024x768 window.
std::vector<float> projection {  
    0.001953f,       0.0f,  0.0f, 0.0f,
         0.0f, -0.002604f,  0.0f, 0.0f,
         0.0f,       0.0f, -1.0f, 0.0f,
        -1.0f,       1.0f,  0.0f, 1.0f
};
// Model matrix translating to x 50, y 50
// and scaling to x 200, y 200.
std::vector<float> model {  
    200.0f,   0.0f, 0.0f, 0.0f,
      0.0f, 200.0f, 0.0f, 0.0f,
      0.0f,   0.0f, 1.0f, 0.0f,
     50.0f,  50.0f, 0.0f, 1.0f
};
// Now we can send our calculated matricies to the program.
glUseProgram(program);
glUniformMatrix4fv(projectionLocation,   // location
                   1,                    // count
                   GL_FALSE,             // transpose the matrix
                   projection.data());   // data
glUniformMatrix4fv(modelLocation, 1, GL_FALSE, model.data());
glUseProgram(0);
// The glUniform*() calls have to be done, while the program is bound.
```

The application should now display the texture at the defined position and size.<br>
You can find the current code here: [OpenGL - 4](https://pastebin.com/9ahpFLkY)

```cpp
// There are many math librarys for OpenGL, which create
// matricies and vectors, the most used in C++ is glm (OpenGL Mathematics).
// Its a header only library.
// The same code using glm would look like:
glm::mat4 projection{ glm::ortho(0.0f, 1024.0f, 768.0f, 0.0f) };
glUniformMatrix4fv(projectionLocation, 1, GL_FALSE,
                   glm::value_ptr(projection));
// Initialise the model matrix to the identity matrix, otherwise every
// multiplication would be 0.
glm::mat4 model{ 1.0f };
model = glm::translate(model, glm::vec3{ 50.0f, 50.0f, 0.0f });
model = glm::scale(model, glm::vec3{ 200.0f, 200.0f, 0.0f });
glUniformMatrix4fv(modelLocation, 1, GL_FALSE,
                   glm::value_ptr(model));
```

## Geometry Shader

Geometry shaders were introduced in OpenGL 3.2, they can produce vertices
that are send to the rasterizer. They can also change the primitive type e.g.
they can take a point as an input and output other primitives.
Geometry shaders are inbetween the vertex and the fragment shader.

**Vertex Shader**

```glsl
#version 330 core

layout(location = 0) in vec3 position;
layout(location = 1) in vec3 color;
// Create an output interface block passed to the next shadaer stage.
// Interface blocks can be used to structure data passed between shaders.
out VS_OUT {
    vec3 color;
} vs_out;

void main() {
    vs_out.color = color
    gl_Position = vec4(position, 1.0);
}
```

**Geometry Shader**

```glsl
#version 330 core
// The geometry shader takes in points.
layout(points) in;
// It outputs a triangle every 3 vertices emitted.
layout(triangle_strip, max_vertices = 3) out;
// VS_OUT becomes an input variable in the geometry shader.
// Every input to the geometry shader in treated as an array.
in VS_OUT {
    vec3 color;
} gs_in[];
// Output color for the fragment shader.
// You can also simply define color as 'out vec3 color',
// If you don't want to use interface blocks.
out GS_OUT {
    vec3 color;
} gs_out;

void main() {
    // Each emit calls the fragment shader, so we set a color for each vertex.
    gs_out.color = mix(gs_in[0].color, vec3(1.0, 0.0, 0.0), 0.5);
    // Move 0.5 units to the left and emit the new vertex.
    // gl_in[] is the current vertex from the vertex shader, here we only
    // use 0, because we are receiving points.
    gl_Position = gl_in[0].gl_Position + vec4(-0.5, 0.0, 0.0, 0.0);
    EmitVertex();
    gs_out.color = mix(gs_in[0].color, vec3(0.0, 1.0, 0.0), 0.5);
    // Move 0.5 units to the right and emit the new vertex.
    gl_Position = gl_in[0].gl_Position + vec4(0.5, 0.0, 0.0, 0.0);
    EmitVertex();
    gs_out.color = mix(gs_in[0].color, vec3(0.0, 0.0, 1.0), 0.5);
    // Move 0.5 units up and emit the new vertex.
    gl_Position = gl_in[0].gl_Position + vec4(0.0, 0.75, 0.0, 0.0);
    EmitVertex();
    EndPrimitive();
}
```

**Fragment Shader**

```glsl
in GS_OUT {
    vec3 color;
} fs_in;

out vec4 outColor;

void main() {
    outColor = vec4(fs_in.color, 1.0);
}
```

If you now store a single point with a single color in a VBO and draw them,
you should see a triangle, with your color mixed half way between
red, green and blue on each vertex.


## Quotes
<sup>[1]</sup>[OpenGL - Wikipedia](https://en.wikipedia.org/wiki/OpenGL)

## Books

- OpenGL Superbible - Fifth Edition (covering OpenGL 3.3)
- OpenGL Programming Guide - Eighth Edition (covering OpenGL 4.3)
