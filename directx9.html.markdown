---
category: tool
tool: DirectX 9
filename: learndirectx9.cpp
contributors:
    - ["Simon Deitermann", "s.f.deitermann@t-online.de"]
---

**Microsoft DirectX** is a collection of application programming interfaces (APIs) for handling tasks related to
multimedia, especially game programming and video, on Microsoft platforms. Originally, the names of these APIs
all began with Direct, such as Direct3D, DirectDraw, DirectMusic, DirectPlay, DirectSound, and so forth. [...]
Direct3D (the 3D graphics API within DirectX) is widely used in the development of video games for Microsoft
Windows and the Xbox line of consoles.<sup>[1]</sup>

In this tutorial we will be focusing on DirectX 9, which is not as low-level as it's successors, which are aimed at programmers very familiar with how graphics hardware works. It makes a great starting point for learning Direct3D. In this tutorial I will be using the Win32-API for window handling and the DirectX 2010 SDK.

## Window creation

```cpp
#include <Windows.h>

bool _running{ false };

LRESULT CALLBACK WndProc(HWND hWnd, UINT msg, WPARAM wParam, LPARAM lParam) {
    // Handle incoming message.
    switch (msg) {
        // Set running to false if the user tries to close the window.
        case WM_DESTROY:
            _running = false;
            PostQuitMessage(0);
            break;
    }
    // Return the handled event.
    return DefWindowProc(hWnd, msg, wParam, lParam);
}

int WINAPI WinMain(HINSTANCE hInstance, HINSTANCE hPrevInstance,
                   LPSTR lpCmdLine, int nCmdShow) {
    // Set window properties we want to use.
    WNDCLASSEX wndEx{ };
    wndEx.cbSize        = sizeof(WNDCLASSEX);        // structure size
    wndEx.style         = CS_VREDRAW | CS_HREDRAW;   // class styles
    wndEx.lpfnWndProc   = WndProc;                   // window procedure
    wndEx.cbClsExtra    = 0;                         // extra memory (struct)
    wndEx.cbWndExtra    = 0;                         // extra memory (window)
    wndEx.hInstance     = hInstance;                 // module instance
    wndEx.hIcon         = LoadIcon(nullptr, IDI_APPLICATION); // icon
    wndEx.hCursor       = LoadCursor(nullptr, IDC_ARROW);     // cursor
    wndEx.hbrBackground = (HBRUSH) COLOR_WINDOW;     // background color
    wndEx.lpszMenuName  = nullptr;                   // menu name
    wndEx.lpszClassName = "DirectXClass";            // register class name
    wndEx.hIconSm       = nullptr;                   // small icon (taskbar)
    // Register created class for window creation.
    RegisterClassEx(&wndEx);
    // Create a new window handle.
    HWND hWnd{ nullptr };
    // Create a new window handle using the registered class.
    hWnd = CreateWindow("DirectXClass",      // registered class
                        "directx window",    // window title
                        WS_OVERLAPPEDWINDOW, // window style
                        50, 50,              // x, y (position)
                        1024, 768,           // width, height (size)
                        nullptr,             // parent window
                        nullptr,             // menu
                        hInstance,           // module instance
                        nullptr);            // struct for infos
    // Check if a window handle has been created.
    if (!hWnd)
        return -1;   
    // Show and update the new window.
    ShowWindow(hWnd, nCmdShow);
    UpdateWindow(hWnd);
    // Start the game loop and send incoming messages to the window procedure.
    _running = true;
    MSG msg{ };
    while (_running) {
        while (PeekMessage(&msg, hWnd, 0, 0, PM_REMOVE)) {
            TranslateMessage(&msg);
            DispatchMessage(&msg);
        }
    }  
    return 0;
}
```

This should create a window, that can the moved, resized and closed.

## Direct3D initialization

```cpp
// Includes DirectX 9 structures and functions.
// Remember to link "d3d9.lib" and "d3dx9.lib".
// For "d3dx9.lib" the DirectX SDK (June 2010) is needed.
// Don't forget to set your subsystem to Windows.
#include <d3d9.h>
#include <d3dx9.h>
// Includes the ComPtr, a smart pointer automatically releasing COM objects.
#include <wrl.h>
using namespace Microsoft::WRL;
// Next we define some Direct3D9 interface structs we need.
ComPtr<IDirect3D9> _d3d{ };
ComPtr<IDirect3DDevice9> _device{ };
```

With all interfaces declared we can now initialize Direct3D.

```cpp
bool InitD3D(HWND hWnd) {
    // Store the size of the window rectangle.
    RECT clientRect{ };
    GetClientRect(hWnd, &clientRect);
    // Initialize Direct3D
    _d3d = Direct3DCreate9(D3D_SDK_VERSION);
    // Get the display mode which format will be the window format.
    D3DDISPLAYMODE displayMode{ };
    _d3d->GetAdapterDisplayMode(D3DADAPTER_DEFAULT, // use default graphics card
                                &displayMode);      // display mode pointer
    // Next we have to set some presentation parameters.
    D3DPRESENT_PARAMETERS pp{ };
    pp.BackBufferWidth = clientRect.right;    // width is window width
    pp.BackBufferHeight = clientRect.bottom;  // height is window height
    pp.BackBufferFormat = displayMode.Format; // use adapter format
    pp.BackBufferCount = 1;                   // 1 back buffer (default)
    pp.SwapEffect = D3DSWAPEFFECT_DISCARD;    // discard after presentation
    pp.hDeviceWindow = hWnd;                  // associated window handle
    pp.Windowed = true;                       // display in window mode
    pp.Flags = 0;                             // no special flags
    // Variable to store results of methods to check if everything succeeded.
    HRESULT result{ };
    result = _d3d->CreateDevice(D3DADAPTER_DEFAULT, // use default graphics card
                                D3DDEVTYPE_HAL,     // use hardware acceleration
                                hWnd,               // the window handle
                                D3DCREATE_HARDWARE_VERTEXPROCESSING,
                                    // vertices are processed by the hardware
                                &pp,       // the present parameters
                                &_device); // struct to store the device
    // Return false if the device creation failed.
    // It is helpful to set breakpoints at the return line.
    if (FAILED(result))
        return false;
    // Create a viewport which hold information about which region to draw to.
    D3DVIEWPORT9 viewport{ };
    viewport.X = 0;         // start at top left corner
    viewport.Y = 0;         // ..
    viewport.Width = clientRect.right;   // use the entire window
    viewport.Height = clientRect.bottom; // ..
    viewport.MinZ = 0.0f;   // minimum view distance
    viewport.MaxZ = 100.0f; // maximum view distance
    // Apply the created viewport.
    result = _device->SetViewport(&viewport);
    // Always check if something failed.
    if (FAILED(result))
        return false;
    // Everything was successful, return true.
    return true;
}
// ...
// Back in our WinMain function we call our initialization function.
// ...
// Check if Direct3D initialization succeeded, else exit the application.
if (!InitD3D(hWnd))
    return -1;
        
MSG msg{ };
while (_running) {
    while (PeekMessage(&msg, hWnd, 0, 0, PM_REMOVE)) {
        TranslateMessage(&msg);
        DispatchMessage(&msg);
    }
    // Clear to render target to a specified color.
    _device->Clear(0,               // number of rects to clear
                   nullptr,         // indicates to clear the entire window
                   D3DCLEAR_TARGET, // clear all render targets
                   D3DXCOLOR{ 1.0f, 0.0f, 0.0f, 1.0f }, // color (red)
                   0.0f,            // depth buffer clear value
                   0);              // stencil buffer clear value
    // ...
    // Drawing operations go here.
    // ...
    // Flip the front- and backbuffer.
    _device->Present(nullptr,  // no source rectangle
                     nullptr,  // no destination rectangle
                     nullptr,  // don't change the current window handle
                     nullptr); // pretty much always nullptr
}
// ...
```

Now the window should be displayed in a bright red color.

## Vertex Buffer

Let's create a vertex buffer to store the vertices for our triangle

```cpp
// At the top of the file we need to add a include.
#include <vector>
// First we declare a new ComPtr holding a vertex buffer.
ComPtr<IDirect3DVertexBuffer9> _vertexBuffer{ };
// Lets define a function to calculate the byte size of a std::vector
template <typename T>
unsigned int GetByteSize(const std::vector<T>& vec) {
    return sizeof(vec[0]) * vec.size();
}
// Define "flexible vertex format" describing the content of our vertex struct.
// Use the defined color as diffuse color.
const unsigned long VertexStructFVF = D3DFVF_XYZ | D3DFVF_DIFFUSE;
// Define a struct representing the vertex data the buffer will hold.
struct VStruct {
    float x, y, z;   // store the 3D position
    D3DCOLOR color;  // store a color
};
// Declare a new function to create a vertex buffer.
IDirect3DVertexBuffer9* CreateBuffer(const std::vector<VStruct>& vertices) {
    // Declare the buffer to be returned.
    IDirect3DVertexBuffer9* buffer{ };
    HRESULT result{ };
    result = _device->CreateVertexBuffer(
                 GetByteSize(vertices), // vector size in bytes
                 0,                     // data usage
                 VertexStructFVF,       // FVF of the struct
                 D3DPOOL_DEFAULT,       // use default pool for the buffer
                 &buffer,               // receiving buffer
                 nullptr);              // special shared handle
    // Check if buffer was created successfully.
    if (FAILED(result))
        return nullptr;
    // Create a data pointer for copying the vertex data
    void* data{ };
    // Lock the buffer to get a buffer for data storage.
    result = buffer->Lock(0,                     // byte offset
                          GetByteSize(vertices), // size to lock
                          &data,                 // receiving data pointer
                          0);                    // special lock flags
    // Check if buffer was locked successfully.
    if (FAILED(result))
        return nullptr;
    // Copy the vertex data using C standard libraries memcpy.
    memcpy(data, vertices.data(), GetByteSize(vertices));
    buffer->Unlock();
    // Set the FVF Direct3D uses for rendering.
    _device->SetFVF(VertexStructFVF);
    // If everything was successful return the filled vertex buffer.
    return buffer;
}
```

In our **WinMain** we can now call the new function after the Direct3D initialization.

```cpp
// ...
if (!InitD3D(hWnd))
    return -1;
// Define the vertices we need to draw a triangle.
// Values are declared in a clockwise direction else Direct3D would cull them.
// If you want to disable culling just call:
// _device->SetRenderState(D3DRS_CULLMODE, D3DCULL_NONE);
std::vector<VStruct> vertices {
    // Bottom left
    VStruct{ -1.0f, -1.0f, 1.0f, D3DXCOLOR{ 1.0f, 0.0f, 0.0f, 1.0f } },
    // Top left
    VStruct{ -1.0f,  1.0f, 1.0f, D3DXCOLOR{ 0.0f, 1.0f, 0.0f, 1.0f } },
    // Top right
    VStruct{  1.0f,  1.0f, 1.0f, D3DXCOLOR{ 0.0f, 0.0f, 1.0f, 1.0f } }
};
// Try to create the vertex buffer else exit the application.
if (!(_vertexBuffer = CreateBuffer(vertices)))
    return -1;
// ...
```

## Transformations

Before we can use the vertex buffer to draw our primitives, we first need to set up the matrices.

```cpp
// Lets create a new functions for the matrix transformations.
bool SetupTransform() {
    // Create a view matrix that transforms world space to
    // view space.
    D3DXMATRIX view{ };
    // Use a left-handed coordinate system.
    D3DXMatrixLookAtLH(
        &view,                              // receiving matrix
        &D3DXVECTOR3{ 0.0f, 0.0f, -20.0f }, // "camera" position
        &D3DXVECTOR3{ 0.0f, 0.0f, 0.0f },   // position where to look at
        &D3DXVECTOR3{ 0.0f, 1.0f, 0.0f });  // positive y-axis is up
    HRESULT result{ };
    result = _device->SetTransform(D3DTS_VIEW, &view); // apply the view matrix
    if (FAILED(result))
        return false;
    // Create a projection matrix that defines the view frustrum.
    // It transforms the view space to projection space.
    D3DXMATRIX projection{ };
    // Create a perspective projection using a left-handed coordinate system.
    D3DXMatrixPerspectiveFovLH(
        &projection,         // receiving matrix
        D3DXToRadian(60.0f), // field of view in radians
        1024.0f / 768.0f,    // aspect ratio (width / height)
        0.0f,                // minimum view distance
        100.0f);             // maximum view distance
    result = _device->SetTransform(D3DTS_PROJECTION, &projection);
    if (FAILED(result))
        return false;
    // Disable lighting for now so we can see what we want to render.
    result = _device->SetRenderState(D3DRS_LIGHTING, false);
    // View and projection matrix are successfully applied, return true.
    return true;
}
// ...
// Back in the WinMain function we can now call the transformation function.
// ...
if (!(_vertexBuffer = CreateVertexBuffer(vertices)))
    return -1;
// Call the transformation setup function.
if (!SetupTransform())
    return -1;
// ...
```

## Rendering

Now that everything is setup we can start drawing our first 2D triangle in 3D space.

```cpp
// ...
if (!SetupTransform())
    return -1;
// First we have to bind our vertex buffer to the data stream.
HRESULT result{ };
result = _device->SetStreamSource(0,                   // use the default stream
                                  _vertexBuffer.Get(), // pass the vertex buffer
                                  0,                   // no offset
                                  sizeof(VStruct));    // size of vertex struct
if (FAILED(result))
    return -1;

// Create a world transformation matrix and set it to an identity matrix.
D3DXMATRIX world{ };
D3DXMatrixIdentity(&world);
// Create a scalation matrix scaling our primitive by 10 in the x,
// 10 in the y and keeping the z direction.
D3DXMATRIX scaling{ };
D3DXMatrixScaling(&scaling, // matrix to scale
                  10,       // x scaling
                  10,       // y scaling
                  1);       // z scaling
// Create a rotation matrix storing the current rotation of our primitive.
// We set the current rotation matrix to an identity matrix for now.
D3DXMATRIX rotation{ };
D3DXMatrixIdentity(&rotation);
// Now we multiply the scalation and rotation matrix and store the result
// in the world matrix.
D3DXMatrixMultiply(&world,     // destination matrix
                   &scaling,   // matrix 1
                   &rotation); // matrix 2
// Apply the current world matrix.
_device->SetTransform(D3DTS_WORLD, &world);
// Disable culling so we can see the back of our primitive when it rotates.
_device->SetRenderState(D3DRS_CULLMODE, D3DCULL_NONE);
// The default cullmode is D3DCULL_CW.
// After we used our the rotation matrix for multiplication we can set it
// to rotate a small amount.
// D3DXToRadian() function converts degree to radians.
D3DXMatrixRotationY(&rotation,           // matrix to rotate
                    D3DXToRadian(0.5f)); // rotation angle in radians

MSG msg{ };
    while (_running) {
    // ...
        _device->Clear(0, nullptr, D3DCLEAR_TARGET,
                       D3DXCOLOR{ 0.0f, 0.0f, 0.0f, 1.0f }, 0.0f, 0);
        // With everything setup we can call the draw function.
        _device->BeginScene();
        _device->DrawPrimitive(D3DPT_TRIANGLELIST, // primitive type
                               0,                  // start vertex
                               1);                 // primitive count
        _device->EndScene();
    
        _device->Present(nullptr, nullptr, nullptr, nullptr);
        // We can keep multiplying the world matrix with our rotation matrix
        // to add it's rotation to the world matrix.
        D3DXMatrixMultiply(&world, &world, &rotation);
        // Update the modified world matrix.
        _device->SetTransform(D3DTS_WORLD, &world);
    // ...
```

You should now be viewing a 10x10 units colored triangle from 20 units away, rotating around its origin.<br>
You can find the complete working code here: [DirectX - 1](https://pastebin.com/YkSF2rkk)

## Indexing

To make it easier to draw primitives sharing a lot of vertices we can use indexing, so we only have to declare the unique vertices and put the order they are called in another array.

```cpp
// First we declare a new ComPtr for our index buffer.
ComPtr<IDirect3DIndexBuffer9> _indexBuffer{ };
// ...
// Declare a function creating a index buffer from a std::vector
IDirect3DIndexBuffer9* CreateIBuffer(std::vector<unsigned int>& indices) {
    IDirect3DIndexBuffer9* buffer{ };
    HRESULT result{ };
    result = _device->CreateIndexBuffer(
                 GetByteSize(indices), // vector size in bytes
                 0,                    // data usage 
                 D3DFMT_INDEX32,       // format is 32 bit int
                 D3DPOOL_DEFAULT,      // default pool
                 &buffer,              // receiving buffer
                 nullptr);             // special shared handle
    if (FAILED(result))
        return nullptr;
    // Create a data pointer pointing to the buffer data.
    void* data{ };
    result = buffer->Lock(0,                    // byte offset
                          GetByteSize(indices), // byte size
                          &data,                // receiving data pointer
                          0);                   // special lock flag
    if (FAILED(result))
        return nullptr;
    // Copy the index data and unlock after copying.
    memcpy(data, indices.data(), GetByteSize(indices));
    buffer->Unlock();
    // Return the filled index buffer.
    return buffer;
}
// ...
// In our WinMain we can now change the vertex data and create new index data.
// ...
std::vector<VStruct> vertices {
    VStruct{ -1.0f, -1.0f, 1.0f, D3DXCOLOR{ 1.0f, 0.0f, 0.0f, 1.0f } },
    VStruct{ -1.0f,  1.0f, 1.0f, D3DXCOLOR{ 0.0f, 1.0f, 0.0f, 1.0f } },
    VStruct{  1.0f,  1.0f, 1.0f, D3DXCOLOR{ 0.0f, 0.0f, 1.0f, 1.0f } },
    // Add a vertex for the bottom right.
    VStruct{  1.0f, -1.0f, 1.0f, D3DXCOLOR{ 1.0f, 1.0f, 0.0f, 1.0f } }
};
// Declare the index data, here we build a rectangle from two triangles.
std::vector<unsigned int> indices {
    0, 1, 2, // the first triangle (b,left -> t,left -> t,right)
    0, 2, 3  // the second triangle (b,left -> t,right -> b,right)
};
// ...
// Now we call the "CreateIBuffer" function to create a index buffer.
// ...
if (!(_indexBuffer = CreateIBuffer(indices)))
    return -1;
// ...
// After binding the vertex buffer we have to bind the index buffer to
// use indexed rendering.
result = _device->SetStreamSource(0, _vertexBuffer.Get(), 0, sizeof(VStruct));
if (FAILED(result))
    return -1;
// Bind the index data to the default data stream.
result = _device->SetIndices(_indexBuffer.Get())
if (FAILED(result))
    return -1;
// ...
// Now we replace the "DrawPrimitive" function with an indexed version.
_device->DrawIndexedPrimitive(D3DPT_TRIANGLELIST, // primitive type
                              0,                  // base vertex index
                              0,                  // minimum index
                              indices.size(),     // amount of vertices
                              0,                  // start in index buffer
                              2);                 // primitive count
// ...
```

Now you should see a colored rectangle made up of 2 triangles. If you set the primitive count in the "DrawIndexedPrimitive" method to 1 only the first triangle should be rendered and if you set the start of the index buffer to 3 and the primitive count to 1 only the second triangle should be rendered.<br>
You can find the complete working code here: [DirectX - 2](https://pastebin.com/yWBPWPRG)

## Vertex declaration

Instead of using the old "flexible vertex format" we should use vertex declarations instead, as the FVF declarations get converted to vertex declarations internally anyway.

```cpp
// First we have to REMOVE the following lines:
const unsigned long VertexStructFVF = D3DFVF_XYZ | D3DFVF_DIFFUSE;
// and
_device->SetFVF(VertexStructFVF);
// ...
// We also have to change the vertex buffer creation FVF-flag.
result = _device->CreateVertexBuffer(
                      GetByteSize(vertices),
                      0,
                      0,        // <- 0 indicates we use vertex declarations
                      D3DPOOL_DEFAULT,
                      &buffer,
                      nullptr); 
// Next we have to declare a new ComPtr.
ComPtr<IDirect3DVertexDeclaration9> _vertexDecl{ };
// ...
result = _device->SetIndices(_indexBuffer.Get());
if (FAILED(result))
    return -1;
// Now we have to declare and apply the vertex declaration.
// Create a vector of vertex elements making up the vertex declaration.
std::vector<D3DVERTEXELEMENT9> vertexDeclDesc {
    { 0,                     // stream index
      0,                     // byte offset from the struct beginning
      D3DDECLTYPE_FLOAT3,    // data type (3d float vector)
      D3DDECLMETHOD_DEFAULT, // tessellator operation
      D3DDECLUSAGE_POSITION,  // usage of the data
      0 },                   // index (multiples usage of the same type)
    { 0,
      12,                    // byte offset (3 * sizeof(float) bytes)
      D3DDECLTYPE_D3DCOLOR,
      D3DDECLMETHOD_DEFAULT,
      D3DDECLUSAGE_COLOR,
      0 },
    D3DDECL_END()            // marks the end of the vertex declaration
};
// After having defined the vector we can create a vertex declaration from it.
result = _device->CreateVertexDeclaration(
                      vertexDeclDesc.data(), // the vertex element array
                      &_vertexDecl);         // receiving pointer
if (FAILED(result)) 
    return -1;
// Apply the created vertex declaration.
_device->SetVertexDeclaration(_vertexDecl.Get());
// ...
```

## Shader

The maximum shader model for Direct3D 9 is shader model 3.0. Even though every modern graphics card should support it, it is best to check for capabilities.

```cpp
// ...
_device->SetVertexDeclaration(_vertexDecl.Get());
// First we have to request the device capabilities.
D3DCAPS9 deviceCaps{ };
_device->GetDeviceCaps(&deviceCaps);
// Now we check if shader model 3.0 is supported for the vertex shader.
if (deviceCaps.VertexShaderVersion < D3DVS_VERSION(3, 0))
    return -1;
// And the same for the pixel shader.
if (deviceCaps.PixelShaderVersion < D3DPS_VERSION(3, 0))
    return -1;
```

Now that we are sure shader model 3.0 is supported let's create the vertex and pixel shader files.
DirectX 9 introduced the HLSL (**High Level Shading Language**), a C-like shader language, which
simplified the shader programming a lot, as you could only write shaders in shader assembly in DirectX 8.
Let's create a simple vertex- and pixel shader. 

**Vertex Shader**

```cpp
// 3 4x4 float matrices representing the matrices we set in the fixed-function
// pipeline by using the SetTransform() method.
float4x4 projectionMatrix;
float4x4 viewMatrix;
float4x4 worldMatrix;
// The input struct to the vertex shader.
// It holds a 3d float vector for the position and a 4d float vector
// for the color.
struct VS_INPUT {
    float3 position : POSITION;
    float4 color : COLOR;
};
// The output struct of the vertex shader, that is passed to the pixel shader.
struct VS_OUTPUT {
    float4 position : POSITION;
    float4 color : COLOR;
};
// The main function of the vertex shader returns the output it sends to the
// pixel shader and receives it's input as a parameter.
VS_OUTPUT main(VS_INPUT input) {
    // Declare a empty struct, that the vertex shader returns.
    VS_OUTPUT output;
    // Set the output position to the input position and set
    // the w-component to 1, as the input position is a 3d vector and
    // the output position a 4d vector.
    output.position = float4(input.position, 1.0f);
    // Multiply the output position step by step with the world, view and
    // projection matrices.
    output.position = mul(output.position, worldMatrix);	
    output.position = mul(output.position, viewMatrix);
    output.position = mul(output.position, projectionMatrix);
	// Pass the input color unchanged to the pixel shader.
    output.color = input.color;
    // Return the output struct to the pixel shader.
    // The position value is automatically used as the vertex position.
    return output;
}
```

**Pixel Shader**

```cpp
// The pixel shader input struct must be the same as the vertex shader output!
struct PS_INPUT {
    float4 position : POSITION;
    float4 color : COLOR;
};
// The pixel shader simply returns a 4d vector representing the vertex color.
// It receives it's input as a parameter just like the vertex shader.
// We have to declare the output semantic as color to it gets interpreted
// correctly.
float4 main(PS_INPUT input) : COLOR {
    return input.color;
}
```

For more on semantics: [DirectX - Semantics](https://docs.microsoft.com/en-us/windows/win32/direct3dhlsl/dx-graphics-hlsl-semantics#vertex-shader-semantics)

Now we have to do quite some changes to the code.

```cpp
ComPtr<IDirect3DDevice9> _device{ };
ComPtr<IDirect3DVertexBuffer9> _vertexBuffer{ };
ComPtr<IDirect3DIndexBuffer9> _indexBuffer{ };
ComPtr<IDirect3DVertexDeclaration9> _vertexDecl{ };
// We have to add a ComPtr for the vertex- and pixel shader, aswell as one
// for the constants (matrices) in our vertex shader.
ComPtr<IDirect3DVertexShader9> _vertexShader{ };
ComPtr<IDirect3DPixelShader9> _pixelShader{ };
ComPtr<ID3DXConstantTable> _vertexTable{ };
// Declare the world and rotation matrix as global, because we use them in
// WinMain and SetupTransform now.
D3DXMATRIX _worldMatrix{ };
D3DXMATRIX _rotationMatrix{ };
// ...
bool SetupTransform() {
    // Set the world and rotation matrix to an identity matrix.
    D3DXMatrixIdentity(&_worldMatrix);
    D3DXMatrixIdentity(&_rotationMatrix);
	
    D3DXMATRIX scaling{ };
    D3DXMatrixScaling(&scaling, 10, 10, 1);
    D3DXMatrixMultiply(&_worldMatrix, &scaling, &_rotationMatrix);
    // After multiplying the scalation and rotation matrix the have to pass
    // them to the shader, by using a method from the constant table
    // of the vertex shader.
    HRESULT result{ };
    result = _vertexTable->SetMatrix(
                         _device.Get(),   // direct3d device
                         "worldMatrix",   // matrix name in the shader
                          &_worldMatrix); // pointer to the matrix
    if (FAILED(result))
        return false;

    D3DXMATRIX view{ };
    D3DXMatrixLookAtLH(&view, &D3DXVECTOR3{ 0.0f, 0.0f, -20.0f },
           &D3DXVECTOR3{ 0.0f, 0.0f, 0.0f }, &D3DXVECTOR3{ 0.0f, 1.0f, 0.0f });
    // Do the same for the view matrix.
    result = _vertexTable->SetMatrix(
	                       _device.Get(), // direct 3d device
	                       "viewMatrix",  // matrix name
	                       &view);        // matrix
    if (FAILED(result))
        return false;

    D3DXMATRIX projection{ };
    D3DXMatrixPerspectiveFovLH(&projection, D3DXToRadian(60.0f),
        1024.0f / 768.0f, 0.0f, 100.0f);
    // And also for the projection matrix.
    result = _vertexTable->SetMatrix(
	                       _device.Get(),
	                       "projectionMatrix",
	                       &projection);
    if (FAILED(result))
        return false;

    D3DXMatrixRotationY(&_rotationMatrix, D3DXToRadian(0.5f));
    return true;
}
// ...
// Vertex and index buffer creation aswell as initialization stay unchanged.
// ...
// After checking that shader model 3.0 is available we have to compile and
// create the shaders.
// Declare two temporary buffers storing the compiled shader code.
ID3DXBuffer* vertexShaderBuffer{ };
ID3DXBuffer* pixelShaderBuffer{ };
result = D3DXCompileShaderFromFile("vertex.hlsl",  // shader name
                                   nullptr,        // macro definitions
                                   nullptr,        // special includes
                                   "main",         // entry point name
                                   "vs_3_0",       // shader model version
                                   0,              // special flags
                                   &vertexShaderBuffer, // code buffer
                                   nullptr,        // error message
                                   &_vertexTable); // constant table
if (FAILED(result))
    return -1;
// After the vertex shader compile the pixel shader.
result = D3DXCompileShaderFromFile("pixel.hlsl",
                                   nullptr,
                                   nullptr,
                                   "main",
                                   "ps_3_0", // pixel shader model 3.0
                                   0,
                                   &pixelShaderBuffer,
                                   nullptr,
                                   nullptr); // no need for a constant table
if (FAILED(result))
    return -1;
// Create the vertex shader from the code buffer.
result = _device->CreateVertexShader(
             (DWORD*)vertexShaderBuffer->GetBufferPointer(), // code buffer
             &_vertexShader); // vertex shader pointer
if (FAILED(result))
    return -1;
	
result = _device->CreatePixelShader(
             (DWORD*)pixelShaderBuffer->GetBufferPointer(),
             &_pixelShader);
if (FAILED(result))
    return -1;
// Release the temporary code buffers after the shaders are created.
vertexShaderBuffer->Release();
pixelShaderBuffer->Release();
// Apply the vertex- and pixel shader.
_device->SetVertexShader(_vertexShader.Get());
_device->SetPixelShader(_pixelShader.Get());
// Apply the transform after the shaders have been set.
if (!SetupTransform())
    return -1;
// You can also REMOVE the call so set the lighting render state.
_device->SetRenderState(D3DRS_LIGHTING, false);
```

You can find the complete code here: [DirectX - 3](https://pastebin.com/y4NrvawY)

## Texturing

```cpp
// First we need to declare a ComPtr for the texture.
ComPtr<IDirect3DTexture9> _texture{ };
// Then we have to change the vertex struct.
struct VStruct {
    float x, y, z;
    float u, v;      // Add texture u and v coordinates
    D3DCOLOR color;
};
// In the vertex declaration we have to add the texture coordinates.
// the top left of the texture is u: 0, v: 0.
std::vector<VStruct> vertices {
    VStruct{ -1.0f, -1.0f, 1.0f, 0.0f, 1.0f, ... }, // bottom left
    VStruct{ -1.0f,  1.0f, 1.0f, 0.0f, 0.0f, ... }, // top left
    VStruct{  1.0f,  1.0f, 1.0f, 1.0f, 0.0f, ... }, // top right
    VStruct{  1.0f, -1.0f, 1.0f, 1.0f, 1.0f, ... }  // bottom right
};
// Next is the vertex declaration.
std::vector<D3DVERTEXELEMENT9> vertexDecl{
    {0, 0, D3DDECLTYPE_FLOAT3, D3DDECLMETHOD_DEFAULT, D3DDECLUSAGE_POSITION, 0},
    // Add a 2d float vector used for texture coordinates.
    {0, 12, D3DDECLTYPE_FLOAT2, D3DDECLMETHOD_DEFAULT, D3DDECLUSAGE_TEXCOORD, 0},
    // The color offset is not (3 + 2) * sizeof(float) = 20 bytes
    {0, 20, D3DDECLTYPE_D3DCOLOR, D3DDECLMETHOD_DEFAULT, D3DDECLUSAGE_COLOR, 0},
    D3DDECL_END()
};
// Now we have to load the texture and pass its to the shader.
// ...
_device->SetRenderState(D3DRS_CULLMODE, D3DCULL_NONE);
// Create a Direct3D texture from a png file.
result = D3DXCreateTextureFromFile(_device.Get(), // direct3d device
                                   "texture.png", // texture path
                                   &_texture);    // receiving texture pointer
if (FAILED(result))
    return -1;
// Attach the texture to shader stage 0, which is equal to texture register 0
// in the pixel shader.
_device->SetTexture(0, _texture.Get());
```

With the main code ready we now have to adjust the shaders to these changes.

**Vertex Shader**

```cpp
float4x4 projectionMatrix;
float4x4 viewMatrix;
float4x4 worldMatrix;
// Add the texture coordinates to the vertex shader in- and output.
struct VS_INPUT {
    float3 position : POSITION;
    float2 texcoord : TEXCOORD;
    float4 color : COLOR;
};

struct VS_OUTPUT {
    float4 position : POSITION;
    float2 texcoord : TEXCOORD;
    float4 color : COLOR;
};

VS_OUTPUT main(VS_INPUT input) {
    VS_OUTPUT output;
    
    output.position = float4(input.position, 1.0f);
    output.position = mul(output.position, worldMatrix);	
    output.position = mul(output.position, viewMatrix);
    output.position = mul(output.position, projectionMatrix);

    output.color = input.color;
    // Set the texcoord output to the input.
    output.texcoord = input.texcoord;
    
    return output;
}
```

**Pixel Shader**

```cpp
// Create  a sampler called "sam0" using sampler register 0, which is equal
// to the texture stage 0, to which we passed the texture.
sampler sam0 : register(s0);

struct PS_INPUT {
    float4 position : POSITION;
    float2 texcoord : TEXCOORD;
    float4 color : COLOR;
};

float4 main(PS_INPUT input) : COLOR{
    // Do a linear interpolation between the texture color and the input color
    // using 75% of the input color.
    // tex2D returns the texture data at the specified texture coordinate.
    return lerp(tex2D(sam0, input.texcoord), input.color, 0.75f);
}
```

## Quotes
<sup>[1]</sup>[DirectX - Wikipedia](https://en.wikipedia.org/wiki/DirectX)
