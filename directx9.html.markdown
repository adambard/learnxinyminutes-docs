---
tool: DirectX 9
name: DirectX 9
filename: learndirectx9.cpp
contributors:
    - ["Simon Deitermann", "s.f.deitermann@t-online.de"]
---

**Microsoft DirectX** is a collection of application programming interfaces (APIs) for handling tasks related to
multimedia, especially game programming and video, on Microsoft platforms. Originally, the names of these APIs
all began with Direct, such as Direct3D, DirectDraw, DirectMusic, DirectPlay, DirectSound, and so forth. [...]
Direct3D (the 3D graphics API within DirectX) is widely used in the development of video games for Microsoft
Windows and the Xbox line of consoles.<sup>[1]</sup>

In this tutorial we will be focusing on DirectX 9, which is not as low-level as it's sucessors, which are aimed at programmers very familiar with how graphics hardware works. It makes a great starting point for learning Direct3D. In this tutorial I will be using the Win32-API for window handling and the DirectX 2010 SDK.

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
    // Check if a window handle had been created.
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
    // Store the rectangle size of the window.
    RECT clientRect{ };
    GetClientRect(hWnd, &clientRect);
    // Initialize Direct3D
    _d3d = Direct3DCreate9(D3D_SDK_VERSION);
    // Get the display mode which format will be the window format.
    D3DDISPLAYMODE displayMode{ };
    _d3d->GetAdapterDisplayMode(D3DADAPTER_DEFAULT, // use default graphics card
                                &displayMode);      // display mode struct
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
    // Variable to store results of methods to check if everything succeds.
    HRESULT result{ };
    result = _d3d->CreateDevice(D3DADAPTER_DEFAULT, // use default graphics card
                                D3DDEVTYPE_HAL,     // use hardware acceleration
                                hWnd,               // the window handle
                                D3DCREATE_HARDWARE_VERTEXPROCESSING,
                                    // vertices are processed by the hardware
                                &pp,       // the present parameters
                                &_device); // struct to store the device
    // Return if the device creation failed.
    // It is helpful to set breakpoints at the return line.
    if (FAILED(result))
        return false;
    // Create a viewport which hold information about which region to draw to.
    D3DVIEWPORT9 viewport{ };
    viewport.X = 0;         // start at top left corner
    viewport.Y = 0;         // ..
    viewport.Width = clientRect.right;   // use the entire window
    viewport.Height = clientRect.bottom; // ..
    viewport.MinZ = 0.0f;   // minimun view distance
    viewport.MaxZ = 100.0f; // maximum view distance
    // Apply the created viewport.
    result = _device->SetViewport(&viewport);
    // Always check if something failed everywhere.
    if (FAILED(result))
        return false;
    // Everything was successful, return true.
    return true;
}
```
Back in our **WinMain** function:
```cpp
    // ...
    // Check if Direct3D initialization succeded, else exit the application.
    if (!InitD3D(hWnd))
        return -1;
        
    MSG msg{ };
    while (_running) {
        while (PeekMessage(&msg, hWnd, 0, 0, PM_REMOVE)) {
            TranslateMessage(&msg);
            DispatchMessage(&msg);
        }
        // Clear to render target to a specified color.
        _device->Clear(0,               // number of rect to specified
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
// At the top of the file we need to add some includes.
#include <vector>
// First we declare a new ComPtr holding a vertex buffer.
ComPtr<IDirect3DVertexBuffer9> _vertexBuffer{ };
// Lets define a funtion to calculate the byte size of a std::vector
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
                 &buffer,               // target buffer
                 nullptr);              // always nullptr (reserved)
    // Check if buffer was created successful.
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
// If you want to diable culling just call:
_device->SetRenderState(D3DRS_CULLMODE, D3DCULL_NONE);
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
Before we can use the vertex buffer to draw our primitives, we first need to set up the matricies.
```cpp
// Lets create a new funtions for the matrix transformations.
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
        1024.0f / 768.0f,    // aspect ratio (use client rect instead)
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
```
Back in the **WinMain** function.
```cpp
// ...
if (!(_vertexBuffer = CreateVertexBuffer(vertices)))
    return -1;
// Call the transformation setup function.
if (!SetupTransform())
    return -1;
// ...
```
## Rendering
Now that everything is setup we can start drawing our first 2D triangle sitting in 3D space.
```cpp
// ...
if (!SetupTransform())
    return -1;
// First we have to bind our vertex buffer to the data stream.
HRESULT result{ };
result = _device->SetStreamSource(0,                   // use the first stream
                                  _vertexBuffer.Get(), // pass our vertex data
                                  0,                   // no offset
                                  sizeof(VStruct));    // size of vertex info
if (FAILED(result))
    return -1;

D3DXMATRIX world{ };
D3DXMatrixScaling(&world, // matrix to scale
                  10,     // x scaling
                  10,     // y scaling
                  1);     // z scaling
_device->SetTransform(D3DTS_WORLD, &world);

MSG msg{ };
    while (_running) {
    // ...
        _device->Clear(0, nullptr, D3DCLEAR_TARGET,
                       D3DXCOLOR{ 0.0f, 0.0f, 0.0f, 1.0f }, 0.0f, 0);
        // With the data stream set we can call the draw functions.
        _device->BeginScene();
        _device->DrawPrimitive(D3DPT_TRIANGLELIST, // primitive type
                               0,                  // start vertex
                               1);                 // primitive count
        _device->EndScene();
    
        _device->Present(nullptr, nullptr, nullptr, nullptr);
    // ...
```
You should now be viewing a 10x10 units colored triangle from 20 units away.
You can find the complete working code here: [DirectX - 1](https://pastebin.com/YkSF2rkk)



## Quotes
<sup>[1]</sup>[DirectX - Wikipedia](https://en.wikipedia.org/wiki/DirectX)
