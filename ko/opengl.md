---
category: framework
name: DirectX 9
filename: learndirectx9.cpp
contributors:
    - ["Simon Deitermann", "s.f.deitermann@t-online.de"]
translators:
    - ["Taeyoon Kim", "https://github.com/partrita"]
---

**Microsoft DirectX**는 멀티미디어, 특히 게임 프로그래밍 및 비디오와 관련된 작업을 Microsoft 플랫폼에서 처리하기 위한 애플리케이션 프로그래밍 인터페이스(API) 모음입니다. 원래 이러한 API의 이름은 모두 Direct로 시작했습니다(예: Direct3D, DirectDraw, DirectMusic, DirectPlay, DirectSound 등). [...] Direct3D(DirectX 내의 3D 그래픽 API)는 Microsoft Windows 및 Xbox 콘솔 라인용 비디오 게임 개발에 널리 사용됩니다.<sup>[1]</sup>

이 튜토리얼에서는 DirectX 9에 초점을 맞출 것입니다. DirectX 9는 후속 버전만큼 저수준은 아니지만, 그래픽 하드웨어 작동 방식에 매우 익숙한 프로그래머를 대상으로 합니다. Direct3D를 배우기 위한 훌륭한 시작점입니다. 이 튜토리얼에서는 창 처리 및 DirectX 2010 SDK에 Win32-API를 사용할 것입니다.

## 창 생성

```cpp
#include <Windows.h>

bool _running{ false };

LRESULT CALLBACK WndProc(HWND hWnd, UINT msg, WPARAM wParam, LPARAM lParam) {
    // 들어오는 메시지 처리.
    switch (msg) {
        // 사용자가 창을 닫으려고 하면 running을 false로 설정합니다.
        case WM_DESTROY:
            _running = false;
            PostQuitMessage(0);
            break;
    }
    // 처리된 이벤트 반환.
    return DefWindowProc(hWnd, msg, wParam, lParam);
}

int WINAPI WinMain(HINSTANCE hInstance, HINSTANCE hPrevInstance,
                   LPSTR lpCmdLine, int nCmdShow) {
    // 사용할 창 속성 설정.
    WNDCLASSEX wndEx{ };
    wndEx.cbSize        = sizeof(WNDCLASSEX);        // 구조체 크기
    wndEx.style         = CS_VREDRAW | CS_HREDRAW;   // 클래스 스타일
    wndEx.lpfnWndProc   = WndProc;                   // 창 프로시저
    wndEx.cbClsExtra    = 0;                         // 추가 메모리 (구조체)
    wndEx.cbWndExtra    = 0;                         // 추가 메모리 (창)
    wndEx.hInstance     = hInstance;                 // 모듈 인스턴스
    wndEx.hIcon         = LoadIcon(nullptr, IDI_APPLICATION); // 아이콘
    wndEx.hCursor       = LoadCursor(nullptr, IDC_ARROW);     // 커서
    wndEx.hbrBackground = (HBRUSH) COLOR_WINDOW;     // 배경색
    wndEx.lpszMenuName  = nullptr;                   // 메뉴 이름
    wndEx.lpszClassName = "DirectXClass";            // 클래스 이름 등록
    wndEx.hIconSm       = nullptr;                   // 작은 아이콘 (작업 표시줄)
    // 창 생성을 위해 생성된 클래스 등록.
    RegisterClassEx(&wndEx);
    // 새 창 핸들 생성.
    HWND hWnd{ nullptr };
    // 등록된 클래스를 사용하여 새 창 핸들 생성.
    hWnd = CreateWindow("DirectXClass",      // 등록된 클래스
                        "directx window",    // 창 제목
                        WS_OVERLAPPEDWINDOW, // 창 스타일
                        50, 50,              // x, y (위치)
                        1024, 768,           // 너비, 높이 (크기)
                        nullptr,             // 부모 창
                        nullptr,             // 메뉴
                        hInstance,           // 모듈 인스턴스
                        nullptr);            // 정보 구조체
    // 창 핸들이 생성되었는지 확인.
    if (!hWnd)
        return -1;
    // 새 창 표시 및 업데이트.
    ShowWindow(hWnd, nCmdShow);
    UpdateWindow(hWnd);
    // 게임 루프를 시작하고 들어오는 메시지를 창 프로시저로 보냅니다.
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

이렇게 하면 이동, 크기 조정 및 닫을 수 있는 창이 생성됩니다.

## Direct3D 초기화

```cpp
// DirectX 9 구조체 및 함수를 포함합니다.
// "d3d9.lib" 및 "d3dx9.lib"를 링크하는 것을 잊지 마십시오.
// "d3dx9.lib"의 경우 DirectX SDK (2010년 6월)가 필요합니다.
// 서브시스템을 Windows로 설정하는 것을 잊지 마십시오.
#include <d3d9>
#include <d3dx9.h>
// COM 객체를 자동으로 해제하는 스마트 포인터인 ComPtr를 포함합니다.
#include <wrl.h>
using namespace Microsoft::WRL;
// 다음으로 필요한 Direct3D9 인터페이스 구조체를 정의합니다.
ComPtr<IDirect3D9> _d3d{ };
ComPtr<IDirect3DDevice9> _device{ };
```

모든 인터페이스가 선언되었으므로 이제 Direct3D를 초기화할 수 있습니다.

```cpp
bool InitD3D(HWND hWnd) {
    // 창 사각형의 크기를 저장합니다.
    RECT clientRect{ };
    GetClientRect(hWnd, &clientRect);
    // Direct3D 초기화
    _d3d = Direct3DCreate9(D3D_SDK_VERSION);
    // 디스플레이 모드를 가져옵니다. 이 모드는 창 형식으로 사용됩니다.
    D3DDISPLAYMODE displayMode{ };
    _d3d->GetAdapterDisplayMode(D3DADAPTER_DEFAULT, // 기본 그래픽 카드 사용
                                &displayMode);      // 디스플레이 모드 포인터
    // 다음으로 일부 프레젠테이션 매개변수를 설정해야 합니다.
    D3DPRESENT_PARAMETERS pp{ };
    pp.BackBufferWidth = clientRect.right;    // 너비는 창 너비
    pp.BackBufferHeight = clientRect.bottom;  // 높이는 창 높이
    pp.BackBufferFormat = displayMode.Format; // 어댑터 형식 사용
    pp.BackBufferCount = 1;                   // 1개의 백 버퍼 (기본값)
    pp.SwapEffect = D3DSWAPEFFECT_DISCARD;    // 프레젠테이션 후 버림
    pp.hDeviceWindow = hWnd;                  // 연결된 창 핸들
    pp.Windowed = true;                       // 창 모드로 표시
    pp.Flags = 0;                             // 특수 플래그 없음
    // 모든 것이 성공했는지 확인하기 위해 메서드 결과를 저장할 변수입니다.
    HRESULT result{ };
    result = _d3d->CreateDevice(D3DADAPTER_DEFAULT, // 기본 그래픽 카드 사용
                                D3DDEVTYPE_HAL,     // 하드웨어 가속 사용
                                hWnd,               // 창 핸들
                                D3DCREATE_HARDWARE_VERTEXPROCESSING,
                                    // 정점은 하드웨어에서 처리됩니다.
                                &pp,       // 현재 매개변수
                                &_device); // 장치를 저장할 구조체
    // 장치 생성이 실패하면 false를 반환합니다.
    // 반환 줄에 중단점을 설정하는 것이 도움이 됩니다.
    if (FAILED(result))
        return false;
    // 그릴 영역에 대한 정보를 담는 뷰포트를 생성합니다.
    D3DVIEWPORT9 viewport{ };
    viewport.X = 0;         // 왼쪽 상단 모서리에서 시작
    viewport.Y = 0;         // ..
    viewport.Width = clientRect.right;   // 전체 창 사용
    viewport.Height = clientRect.bottom; // ..
    viewport.MinZ = 0.0f;   // 최소 보기 거리
    viewport.MaxZ = 100.0f; // 최대 보기 거리
    // 생성된 뷰포트 적용.
    result = _device->SetViewport(&viewport);
    // 항상 실패 여부를 확인하십시오.
    if (FAILED(result))
        return false;
    // 모든 것이 성공했으므로 true를 반환합니다.
    return true;
}
// ...
// WinMain 함수에서 초기화 함수를 호출합니다.
// ...
// Direct3D 초기화가 성공했는지 확인하고, 그렇지 않으면 애플리케이션을 종료합니다.
if (!InitD3D(hWnd))
    return -1;

MSG msg{ };
while (_running) {
    while (PeekMessage(&msg, hWnd, 0, 0, PM_REMOVE)) {
        TranslateMessage(&msg);
        DispatchMessage(&msg);
    }
    // 지정된 색상으로 렌더링 대상을 지웁니다.
    _device->Clear(0,               // 지울 사각형 수
                   nullptr,         // 전체 창을 지울 것을 나타냅니다.
                   D3DCLEAR_TARGET, // 모든 렌더링 대상을 지웁니다.
                   D3DXCOLOR{ 1.0f, 0.0f, 0.0f, 1.0f }, // 색상 (빨간색)
                   0.0f,            // 깊이 버퍼 지우기 값
                   0);              // 스텐실 버퍼 지우기 값
    // ...
    // 여기에 그리기 작업이 들어갑니다.
    // ...
    // 전면 및 후면 버퍼를 뒤집습니다.
    _device->Present(nullptr,  // 소스 사각형 없음
                     nullptr,  // 대상 사각형 없음
                     nullptr,  // 현재 창 핸들을 변경하지 않습니다.
                     nullptr); // 거의 항상 nullptr
}
// ...
```

이제 창이 밝은 빨간색으로 표시되어야 합니다.

## 정점 버퍼

삼각형의 정점을 저장할 정점 버퍼를 만들어 보겠습니다.

```cpp
// 먼저 파일에 include를 추가해야 합니다.
#include <vector>
// 먼저 정점 버퍼를 담을 새 ComPtr를 선언합니다.
ComPtr<IDirect3DVertexBuffer9> _vertexBuffer{ };
// std::vector의 바이트 크기를 계산하는 함수를 정의해 보겠습니다.
template <typename T>
unsigned int GetByteSize(const std::vector<T>& vec) {
    return sizeof(vec[0]) * vec.size();
}
// 정점 구조체의 내용을 설명하는 "유연한 정점 형식"을 정의합니다.
// 정의된 색상을 확산 색상으로 사용합니다.
const unsigned long VertexStructFVF = D3DFVF_XYZ | D3DFVF_DIFFUSE;
// 버퍼가 담을 정점 데이터를 나타내는 구조체를 정의합니다.
struct VStruct {
    float x, y, z;   // 3D 위치 저장
    D3DCOLOR color;  // 색상 저장
};
// 정점 버퍼를 생성하는 새 함수를 선언합니다.
IDirect3DVertexBuffer9* CreateBuffer(const std::vector<VStruct>& vertices) {
    // 반환할 버퍼를 선언합니다.
    IDirect3DVertexBuffer9* buffer{ };
    HRESULT result{ };
    result = _device->CreateVertexBuffer(
                 GetByteSize(vertices), // 벡터 크기(바이트)
                 0,                     // 데이터 사용량
                 VertexStructFVF,       // 구조체의 FVF
                 D3DPOOL_DEFAULT,       // 버퍼의 기본 풀 사용
                 &buffer,               // 수신 버퍼
                 nullptr);              // 특수 공유 핸들
    // 버퍼가 성공적으로 생성되었는지 확인합니다.
    if (FAILED(result))
        return nullptr;
    // 정점 데이터를 복사하기 위한 데이터 포인터를 생성합니다.
    void* data{ };
    // 데이터 저장을 위한 버퍼를 얻기 위해 버퍼를 잠급니다.
    result = buffer->Lock(0,                     // 바이트 오프셋
                          GetByteSize(vertices), // 잠글 크기
                          &data,                 // 수신 데이터 포인터
                          0);                    // 특수 잠금 플래그
    // 버퍼가 성공적으로 잠겼는지 확인합니다.
    if (FAILED(result))
        return nullptr;
    // C 표준 라이브러리 memcpy를 사용하여 정점 데이터를 복사합니다.
    memcpy(data, vertices.data(), GetByteSize(vertices));
    buffer->Unlock();
    // 렌더링에 사용할 FVF Direct3D를 설정합니다.
    _device->SetFVF(VertexStructFVF);
    // 모든 것이 성공했으면 채워진 정점 버퍼를 반환합니다.
    return buffer;
}
```

**WinMain**에서 Direct3D 초기화 후 새 함수를 호출할 수 있습니다.

```cpp
// ...
if (!InitD3D(hWnd))
    return -1;
// 삼각형을 그리는 데 필요한 정점을 정의합니다.
// 값은 시계 방향으로 선언됩니다. 그렇지 않으면 Direct3D가 컬링합니다.
// 컬링을 비활성화하려면 다음을 호출하십시오:
// _device->SetRenderState(D3DRS_CULLMODE, D3DCULL_NONE);
std::vector<VStruct> vertices {
    // 왼쪽 하단
    VStruct{ -1.0f, -1.0f, 1.0f, D3DXCOLOR{ 1.0f, 0.0f, 0.0f, 1.0f } },
    // 왼쪽 상단
    VStruct{ -1.0f,  1.0f, 1.0f, D3DXCOLOR{ 0.0f, 1.0f, 0.0f, 1.0f } },
    // 오른쪽 상단
    VStruct{  1.0f,  1.0f, 1.0f, D3DXCOLOR{ 0.0f, 0.0f, 1.0f, 1.0f } }
};
// 정점 버퍼를 생성하거나 애플리케이션을 종료합니다.
if (!(_vertexBuffer = CreateBuffer(vertices)))
    return -1;
// ...
```

## 변환

정점 버퍼를 사용하여 기본 요소를 그리기 전에 먼저 행렬을 설정해야 합니다.

```cpp
// 행렬 변환을 위한 새 함수를 만들어 보겠습니다.
bool SetupTransform() {
    // 월드 공간을 뷰 공간으로 변환하는 뷰 행렬을 생성합니다.
    D3DXMATRIX view{ };
    // 왼손 좌표계를 사용합니다.
    D3DXMatrixLookAtLH(
        &view,                              // 수신 행렬
        &D3DXVECTOR3{ 0.0f, 0.0f, -20.0f }, // "카메라" 위치
        &D3DXVECTOR3{ 0.0f, 0.0f, 0.0f },   // 볼 위치
        &D3DXVECTOR3{ 0.0f, 1.0f, 0.0f });  // 양의 y축이 위쪽
    HRESULT result{ };
    result = _device->SetTransform(D3DTS_VIEW, &view); // 뷰 행렬 적용
    if (FAILED(result))
        return false;
    // 뷰 절두체를 정의하는 투영 행렬을 생성합니다.
    // 뷰 공간을 투영 공간으로 변환합니다.
    D3DXMATRIX projection{ };
    // 왼손 좌표계를 사용하여 원근 투영을 생성합니다.
    D3DXMatrixPerspectiveFovLH(
        &projection,         // 수신 행렬
        D3DXToRadian(60.0f), // 라디안 단위의 시야각
        1024.0f / 768.0f,    // 종횡비 (너비 / 높이)
        0.0f,                // 최소 보기 거리
        100.0f);             // 최대 보기 거리
    result = _device->SetTransform(D3DTS_PROJECTION, &projection);
    if (FAILED(result))
        return false;
    // 렌더링하려는 것을 볼 수 있도록 조명을 비활성화합니다.
    result = _device->SetRenderState(D3DRS_LIGHTING, false);
    // 뷰 및 투영 행렬이 성공적으로 적용되었으므로 true를 반환합니다.
    return true;
}
// ...
// WinMain 함수에서 이제 변환 함수를 호출할 수 있습니다.
// ...
if (!(_vertexBuffer = CreateVertexBuffer(vertices)))
    return -1;
// 변환 설정 함수 호출.
if (!SetupTransform())
    return -1;
// ...
```

## 렌더링

이제 모든 설정이 완료되었으므로 3D 공간에 첫 번째 2D 삼각형을 그리기 시작할 수 있습니다.

```cpp
// ...
if (!SetupTransform())
    return -1;
// 먼저 정점 버퍼를 데이터 스트림에 바인딩해야 합니다.
HRESULT result{ };
result = _device->SetStreamSource(0,                   // 기본 스트림 사용
                                  _vertexBuffer.Get(), // 정점 버퍼 전달
                                  0,                   // 오프셋 없음
                                  sizeof(VStruct));    // 정점 구조체 크기
if (FAILED(result))
    return -1;

// 월드 변환 행렬을 생성하고 항등 행렬로 설정합니다.
D3DXMATRIX world{ };
D3DXMatrixIdentity(&world);
// x축으로 10, y축으로 10만큼 스케일링하고 z축 방향을 유지하는 스케일링 행렬을 생성합니다.
D3DXMATRIX scaling{ };
D3DXMatrixScaling(&scaling, // 스케일링할 행렬
                  10,       // x 스케일링
                  10,       // y 스케일링
                  1);       // z 스케일링
// 기본 요소의 현재 회전을 저장하는 회전 행렬을 생성합니다.
// 현재 회전 행렬을 항등 행렬로 설정합니다.
D3DXMATRIX rotation{ };
D3DXMatrixIdentity(&rotation);
// 이제 스케일링 및 회전 행렬을 곱하고 결과를
// 월드 행렬에 저장합니다.
D3DXMatrixMultiply(&world,     // 대상 행렬
                   &scaling,   // 행렬 1
                   &rotation); // 행렬 2
// 현재 월드 행렬 적용.
_device->SetTransform(D3DTS_WORLD, &world);
// 회전할 때 기본 요소의 뒷면을 볼 수 있도록 컬링을 비활성화합니다.
_device->SetRenderState(D3DRS_CULLMODE, D3DCULL_NONE);
// 기본 컬링 모드는 D3DCULL_CW입니다.
// 곱셈에 회전 행렬을 사용한 후
// 약간 회전하도록 설정할 수 있습니다.
// D3DXToRadian() 함수는 도를 라디안으로 변환합니다.
D3DXMatrixRotationY(&rotation,           // 회전할 행렬
                    D3DXToRadian(0.5f)); // 라디안 단위의 회전 각도

MSG msg{ };
    while (_running) {
    // ...
        _device->Clear(0, nullptr, D3DCLEAR_TARGET,
                       D3DXCOLOR{ 0.0f, 0.0f, 0.0f, 1.0f }, 0.0f, 0);
        // 모든 설정이 완료되었으므로 그리기 함수를 호출할 수 있습니다.
        _device->BeginScene();
        _device->DrawPrimitive(D3DPT_TRIANGLELIST, // 기본 유형
                               0,                  // 시작 정점
                               1);                 // 기본 요소 수
        _device->EndScene();

        _device->Present(nullptr, nullptr, nullptr, nullptr);
        // 월드 행렬에 회전을 추가하기 위해 월드 행렬에 회전 행렬을 계속 곱할 수 있습니다.
        D3DXMatrixMultiply(&world, &world, &rotation);
        // 수정된 월드 행렬 업데이트.
        _device->SetTransform(D3DTS_WORLD, &world);
    // ...
```

이제 20단위 떨어진 곳에서 원점을 중심으로 회전하는 10x10 단위의 색상 삼각형이 표시되어야 합니다.<br>
전체 작동 코드는 여기에서 찾을 수 있습니다: [DirectX - 1](https://pastebin.com/YkSF2rkk)

## 인덱싱

많은 정점을 공유하는 기본 요소를 더 쉽게 그릴 수 있도록 인덱싱을 사용할 수 있습니다. 이렇게 하면 고유한 정점만 선언하고 호출 순서를 다른 배열에 넣을 수 있습니다.

```cpp
// 먼저 인덱스 버퍼를 위한 새 ComPtr를 선언해야 합니다.
ComPtr<IDirect3DIndexBuffer9> _indexBuffer{ };
// ...
// std::vector에서 인덱스 버퍼를 생성하는 함수를 선언합니다.
IDirect3DIndexBuffer9* CreateIBuffer(std::vector<unsigned int>& indices) {
    IDirect3DIndexBuffer9* buffer{ };
    HRESULT result{ };
    result = _device->CreateIndexBuffer(
                 GetByteSize(indices), // 벡터 크기(바이트)
                 0,                    // 데이터 사용량
                 D3DFMT_INDEX32,       // 형식은 32비트 int
                 D3DPOOL_DEFAULT,      // 기본 풀
                 &buffer,              // 수신 버퍼
                 nullptr);             // 특수 공유 핸들
    if (FAILED(result))
        return nullptr;
    // 버퍼 데이터에 대한 데이터 포인터를 생성합니다.
    void* data{ };
    result = buffer->Lock(0,                    // 바이트 오프셋
                          GetByteSize(indices), // 바이트 크기
                          &data,                // 수신 데이터 포인터
                          0);                   // 특수 잠금 플래그
    if (FAILED(result))
        return nullptr;
    // 인덱스 데이터를 복사하고 복사 후 잠금을 해제합니다.
    memcpy(data, indices.data(), GetByteSize(indices));
    buffer->Unlock();
    // 채워진 인덱스 버퍼를 반환합니다.
    return buffer;
}
// ...
// WinMain에서 이제 정점 데이터를 변경하고 새 인덱스 데이터를 생성할 수 있습니다.
// ...
std::vector<VStruct> vertices {
    VStruct{ -1.0f, -1.0f, 1.0f, D3DXCOLOR{ 1.0f, 0.0f, 0.0f, 1.0f } },
    VStruct{ -1.0f,  1.0f, 1.0f, D3DXCOLOR{ 0.0f, 1.0f, 0.0f, 1.0f } },
    VStruct{  1.0f,  1.0f, 1.0f, D3DXCOLOR{ 0.0f, 0.0f, 1.0f, 1.0f } },
    // 오른쪽 하단에 정점 추가.
    VStruct{  1.0f, -1.0f, 1.0f, D3DXCOLOR{ 1.0f, 1.0f, 0.0f, 1.0f } }
};
// 인덱스 데이터를 선언합니다. 여기서는 두 개의 삼각형으로 사각형을 만듭니다.
std::vector<unsigned int> indices {
    0, 1, 2, // 첫 번째 삼각형 (왼쪽 하단 -> 왼쪽 상단 -> 오른쪽 상단)
    0, 2, 3  // 두 번째 삼각형 (왼쪽 하단 -> 오른쪽 상단 -> 오른쪽 하단)
};
// ...
// 이제 "CreateIBuffer" 함수를 호출하여 인덱스 버퍼를 생성합니다.
// ...
if (!(_indexBuffer = CreateIBuffer(indices)))
    return -1;
// ...
// 정점 버퍼를 바인딩한 후 인덱스 버퍼를 바인딩하여
// 인덱싱된 렌더링을 사용해야 합니다.
result = _device->SetStreamSource(0, _vertexBuffer.Get(), 0, sizeof(VStruct));
if (FAILED(result))
    return -1;
// 인덱스 데이터를 기본 데이터 스트림에 바인딩합니다.
result = _device->SetIndices(_indexBuffer.Get())
if (FAILED(result))
    return -1;
// ...
// 이제 "DrawPrimitive" 함수를 인덱싱된 버전으로 교체합니다.
_device->DrawIndexedPrimitive(D3DPT_TRIANGLELIST, // 기본 유형
                              0,                  // 기본 정점 인덱스
                              0,                  // 최소 인덱스
                              indices.size(),     // 정점 수
                              0,                  // 인덱스 버퍼 시작
                              2);                 // 기본 요소 수
// ...
```

이제 2개의 삼각형으로 구성된 색상 사각형이 표시되어야 합니다. "DrawIndexedPrimitive" 메서드의 기본 요소 수를 1로 설정하면 첫 번째 삼각형만 렌더링되고, 인덱스 버퍼의 시작을 3으로 설정하고 기본 요소 수를 1로 설정하면 두 번째 삼각형만 렌더링됩니다.<br>
전체 작동 코드는 여기에서 찾을 수 있습니다: [DirectX - 2](https://pastebin.com/yWBPWPRG)

## 정점 선언

오래된 "유연한 정점 형식"을 사용하는 대신 정점 선언을 사용해야 합니다. FVF 선언은 내부적으로 정점 선언으로 변환되기 때문입니다.

```cpp
// 먼저 다음 줄을 제거해야 합니다:
const unsigned long VertexStructFVF = D3DFVF_XYZ | D3DFVF_DIFFUSE;
// 그리고
_device->SetFVF(VertexStructFVF);
// ...
// 정점 버퍼 생성 FVF 플래그도 변경해야 합니다.
result = _device->CreateVertexBuffer(
                      GetByteSize(vertices),
                      0,
                      0,        // <- 0은 정점 선언을 사용함을 나타냅니다.
                      D3DPOOL_DEFAULT,
                      &buffer,
                      nullptr);
// 다음으로 새 ComPtr를 선언해야 합니다.
ComPtr<IDirect3DVertexDeclaration9> _vertexDecl{ };
// ...
result = _device->SetIndices(_indexBuffer.Get());
if (FAILED(result))
    return -1;
// 이제 정점 선언을 선언하고 적용해야 합니다.
// 정점 선언을 구성하는 정점 요소 벡터를 생성합니다.
std::vector<D3DVERTEXELEMENT9> vertexDeclDesc {
    { 0,                     // 스트림 인덱스
      0,                     // 구조체 시작부터의 바이트 오프셋
      D3DDECLTYPE_FLOAT3,    // 데이터 유형 (3D float 벡터)
      D3DDECLMETHOD_DEFAULT, // 테셀레이터 작업
      D3DDECLUSAGE_POSITION,  // 데이터 사용량
      0 },                   // 인덱스 (동일한 유형의 여러 사용)
    { 0,
      12,                    // 바이트 오프셋 (3 * sizeof(float) 바이트)
      D3DDECLTYPE_D3DCOLOR,
      D3DDECLMETHOD_DEFAULT,
      D3DDECLUSAGE_COLOR,
      0 },
    D3DDECL_END()            // 정점 선언의 끝을 표시합니다.
};
// 벡터를 정의한 후 이를 사용하여 정점 선언을 생성할 수 있습니다.
result = _device->CreateVertexDeclaration(
                      vertexDeclDesc.data(), // 정점 요소 배열
                      &_vertexDecl);         // 수신 포인터
if (FAILED(result))
    return -1;
// 생성된 정점 선언 적용.
_device->SetVertexDeclaration(_vertexDecl.Get());
// ...
```

## 셰이더

Direct3D 9의 최대 셰이더 모델은 셰이더 모델 3.0입니다. 모든 최신 그래픽 카드가 이를 지원해야 하지만, 기능 확인이 가장 좋습니다.

```cpp
// ...
_device->SetVertexDeclaration(_vertexDecl.Get());
// 먼저 장치 기능을 요청해야 합니다.
D3DCAPS9 deviceCaps{ };
_device->GetDeviceCaps(&deviceCaps);
// 이제 정점 셰이더에 대해 셰이더 모델 3.0이 지원되는지 확인합니다.
if (deviceCaps.VertexShaderVersion < D3DVS_VERSION(3, 0))
    return -1;
// 픽셀 셰이더도 마찬가지입니다.
if (deviceCaps.PixelShaderVersion < D3DPS_VERSION(3, 0))
    return -1;
```

이제 셰이더 모델 3.0이 지원됨을 확인했으므로 정점 및 픽셀 셰이더 파일을 생성해 보겠습니다.
DirectX 9는 C와 유사한 셰이더 언어인 HLSL(**High Level Shading Language**)을 도입하여
셰이더 프로그래밍을 훨씬 단순화했습니다. DirectX 8에서는 셰이더 어셈블리로만 셰이더를 작성할 수 있었습니다.
두 가지 기본 셰이더를 만들어 보겠습니다.

**정점 셰이더**

```glsl
// SetTransform() 메서드를 사용하여 고정 함수 파이프라인에 설정한 행렬을 나타내는 3개의 4x4 float 행렬입니다.
float4x4 projectionMatrix;
float4x4 viewMatrix;
float4x4 worldMatrix;
// 정점 셰이더에 대한 입력 구조체입니다.
// 위치를 포함하는 3D float 벡터와
// 색상을 포함하는 4D float 벡터를 가집니다.
struct VS_INPUT {
    float3 position : POSITION;
    float4 color : COLOR;
};
// 정점 셰이더의 출력 구조체로, 픽셀 셰이더로 전달됩니다.
struct VS_OUTPUT {
    float4 position : POSITION;
    float4 color : COLOR;
};
// 정점 셰이더의 main 함수는 픽셀 셰이더로 보내는 출력을 반환하고
// 입력을 매개변수로 받습니다.
VS_OUTPUT main(VS_INPUT input) {
    // 정점 셰이더가 반환하는 빈 구조체를 선언합니다.
    VS_OUTPUT output;
    // 출력 위치를 입력 위치로 설정하고
    // w-구성 요소를 1로 설정합니다. 입력 위치는 3D 벡터이고
    // 출력 위치는 4D 벡터이기 때문입니다.
    output.position = float4(input.position, 1.0f);
    // 출력 위치를 월드, 뷰 및 투영 행렬로 단계별로 곱합니다.
    output.position = mul(output.position, worldMatrix);
    output.position = mul(output.position, viewMatrix);
    output.position = mul(output.position, projectionMatrix);
	// 입력 색상을 변경하지 않고 픽셀 셰이더로 전달합니다.
    output.color = input.color;
    // 출력 구조체를 픽셀 셰이더로 반환합니다.
    // 위치 값은 자동으로 정점 위치로 사용됩니다.
    return output;
}
```

**픽셀 셰이더**

```glsl
// 픽셀 셰이더 입력 구조체는 정점 셰이더 출력과 동일해야 합니다!
struct PS_INPUT {
    float4 position : POSITION;
    float4 color : COLOR;
};
// 픽셀 셰이더는 정점 색상을 나타내는 4D 벡터를 간단히 반환합니다.
// 정점 셰이더와 마찬가지로 입력을 매개변수로 받습니다.
// 올바르게 해석되도록 출력 시맨틱을 color로 선언해야 합니다.
float4 main(PS_INPUT input) : COLOR {
    return input.color;
}
```

시맨틱에 대한 자세한 내용은 다음을 참조하십시오: [DirectX - Semantics](https://docs.microsoft.com/en-us/windows/win32/direct3dhlsl/dx-graphics-hlsl-semantics#vertex-shader-semantics)

이제 코드에 상당한 변경을 가해야 합니다.

```cpp
ComPtr<IDirect3DDevice9> _device{ };
ComPtr<IDirect3DVertexBuffer9> _vertexBuffer{ };
ComPtr<IDirect3DIndexBuffer9> _indexBuffer{ };
ComPtr<IDirect3DVertexDeclaration9> _vertexDecl{ };
// 정점 및 픽셀 셰이더를 위한 ComPtr와
// 정점 셰이더의 상수(행렬)를 위한 ComPtr를 추가해야 합니다.
ComPtr<IDirect3DVertexShader9> _vertexShader{ };
ComPtr<IDirect3DPixelShader9> _pixelShader{ };
ComPtr<ID3DXConstantTable> _vertexTable{ };
// WinMain 및 SetupTransform에서 사용하므로 월드 및 회전 행렬을 전역으로 선언합니다.
D3DXMATRIX _worldMatrix{ };
D3DXMATRIX _rotationMatrix{ };
// ...
bool SetupTransform() {
    // 월드 및 회전 행렬을 항등 행렬로 설정합니다.
    D3DXMatrixIdentity(&_worldMatrix);
    D3DXMatrixIdentity(&_rotationMatrix);

    D3DXMATRIX scaling{ };
    D3DXMatrixScaling(&scaling, 10, 10, 1);
    D3DXMatrixMultiply(&_worldMatrix, &scaling, &_rotationMatrix);
    // 스케일링 및 회전 행렬을 곱한 후 정점 셰이더의 상수 테이블에서 메서드를 사용하여 셰이더에 전달해야 합니다.
    HRESULT result{ };
    result = _vertexTable->SetMatrix(
                         _device.Get(),   // direct3d 장치
                         "worldMatrix",   // 셰이더의 행렬 이름
                          &_worldMatrix); // 행렬에 대한 포인터
    if (FAILED(result))
        return false;

    D3DXMATRIX view{ };
    D3DXMatrixLookAtLH(&view, &D3DXVECTOR3{ 0.0f, 0.0f, -20.0f },
           &D3DXVECTOR3{ 0.0f, 0.0f, 0.0f }, &D3DXVECTOR3{ 0.0f, 1.0f, 0.0f });
    // 뷰 행렬도 마찬가지입니다.
    result = _vertexTable->SetMatrix(
	                       _device.Get(), // direct 3d 장치
	                       "viewMatrix",  // 행렬 이름
	                       &view);        // 행렬
    if (FAILED(result))
        return false;

    D3DXMATRIX projection{ };
    D3DXMatrixPerspectiveFovLH(&projection, D3DXToRadian(60.0f),
        1024.0f / 768.0f, 0.0f, 100.0f);
    // 투영 행렬도 마찬가지입니다.
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
// 정점 및 인덱스 버퍼 생성 및 초기화는 변경되지 않습니다.
// ...
// 셰이더 모델 3.0이 사용 가능한지 확인한 후 셰이더를 컴파일하고 생성해야 합니다.
// 컴파일된 셰이더 코드를 저장할 두 개의 임시 버퍼를 선언합니다.
ID3DXBuffer* vertexShaderBuffer{ };
ID3DXBuffer* pixelShaderBuffer{ };
result = D3DXCompileShaderFromFile("vertex.glsl",  // 셰이더 이름
                                   nullptr,        // 매크로 정의
                                   nullptr,        // 특수 포함
                                   "main",         // 진입점 이름
                                   "vs_3_0",       // 셰이더 모델 버전
                                   0,              // 특수 플래그
                                   &vertexShaderBuffer, // 코드 버퍼
                                   nullptr,        // 오류 메시지
                                   &_vertexTable); // 상수 테이블
if (FAILED(result))
    return -1;
// 정점 셰이더 컴파일 후 픽셀 셰이더 컴파일.
result = D3DXCompileShaderFromFile("pixel.glsl",
                                   nullptr,
                                   nullptr,
                                   "main",
                                   "ps_3_0", // 픽셀 셰이더 모델 3.0
                                   0,
                                   &pixelShaderBuffer,
                                   nullptr,
                                   nullptr); // 상수 테이블 필요 없음
if (FAILED(result))
    return -1;
// 코드 버퍼에서 정점 셰이더를 생성합니다.
result = _device->CreateVertexShader(
             (DWORD*)vertexShaderBuffer->GetBufferPointer(), // 코드 버퍼
             &_vertexShader); // 정점 셰이더 포인터
if (FAILED(result))
    return -1;

result = _device->CreatePixelShader(
             (DWORD*)pixelShaderBuffer->GetBufferPointer(),
             &_pixelShader);
if (FAILED(result))
    return -1;
// 셰이더가 생성된 후 임시 코드 버퍼를 해제합니다.
vertexShaderBuffer->Release();
pixelShaderBuffer->Release();
// 정점 및 픽셀 셰이더 적용.
_device->SetVertexShader(_vertexShader.Get());
_device->SetPixelShader(_pixelShader.Get());
// 셰이더가 설정된 후 변환을 적용합니다.
if (!SetupTransform())
    return -1;
// 조명 렌더링 상태를 설정하는 호출을 제거할 수도 있습니다.
_device->SetRenderState(D3DRS_LIGHTING, false);
```

전체 코드는 여기에서 찾을 수 있습니다: [DirectX - 3](https://pastebin.com/y4NrvawY)

## 텍스처링

```cpp
// 먼저 텍스처를 위한 ComPtr를 선언해야 합니다.
ComPtr<IDirect3DTexture9> _texture{ };
// 그런 다음 정점 구조체를 변경해야 합니다.
struct VStruct {
    float x, y, z;
    float u, v;      // 텍스처 u 및 v 좌표 추가
    D3DCOLOR color;
};
// 정점 선언에서 텍스처 좌표를 추가해야 합니다.
// 텍스처의 왼쪽 상단은 u: 0, v: 0입니다.
std::vector<VStruct> vertices {
    VStruct{ -1.0f, -1.0f, 1.0f, 0.0f, 1.0f, ... }, // 왼쪽 하단
    VStruct{ -1.0f,  1.0f, 1.0f, 0.0f, 0.0f, ... }, // 왼쪽 상단
    VStruct{  1.0f,  1.0f, 1.0f, 1.0f, 0.0f, ... }, // 오른쪽 상단
    VStruct{  1.0f, -1.0f, 1.0f, 1.0f, 1.0f, ... }  // 오른쪽 하단
};
// 다음은 정점 선언입니다.
std::vector<D3DVERTEXELEMENT9> vertexDecl{
    {0, 0, D3DDECLTYPE_FLOAT3, D3DDECLMETHOD_DEFAULT, D3DDECLUSAGE_POSITION, 0},
    // 텍스처 좌표에 사용되는 2D float 벡터를 추가합니다.
    {0, 12, D3DDECLTYPE_FLOAT2, D3DDECLMETHOD_DEFAULT, D3DDECLUSAGE_TEXCOORD, 0},
    // 색상 오프셋은 (3 + 2) * sizeof(float) = 20바이트가 아닙니다.
    {0, 20, D3DDECLTYPE_D3DCOLOR, D3DDECLMETHOD_DEFAULT, D3DDECLUSAGE_COLOR, 0},
    D3DDECL_END()
};
// 이제 텍스처를 로드하고 셰이더에 전달해야 합니다.
// ...
_device->SetRenderState(D3DRS_CULLMODE, D3DCULL_NONE);
// png 파일에서 Direct3D 텍스처를 생성합니다.
result = D3DXCreateTextureFromFile(_device.Get(), // direct3d 장치
                                   "texture.png", // 텍스처 경로
                                   &_texture);    // 수신 텍스처 포인터
if (FAILED(result))
    return -1;
// 텍스처를 셰이더 스테이지 0에 연결합니다. 이는 픽셀 셰이더의 텍스처 레지스터 0과 동일합니다.
_device->SetTexture(0, _texture.Get());
```

주요 코드가 준비되었으므로 이제 셰이더를 이러한 변경 사항에 맞게 조정해야 합니다.<br>

**정점 셰이더**

```glsl
float4x4 projectionMatrix;
float4x4 viewMatrix;
float4x4 worldMatrix;
// 정점 셰이더 입력 및 출력에 텍스처 좌표를 추가합니다.
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
    // texcoord 출력을 입력으로 설정합니다.
    output.texcoord = input.texcoord;

    return output;
}
```

**픽셀 셰이더**

```glsl
// "sam0"라는 샘플러를 샘플러 레지스터 0을 사용하여 생성합니다. 이는 텍스처 스테이지 0과 동일하며, 텍스처를 전달했습니다.
uniform sampler2D tex;

struct PS_INPUT {
    float4 position : POSITION;
    float2 texcoord : TEXCOORD;
    float4 color : COLOR;
};

float4 main(PS_INPUT input) : COLOR{
    // 텍스처 색상과 입력 색상 사이를 선형 보간하고
    // 입력 색상의 75%를 사용합니다.
    // tex2D는 지정된 텍스처 좌표에서 텍스처 데이터를 로드합니다.
    return lerp(tex2D(sam0, input.texcoord), input.color, 0.75f);
}
```

## 행렬 변환

**정점 셰이더**

```glsl
#version 330 core

layout(location = 0) in vec3 position;
layout(location = 1) in vec3 color;
layout(location = 2) in vec2 texCoords;
// 투영 행렬과 모델 행렬을 위한 2개의 4x4 행렬을 생성합니다.
// 정적 장면에서 그리므로 뷰 행렬은 필요하지 않습니다.
uniform mat4 projection;
uniform mat4 model;

out vec3 fColor;
out vec2 fTexCoords;

void main() {
    fColor = color;
    fTexCoords = texCoords;
    // 위치를 모델 행렬로 곱한 다음 투영 행렬로 곱합니다.
    // 행렬 곱셈 순서에 주의하십시오!
    gl_Position = projection * model * vec4(position, 1.0);
}
```

소스에서 이제 정점 데이터를 변경하고 모델 및 투영 행렬을 생성해야 합니다.

```cpp
// 새 정점 데이터, 반시계 방향 선언.
std::vector<float> vertexData {
    0.0f, 1.0f, 0.0f,   // 왼쪽 상단
    0.0f, 0.0f, 0.0f,   // 왼쪽 하단
    1.0f, 0.0f, 0.0f,   // 오른쪽 하단
    1.0f, 1.0f, 0.0f    // 오른쪽 상단
};
// 행렬의 위치를 요청합니다.
GLint projectionLocation = glGetUniformLocation(program, "projection");
GLint modelLocation = glGetUniformLocation(program, "model");
// 행렬 선언.
// 1024x768 창에 대한 직교 행렬.
std::vector<float> projection {
    0.001953f,       0.0f,  0.0f, 0.0f,
         0.0f, -0.002604f,  0.0f, 0.0f,
         0.0f,       0.0f, -1.0f, 0.0f,
        -1.0f,       1.0f,  0.0f, 1.0f
};
// x 50, y 50으로 변환하고 x 200, y 200으로 스케일링하는 모델 행렬.
std::vector<float> model {
    200.0f,   0.0f, 0.0f, 0.0f,
      0.0f, 200.0f, 0.0f, 0.0f,
      0.0f,   0.0f, 1.0f, 0.0f,
     50.0f,  50.0f, 0.0f, 1.0f
};
// 이제 계산된 행렬을 프로그램으로 보낼 수 있습니다.
glUseProgram(program);
glUniformMatrix4fv(projectionLocation,   // 위치
                   1,                    // 개수
                   GL_FALSE,             // 행렬 전치
                   projection.data());   // 데이터
glUniformMatrix4fv(modelLocation, 1, GL_FALSE, model.data());
glUseProgram(0);
// glUniform*() 호출은 프로그램이 바인딩되어 있는 동안에만 수행되어야 합니다.
```

애플리케이션은 이제 정의된 위치와 크기로 텍스처를 표시해야 합니다.<br>
현재 코드는 여기에서 찾을 수 있습니다: [OpenGL - 4](https://pastebin.com/9ahpFLkY)

```cpp
// 행렬과 벡터를 생성하는 OpenGL용 수학 라이브러리가 많이 있습니다.
// C++에서 가장 많이 사용되는 것은 glm(OpenGL Mathematics)입니다.
// 헤더 전용 라이브러리입니다.
// glm을 사용한 동일한 코드는 다음과 같습니다:
glm::mat4 projection{ glm::ortho(0.0f, 1024.0f, 768.0f, 0.0f) };
glUniformMatrix4fv(projectionLocation, 1, GL_FALSE,
                   glm::value_ptr(projection));
// 모델 행렬을 항등 행렬로 초기화합니다. 그렇지 않으면 모든 곱셈이 0이 됩니다.
glm::mat4 model{ 1.0f };
model = glm::translate(model, glm::vec3{ 50.0f, 50.0f, 0.0f });
model = glm::scale(model, glm::vec3{ 200.0f, 200.0f, 0.0f });
glUniformMatrix4fv(modelLocation, 1, GL_FALSE,
                   glm::value_ptr(model));
```

## 지오메트리 셰이더

지오메트리 셰이더는 OpenGL 3.2에서 도입되었으며, 래스터라이저로 전송되는 정점을 생성할 수 있습니다. 또한 기본 유형을 변경할 수 있습니다. 예를 들어, 점을 입력으로 받아 다른 기본 요소를 출력할 수 있습니다.
지오메트리 셰이더는 정점 셰이더와 프래그먼트 셰이더 사이에 있습니다.

**정점 셰이더**

```glsl
#version 330 core

layout(location = 0) in vec3 position;
layout(location = 1) in vec3 color;
// 다음 셰이더 단계로 전달되는 출력 인터페이스 블록을 생성합니다.
// 인터페이스 블록은 셰이더 간에 전달되는 데이터를 구조화하는 데 사용할 수 있습니다.
out VS_OUT {
    vec3 color;
} vs_out;

void main() {
    vs_out.color = color
    gl_Position = vec4(position, 1.0);
}
```

**지오메트리 셰이더**

```glsl
#version 330 core
// 지오메트리 셰이더는 점을 입력으로 받습니다.
layout(points) in;
// 내보낸 3개의 정점마다 삼각형을 출력합니다.
layout(triangle_strip, max_vertices = 3) out;
// VS_OUT은 지오메트리 셰이더의 입력 변수가 됩니다.
// 지오메트리 셰이더에 대한 모든 입력은 배열로 처리됩니다.
in VS_OUT {
    vec3 color;
} gs_in[];
// 프래그먼트 셰이더의 출력 색상.
// 인터페이스 블록을 사용하지 않으려면 단순히 color를 'out vec3 color'로 정의할 수도 있습니다.
out GS_OUT {
    vec3 color;
} gs_out;

void main() {
    // 각 emit은 프래그먼트 셰이더를 호출하므로 각 정점에 대한 색상을 설정합니다.
    gs_out.color = mix(gs_in[0].color, vec3(1.0, 0.0, 0.0), 0.5);
    // 0.5 단위 왼쪽으로 이동하고 새 정점을 내보냅니다.
    // gl_in[]은 정점 셰이더의 현재 정점이며, 여기서는 점을 받으므로 0만 사용합니다.
    gl_Position = gl_in[0].gl_Position + vec4(-0.5, 0.0, 0.0, 0.0);
    EmitVertex();
    gs_out.color = mix(gs_in[0].color, vec3(0.0, 1.0, 0.0), 0.5);
    // 0.5 단위 오른쪽으로 이동하고 새 정점을 내보냅니다.
    gl_Position = gl_in[0].gl_Position + vec4(0.5, 0.0, 0.0, 0.0);
    EmitVertex();
    gs_out.color = mix(gs_in[0].color, vec3(0.0, 0.0, 1.0), 0.5);
    // 0.5 단위 위로 이동하고 새 정점을 내보냅니다.
    gl_Position = gl_in[0].gl_Position + vec4(0.0, 0.75, 0.0, 0.0);
    EmitVertex();
    EndPrimitive();
}
```

**프래그먼트 셰이더**

```glsl
in GS_OUT {
    vec3 color;
} fs_in;

out vec4 outColor;

void main() {
    outColor = vec4(fs_in.color, 1.0);
}
```

이제 VBO에 단일 색상으로 단일 점을 저장하고 그리면, 각 정점에서 빨강, 초록, 파랑이 절반씩 섞인 삼각형이 표시되어야 합니다.


## 인용
<sup>[1]</sup>[OpenGL - 위키백과](https://en.wikipedia.org/wiki/OpenGL)

## 도서

- OpenGL Superbible - 5판 (OpenGL 3.3 포함)
- OpenGL Programming Guide - 8판 (OpenGL 4.3 포함)