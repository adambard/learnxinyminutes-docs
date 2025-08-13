---
category: framework
name: raylib
filename: learnraylib.c
contributors:
    - ["Nikolas Wipper", "https://notnik.cc"]
translators:
    - ["Taeyoon Kim", "https://github.com/partrita"]
---

**raylib**은 OpenGL 1.1, 2.1, 3.3 및 OpenGL ES 2.0을 기반으로 구축된 크로스 플랫폼 사용하기 쉬운 그래픽 라이브러리입니다. C로 작성되었지만 50개 이상의 다른 언어에 바인딩되어 있습니다. 이 튜토리얼에서는 C, 특히 C99를 사용합니다.

```c
#include <raylib.h>

int main(void)
{
    const int screenWidth = 800;
    const int screenHeight = 450;

    // raylib을 초기화하기 전에 구성 플래그를 설정할 수 있습니다.
    SetConfigFlags(FLAG_MSAA_4X_HINT | FLAG_VSYNC_HINT);

    // raylib은 인스턴스 구조체를 저장할 필요가 없습니다.
    // 현재 raylib은 한 번에 하나의 창만 처리할 수 있습니다.
    InitWindow(screenWidth, screenHeight, "MyWindow");

    // 게임을 초당 60프레임으로 실행하도록 설정합니다.
    SetTargetFPS(60);

    // 창을 닫는 키를 설정합니다.
    // 키가 없으면 0이 될 수 있습니다.
    SetExitKey(KEY_DELETE);

    // raylib은 Camera3D와 Camera2D의 두 가지 유형의 카메라를 정의합니다.
    // Camera는 Camera3D의 typedef입니다.
    Camera camera = {
            .position   = {0.0f, 0.0f, 0.0f},
            .target     = {0.0f, 0.0f, 1.0f},
            .up         = {0.0f, 1.0f, 0.0f},
            .fovy       = 70.0f,
            .projection = CAMERA_PERSPECTIVE
    };

    // raylib은 다양한 파일 형식에서 모델, 애니메이션, 이미지 및 사운드를 로드하는 것을 지원합니다.
    Model myModel = LoadModel("my_model.obj");
    Font someFont = LoadFont("some_font.ttf");

    // 100x100 렌더 텍스처 생성
    RenderTexture renderTexture = LoadRenderTexture(100, 100);

    // WindowShouldClose는 사용자가 창을 닫고 있는지 확인합니다.
    // 이는 바로 가기, 창 컨트롤 또는 이전에 설정한 키를 사용하여 발생할 수 있습니다.
    while (!WindowShouldClose())
    {

        // 모든 그리기 호출 전에 BeginDrawing을 호출해야 합니다.
        BeginDrawing();
        {

            // 배경을 특정 색상으로 지웁니다.
            ClearBackground(BLACK);

            if (IsKeyDown(KEY_SPACE))
                DrawCircle(400, 400, 30, GREEN);

            // 간단한 텍스트 그리기
            DrawText("Congrats! You created your first window!",
                     190, // x
                     200, // y
                     20,  // 글꼴 크기
                     LIGHTGRAY
            );

            // 대부분의 함수에는 여러 버전이 있습니다.
            // 이들은 일반적으로 Ex, Pro, V
            // 또는 때로는 Rec, Wires (3D만), Lines (2D만) 접미사가 붙습니다.
            DrawTextEx(someFont,
                       "Text in another font",
                       (Vector2) {10, 10},
                       20, // 글꼴 크기
                       2,  // 간격
                       LIGHTGRAY);

            // 3D 그리기에 필요하며 2D와 동일합니다.
            BeginMode3D(camera);
            {

                DrawCube((Vector3) {0.0f, 0.0f, 3.0f},
                         1.0f, 1.0f, 1.0f, RED);

                // 그릴 때 흰색 색조는 원래 색상을 유지합니다.
                DrawModel(myModel, (Vector3) {0.0f, 0.0f, 3.0f},
                          1.0f, // 스케일
                          WHITE);

            }
            // 3D 모드를 종료하여 다시 정상적으로 그릴 수 있습니다.
            EndMode3D();

            // 렌더 텍스처에 그리기 시작
            BeginTextureMode(renderTexture);
            {

                // `BeginDrawing()`을 호출한 것과 동일하게 작동합니다.

                ClearBackground(RAYWHITE);

                BeginMode3D(camera);
                {

                    DrawGrid(10, // 슬라이스
                             1.0f // 간격
                    );

                }
                EndMode3D();

            }
            EndTextureMode();

            // 렌더 텍스처에는 Texture2D 필드가 있습니다.
            DrawTexture(renderTexture.texture, 40, 378, BLUE);

        }
        EndDrawing();
    }

    // 로드된 객체 언로드
    UnloadFont(someFont);
    UnloadModel(myModel);

    // 창 및 OpenGL 컨텍스트 닫기
    CloseWindow();

    return 0;
}
```

## 더 읽을거리
raylib에는 [훌륭한 예제](https://www.raylib.com/examples.html)가 있습니다.
C를 좋아하지 않는다면 [raylib 바인딩](https://github.com/raysan5/raylib/blob/master/BINDINGS.md)을 확인하십시오.