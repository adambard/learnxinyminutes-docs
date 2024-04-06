---
category: tool
tool: raylib
filename: learnraylib.c
contributors:
    - ["Nikolas Wipper", "https://notnik.cc"]
---

**raylib** is a cross-platform easy-to-use graphics library, built around
OpenGL 1.1, 2.1, 3.3 and OpenGL ES 2.0. Even though it is written in C
it has bindings to over 50 different languages. This tutorial will use C,
more specifically C99.

```c
#include <raylib.h>

int main(void)
{
    const int screenWidth = 800;
    const int screenHeight = 450;

    // Before initialising raylib we can set configuration flags
    SetConfigFlags(FLAG_MSAA_4X_HINT | FLAG_VSYNC_HINT);

    // raylib doesn't require us to store any instance structures
    // At the moment raylib can handle only one window at a time
    InitWindow(screenWidth, screenHeight, "MyWindow");

    // Set our game to run at 60 frames-per-second
    SetTargetFPS(60);

    // Set a key that closes the window
    // Could be 0 for no key
    SetExitKey(KEY_DELETE);

    // raylib defines two types of cameras: Camera3D and Camera2D
    // Camera is a typedef for Camera3D
    Camera camera = {
            .position = {0.0f, 0.0f, 0.0f},
            .target   = {0.0f, 0.0f, 1.0f},
            .up       = {0.0f, 1.0f, 0.0f},
            .fovy     = 70.0f,
            .type     = CAMERA_PERSPECTIVE
    };

    // raylib supports loading of models, animations, images and sounds
    // from various different file formats
    Model myModel = LoadModel("my_model.obj");
    Font someFont = LoadFont("some_font.ttf");

    // Creates a 100x100 render texture
    RenderTexture renderTexture = LoadRenderTexture(100, 100);

    // WindowShouldClose checks if the user is closing the window
    // This might happen using a shortcut, window controls
    // or the key we set earlier
    while (!WindowShouldClose())
    {

        // BeginDrawing needs to be called before any draw call
        BeginDrawing();
        {

            // Sets the background to a certain color
            ClearBackground(BLACK);

            if (IsKeyDown(KEY_SPACE))
                DrawCircle(400, 400, 30, GREEN);

            // Simple draw text
            DrawText("Congrats! You created your first window!",
                     190, // x
                     200, // y
                     20,  // font size
                     LIGHTGRAY
            );

            // For most functions there are several versions
            // These are usually postfixed with Ex, Pro, V
            // or sometimes Rec, Wires (only for 3D), Lines (only for 2D)
            DrawTextEx(someFont,
                       "Text in another font",
                       (Vector2) {10, 10},
                       20, // font size
                       2,  // spacing
                       LIGHTGRAY);

            // Required for drawing 3D, has 2D equivalent
            BeginMode3D(camera);
            {

                DrawCube((Vector3) {0.0f, 0.0f, 3.0f},
                         1.0f, 1.0f, 1.0f, RED);

                // White tint when drawing will keep the original color
                DrawModel(myModel, (Vector3) {0.0f, 0.0f, 3.0f},
                          1.0f, //Scale
                          WHITE);

            }
            // End 3D mode so we can draw normally again
            EndMode3D();

            // Start drawing onto render texture
            BeginTextureMode(renderTexture);
            {

                // It behaves the same as if we just called `BeginDrawing()`

                ClearBackground(RAYWHITE);

                BeginMode3D(camera);
                {

                    DrawGrid(10, // Slices
                             1.0f // Spacing
                    );

                }
                EndMode3D();

            }
            EndTextureMode();

            // render textures have a Texture2D field
            DrawTexture(renderTexture.texture, 40, 378, BLUE);

        }
        EndDrawing();
    }

    // Unloading loaded objects
    UnloadFont(someFont);
    UnloadModel(myModel);

    // Close window and OpenGL context
    CloseWindow();

    return 0;
}
```

## Further reading
raylib has some [great examples](https://www.raylib.com/examples.html)
If you don't like C check out the [raylib bindings](https://github.com/raysan5/raylib/blob/master/BINDINGS.md)