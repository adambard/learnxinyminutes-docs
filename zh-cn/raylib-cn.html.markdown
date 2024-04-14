---
category: framework
framework: raylib
lang: zh-cn
filename: learnraylib-cn.c
contributors:
    - ["Nikolas Wipper", "https://notnik.cc"]
translators:
    - ["lzw-723", "https://github.com/lzw-723"]
---

**raylib** 是一个跨平台、易用的图形库，围绕OpenGL 1.1、2.1、3.3和OpenGL ES 2.0构建。
虽然它是用C语言编写的，却有超过50种不同语言的绑定。本教程将使用C语言。
更确切地说，是C99。

```c
#include <raylib.h>

int main(void)
{
    const int screenWidth = 800;
    const int screenHeight = 450;

    // 在初始化raylib之前，可以设置标志位
    SetConfigFlags(FLAG_MSAA_4X_HINT | FLAG_VSYNC_HINT);

    // raylib并不要求我们存储任何实例结构
    // 目前raylib一次只能处理一个窗口
    InitWindow(screenWidth, screenHeight, "MyWindow");

    // 设置我们的游戏以每秒60帧的速度运行
    SetTargetFPS(60);

    // 设置一个关闭窗口的键。
    //可以是0，表示没有键
    SetExitKey(KEY_DELETE);

    // raylib定义了两种类型的相机。Camera3D和Camera2D
    // Camera是Camera3D的一个类型化定义
    Camera camera = {
            .position = {0.0f, 0.0f, 0.0f},
            .target   = {0.0f, 0.0f, 1.0f},
            .up       = {0.0f, 1.0f, 0.0f},
            .fovy     = 70.0f,
            .type     = CAMERA_PERSPECTIVE
    };


    // raylib支持加载各种不同的文件格式的模型、动画、图像和声音。
    Model myModel = LoadModel("my_model.obj");
    Font someFont = LoadFont("some_font.ttf");

    // 创建一个100x100的渲染纹理
    RenderTexture renderTexture = LoadRenderTexture(100, 100);

    // WindowShouldClose方法检查用户是否正在关闭窗口。
    // 可能用的是快捷方式、窗口控制或之前设置的关闭窗口键
    while (!WindowShouldClose())
    {

        // BeginDrawing方法要在任何绘图操作之前被调用。
        BeginDrawing();
        {

            // 为背景设定某种颜色
            ClearBackground(BLACK);

            if (IsKeyDown(KEY_SPACE))
                DrawCircle(400, 400, 30, GREEN);

            // 简单地绘制文本
            DrawText("Congrats! You created your first window!",
                     190, // x
                     200, // y
                     20,  // 字体大小
                     LIGHTGRAY
            );

            // 大多数函数都有几个版本
            // 通常后缀为Ex, Pro, V
            // 或者是Rec、Wires（仅适用于3D）、Lines（仅适用于2D）。
            DrawTextEx(someFont,
                       "Text in another font",
                       (Vector2) {10, 10},
                       20, // 字体大小
                       2,  // 间距
                       LIGHTGRAY);

            // 绘制3D时需要，有2D的等价方法
            BeginMode3D(camera);
            {

                DrawCube((Vector3) {0.0f, 0.0f, 3.0f},
                         1.0f, 1.0f, 1.0f, RED);

                // 绘图时的白色色调将保持原来的颜色
                DrawModel(myModel, (Vector3) {0.0f, 0.0f, 3.0f},
                          1.0f, // 缩放
                          WHITE);

            }
            // 结束3D模式，这样就可以再次普通绘图
            EndMode3D();

            // 开始在渲染纹理上绘图
            BeginTextureMode(renderTexture);
            {

                // 它的行为与刚才调用的`BeginDrawing()`方法相同

                ClearBackground(RAYWHITE);

                BeginMode3D(camera);
                {

                    DrawGrid(10, // Slices
                             1.0f // 间距
                    );

                }
                EndMode3D();

            }
            EndTextureMode();

            // 渲染有Texture2D字段的纹理
            DrawTexture(renderTexture.texture, 40, 378, BLUE);

        }
        EndDrawing();
    }

    // 卸载已载入的对象
    UnloadFont(someFont);
    UnloadModel(myModel);

    // 关闭窗口和OpenGL上下文
    CloseWindow();

    return 0;
}
```

## 延伸阅读
raylib有一些[不错的例子](https://www.raylib.com/examples.html)
如果你不喜欢C语言你也可以看看[raylib的其他语言绑定](https://github.com/raysan5/raylib/blob/master/BINDINGS.md)
