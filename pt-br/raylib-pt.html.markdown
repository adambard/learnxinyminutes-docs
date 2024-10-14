---
category: framework
framework: raylib
filename: learnraylib-pt.c
contributors:
    - ["Nikolas Wipper", "https://notnik.cc"]
lang: pt-br
translators:
    - ["Luiz Bills", "https://github.com/luizbills"]
---

**raylib** é uma biblioteca gráfica multiplataforma fácil de usar, construída em torno do OpenGL 1.1, 2.1, 3.3 e OpenGL ES 2.0. Embora seja escrito em C, possui *bindings* em mais de 50 linguages diferentes. Este tutorial usará C, mais especificamente C99.

```c
#include <raylib.h>

int main(void)
{
    const int larguraJanela = 800;
    const int alturaJanela = 450;

    // Antes de iniciar a raylib nós podemos definir algumas configurações
    SetConfigFlags(FLAG_MSAA_4X_HINT | FLAG_VSYNC_HINT);

    // raylib não necessita que salvamos nenhuma instância
    // até o momento raylib só pode ter controlar uma janela por vez
    InitWindow(larguraJanela, alturaJanela, "MinhaJanela");

    // Faz nosso jogo rodar a 60 quadros por segundo (60 FPS)
    SetTargetFPS(60);

    // Define uma tecla para fechar a janela do jogo
    // Use 0 para nenhuma tecla
    SetExitKey(KEY_DELETE);

    // raylib tem dois tipos de cameras: Camera3D e Camera2D
    // Camera é um typedef da Camera3D
    Camera camera = {
            .position   = {0.0f, 0.0f, 0.0f},
            .target     = {0.0f, 0.0f, 1.0f},
            .up         = {0.0f, 1.0f, 0.0f},
            .fovy       = 70.0f,
            .projection = CAMERA_PERSPECTIVE
    };

    // raylib suporta carregar modelos, animações, imagens and sons
    // de vários formatos de arquivos diferentes
    Model myModel = LoadModel("my_model.obj");
    Font someFont = LoadFont("some_font.ttf");

    // WindowShouldClose() checa se estão tentando fechar a janela
    while (!WindowShouldClose())
    {

        // BeginDrawing() precisa se chamado antes de renderizar
        // qualquer coisa na tela
        BeginDrawing();
        {

            // Define uma cor de fundo
            ClearBackground(BLACK);

            // checa se uma tecla está sendo pressionada
            if (IsKeyDown(KEY_SPACE)) {
                // Mostra um circulo
                DrawCircle(
                    400, // posição x
                    400, // posição y
                    30,  // raio
                    GREEN // cor verde
                );

                // Para muitas funções haverá diversas versões
                // Que terão os sufixos Ex, Pro, V, Lines (apenas 2D),
                // or algumas vezes Rec, Wires (apenas 3D)
                DrawCircleLines(
                    200, // posição x
                    200, // posição y
                    35,  // raio
                    LIGHTGRAY // cor cinza claro
                );
            }

            // Mostra um texto
            DrawText("Parabéns! Você criou sua primeira janela!",
                     190, // posição x
                     200, // posição y
                     20,  // tamanho da fonte
                     LIGHTGRAY // cor
            );

            // Obrigatório para gráficos 3D, também tem 2D
            BeginMode3D(camera);
            {
                // desenha um cubo
                DrawCube(
                    (Vector3) {0.0f, 0.0f, 5.0f},
                    1.0f,
                    1.0f,
                    1.0f,
                    RED
                );
            }
            // Encerra o modo 3D
            EndMode3D();
        }
        // EndDrawing() precisa se chamado depos de renderizar tudo
        EndDrawing();
    }

    // Descarrega (limpa da memória) objetos carregados
    UnloadFont(someFont);
    UnloadModel(myModel);

    // Fecha a janela e encerra o contexto OpenGL
    CloseWindow();

    return 0;
}
```

## Continue lendo

No site oficial da raylib tem [ótimos exemplos](https://www.raylib.com/examples.html).

Se você não gosta de linguagem C você pode usar com outras linguagens usando [bindings](https://github.com/raysan5/raylib/blob/master/BINDINGS.md).
