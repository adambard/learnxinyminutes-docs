---
category: tool
tool: composer
contributors:
    - ["Brett Taylor", "https://github.com/glutnix"]
translators:
    - ["David Lima", "https://github.com/davelima"]
lang: pt-br
filename: LearnComposer-pt.sh
---

[Composer](https://getcomposer.org/) é uma ferramenta de gerenciamento de dependências para PHP. Ele permite que você defina as bibliotecas que seu projeto precisa, e então ele as gerencia (instala/atualiza) para você.

# Instalando

```sh
# Instala o binário composer.phar no diretório atual
curl -sS https://getcomposer.org/installer | php
# Se você fizer desta forma, você precisará chamar o composer assim:
php composer.phar about

# Instala o binário em ~/bin/composer
# Nota: certifique-se de que ~/bin está na variável de ambiente PATH do seu shell
curl -sS https://getcomposer.org/installer | php -- --install-dir=~/bin --filename=composer
```

Usuários Windows devem seguir as Instruções de instalação para Windows:
https://getcomposer.org/doc/00-intro.md#installation-windows (EN)

## Confirmando a instalação

```sh
# Verifica a versão e lista as opções
composer

# Para obter ajuda com os comandos
composer help require

# Verifica se o Composer tem as permissões necessárias e se está atualizado
composer diagnose
composer diag # atalho

# Atualiza o binário do Composer para a última versão
composer self-update
composer self # atalho
```

# Modo de uso

O Composer armazena as dependências do seu projeto em `composer.json`.
Você pode editar este arquivo, mas é recomendável deixar que o Composer faça isso.

```sh
# Cria um novo projeto na pasta atual
composer init
# Executa um questionário interativo, te pedindo detalhes sobre o projeto.
# Você pode deixar o questionário em branco, desde que não haja outros projetos dependendo deste.

# Se um arquivo composer.json já existir, baixa as dependências
composer install

# Para baixar apenas as dependências de produção, excluindo as de desenvolvimento
composer install --no-dev

# Adiciona uma dependência de produção ao projeto
composer require guzzlehttp/guzzle
# O Composer se encarrega de verificar qual é a última versão de
# guzzlehttp/guzzle, baixar e adicionar a nova dependência no
# campo 'require' do composer.json

composer require guzzlehttp/guzzle:6.0.*
# O composer baixa a última versão que combine com o padrão informado (6.0.2, por exemplo)
# e adiciona essa dependência ao campo 'require' do arquivo composer.json

composer require --dev phpunit/phpunit:~4.5.0
# O composer irá baixar a dependencia como desenvolvimento,
# usando a versão mais recente >= 4.5.0 e < 4.6.0

composer require-dev phpunit/phpunit:^4.5.0
# O composer irá baixar a dependencia como desenvolvimento,
# usando a versão mais recente >= 4.5.0 e < 5.0

# Para mais informações sobre os padrões de versões, veja a
# Documentação sobre Versões do Composer: https://getcomposer.org/doc/articles/versions.md (EN)

# Para ver quais pacotes estão disopníveis e quais estão instalados
composer show

# Para ver quais pacotes estão instalados
composer show --installed

# Para encontrar um pacote que tenha 'mailgun' no nome ou descrição
composer search mailgun
```

[Packagist.org](https://packagist.org/) é o repositório principal para pacotes Composer. Pesquise aqui por pacotes existentes.

## `composer.json` vs `composer.lock`

O arquivo `composer.json` armazena as preferências de de versão de cada dependência, além de outras informações

O arquivo `composer.lock` armazena exatamente qual versão foi baixada para cada dependência. Nunca altere este arquivo.

Se você incluir o arquivo `composer.lock` no seu repositório git, todos os desenvolvedores irão instalar a mesma versão das dependências que você.
Mesmo se uma nova versão for lançada, o Composer baixará apenas a versão salva no arquivo de lock.

```sh
# Atualizar todas as dependências para a versão mais recente (ainda dentro das preferências definidas)
composer update

# Para atualizar a versão de uma dependência específica:
composer update phpunit/phpunit

# Para migrar um pacote para uma nova preferência de versão, você pode precisar
# remover o pacote antigo e suas dependências primeiro
composer remove --dev phpunit/phpunit
composer require --dev phpunit/phpunit:^5.0

```

## Autoloader

O Composer cria uma classe autoloader que você pode usar na sua aplicação.
Você pode instanciar as classes pelos seus namespaces.

```php
require __DIR__ . '/vendor/autoload.php';

$mailgun = new Mailgun\Mailgun("key");
```

### Autoloader da PSR-4

Você pode adicionar seus próprios namespaces ao autoloader.

No `composer.json`, adicione um campo 'autoload':

```json
{
  "autoload": {
    "psr-4": {"Acme\\": "src/"}
  }
}
```
Isso irá dizer ao autoloader para buscar na pasta `src` tudo o que estiver no namespace `\Acme\`.

Você também pode [usar a PSR-0, um mapa de classes ou apenas listar os arquivos para incluir](https://getcomposer.org/doc/04-schema.md#autoload),
e pode usar o campo `autoload-dev` para namespaces de desenvolvimento.

Ao adicionar ou alterar alguma chave de autoload, você precisará recriar o autoloader

```sh
composer dump-autoload
composer dump # shorthand

# Otimiza pacotes PSR-0 e PSR-4 para carregar com mapas de classes também.
# É mais demorado, mas melhora a performance em produção.
composer dump-autoload --optimize --no-dev
```

# O cache do Composer

```sh
# O Composer irá evitar baixar pacotes caso eles estejam no cache. Para limpar o cache:
composer clear-cache
```

# Resolução de problemas

```sh
composer diagnose
composer self-update
composer clear-cache
```

## Tópicos (ainda) não falados neste tutorial

* Criando e distribuindo seus próprios pacotes no Packagist.org ou qualquer lugar
* Hooks Pré- e Pós-: rodar tarefas específicas em determinados eventos do Composer

### Referências

* [Composer - O gerenciador de dependências do PHP](https://getcomposer.org/) (EN)
* [Packagist.org](https://packagist.org/) (EN)
