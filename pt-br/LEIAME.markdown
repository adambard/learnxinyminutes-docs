# [Aprenda X em Y minutos](http://learnxinyminutes.com)

Um passeio por um turbilhão das (várias, espero um dia) populares e
cada vez mais populares linguagens de programação, apresentadas de forma
válida, comentada e explicadas durante o uso.

## Nós precisamos de VOCÊ!...

... para escrever mais tutoriais inline de programação. Apenas pegue um arquivo 
existente deste repositório e copie o formato. (não se preocupe, é muito simples).
Crie um novo arquivo, envie um pull request, e se ele passar pela inspeção eu 
aceito na hora.
Lembre-se de adicionar o campo contribuidores para que receba os devidos créditos!

## Contribuindo

Todos as contribuições são bem vindas, das menores aos novos artigos. Traduções em 
todos os idiomas são bem vindas (ou, se for o caso, artigos originais em qualquer 
idioma). Mande um pull request ou abra uma issue a qualquer hora do dia ou da noite.

**Por favor etiquete seus pull requests com [idioma/linguagem-de-programação] no começo**
**(e.g. [python/pt-br] para Python em português do Brasil).** Isso ajudará qualquer um a 
escolher as coisas que mais gosta.

Nós ficamos felizes com qualquer contribuição e de qualquer forma, mas se você fizer 
mais que uma grande modificação (i.e. traduzindo para dois idiomas diferentes) seria 
muito legal que você fizesse dois pull requests separados, um para cada idioma. 
Desta forma, alguém pode revisá-los de forma mais efetiva e/ou individualmente.

### Linhas guia de estilos

* **Mantenha as linhas com menos de 80 caracteres**
* **Prefira exemplos que exposição**
* **Evite redundância**
* **Use UTF-8**

Versão longa:

* Tente manter **o tamanho das linhas nos blocos de código com 80 caracteres ou menos**, ou elas 
ultrapassarão o limite e ficará estranho.

* Tente usar tão poucas palavras quanto possível. Prefira sempre exemplos de códigos que exposição
em todos os casos.

* Novatos são bemvindos, mas a audiência alvo deste site são programadores com alguma experiência.
  Então, evite explicar conceitos básicos exceto para aqueles específicos da linguagem em questão,
  para manter os artigos sucintos e mapeáveis. Todos sabemos como usar o Google aqui.

* Para traduções (ou artigos em inglês com caracteres que não são ASCII), por favor se certifique
  que seu arquivo está em UTF-8, e procure deixar de fora o byte-order-mark no início do arquivo. 
  (`:set nobomb` no Vim)

### Configuração do cabeçalho

O site atual usa Middleman para gerar o arquivo HTML a partir destes arquivos Markdown. O Middleman,
ou ao menos o script padrão que cria a base do site, requer que algumas informações chave sejam
definidas no cabeçalho.

Os campos a seguir são necessários para artigos em Inglês sobre linguagens de programação:

* **language** A *linguagem de programação* em questão
* **contributors** Uma lista de [autor, URL] listas de créditos

Outros campos:

* **filename**: O nome do arquivo para o código deste artigo. Ele será encontrado, colocado junto 
e disponibilizado para download.
  Para artigos em outros idiomas (não-inglês), *nomedoarquivo* deve ter um sufixo com a linguagem específica.
* **lang**: Para traduções, a linguagem humana em que este artigo está. Principalmente para categorização.

Aqui está um exemplo de cabeçalho para uma tradução de Ruby em Esperanto:

```yaml
---
language: ruby
filename: learnruby-epo.ruby
contributors:
    - ["Doktor Esperanto", "http://example.com/"]
    - ["Someone else", "http://someoneelseswebsite.com/"]
lang: ep-ep
---
```

## Licença

Contribuidores retém os direitos autorais sobre sey trabalho, e podem solicitar a remoção a qualquer momento.
Fazendo o upload de um documento aqui, você concorda em poblicar seu trabalho sob o padrão
[Creative Commons Attribution-ShareAlike 3.0 Unported](http://creativecommons.org/licenses/by-sa/3.0/deed.en_US)
licenciamento incluído em cada página do documento.

Qualquer coisa não coberta pelo exposto acima -- basicamente, este arquivo LEIAME -- você pode usar como quiser, eu acho.
