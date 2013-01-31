COMMAND
======

Este design pattern é bastante aplicado em diversos frameworks de interfaces gráficas e interfaces Web.
Aplicamos o Command para encapsular ações que geralmente estão associadas a um objeto de interface gráfica,
como por exemplo um botão. O fato é que o construtor da biblioteca do componente botão não tem como
conhecer qual ação que aquele botão deverá executar, isso vai variar de acordo com a necessidade da tela, e
cabe aos usuários daquela biblioteca programar uma classe que vai encapsular o código que deverá ser
executado para cada botão. Command é um design pattern para se desenvolver este tipo classe.

Foi construida uma classe geral Operar que é uma generalização de Somar e Subtair.

O botão, que no caso, seriam os objetos somar e subtrair apenas executam o commando(Command) e internamente deve ser chamada o método interno a ser executada.
Neste caso é realizada a soma e a subtração apenas.
