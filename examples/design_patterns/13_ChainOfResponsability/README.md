CHAIN OF RESPONSABILITY
======

O padrão de projeto de software Chain of Responsibility representa um encadeamento de objetos receptores para o processamento de uma série de solicitações diferentes. Esses objetos receptores passam a solicitação ao longo da cadeia até que um ou vários objetos a tratem.
Cada objeto receptor possui uma lógica descrevendo os tipos de solicitação que é capaz de processar e como passar adiante aquelas que requeiram processamento por outros receptores. A delegação das solicitações pode formar uma árvore de recursão, com um mecanismo especial para inserção de novos receptores no final da cadeia existente.
Dessa forma, fornece um acoplamento mais fraco por evitar a associação explícita do remetente de uma solicitação ao seu receptor e dar a mais de um objeto a oportunidade de tratar a solicitação.
Um exemplo da aplicação desse padrão é o mecanismo de herança nas linguagens orientadas a objeto: um método chamado em um objeto é buscado na classe que implementa o objeto e, se não encontrado, na superclasse dessa classe, de maneira recursiva.
