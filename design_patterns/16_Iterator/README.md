ITERATOR
=====

Conhecido também como Cursor
Quando temos agregações e composições de objetos, devemos disponibilizar para o cliente uma maneira de percorrer tal estrutura sem necessariamente conhecer o objeto internamente. Iterator é um design pattern que propõe a centralização da regra de navegação em objetos em uma classe, de forma que tal regra possa variar e ele não tenha que expor excessivamente sua estrutura interna de correlacionamento com os objetos que serão percorridos.
