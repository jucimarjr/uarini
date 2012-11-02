FACTORY METHOD
======

Factory Method é também conhecido como Virtual Constructor


Quando temos uma hierarquia de classes definidas, como a hierarquia de Commands, fatalmente temos a necessidade de criar as sub-classes conforme um determinado comando foi acionado.

Para o aplicativo usuário desta família de classes, o ideal seria ter vínculo apenas com a classe BaseCommand e não com suas demais classes, com isso poderíamos pensar que a criação de um novo comando não acarretaria na alteração do aplicativo em si, facilitando a extensibilidade do modelo e aplicativo. Factory Method é um método responsável por encapsular esta lógica de criação condicional.
