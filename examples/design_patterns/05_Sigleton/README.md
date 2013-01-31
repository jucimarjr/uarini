SINGLETON
======

Singleton é um design pattern de criação cujo objetivo é fazer que a classe tecnicamente ofereça apenas uma instância de objeto, que será controlada por ela mesma. Ao aplicarmos o Singleton nas situações corretas, temos como conseqüência um número menor de objetos de “mortalidade infantil”, pois a classe disponibilizará apenas acesso a um único objeto.
No caso do exemplo proposto, ConfigManager disponibiliza apenas um objeto de sua classe, tendo o seu construtor sendo private, impedindo que mais instancias da classe ConfigManager sejam instanciadas.
