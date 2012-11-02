DECORATOR
======

Aplicação de decorator para decorar uma classe.
Inicialmente é definida uma classe abstrata Animal que tem somente a implementação do construtor e dos atributos (nome e idade).
São definidas duas classes que herdam de Animal: Dog e Cat. Ambas implementam obrigatoriamente o método falar da classe Animal.
Em seguida é definida a classe Pet que pode ser umas das duas classes para decorar (Dog ou Cat).
Na classe Decorator define-se inicialmente um objeto dog e outro objeto cat.
Em seguida são definidos os objetos pet1 e pet2, onde pet1 deve decorar dog e pet2 deve decorar cat.
Assim ao executar o método falar de pet1, será como o de dog e ao executar o método de pet2, será como o de cat.
