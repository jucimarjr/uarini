STRATEGY
======

Aplicação de uma única interface que pode ser usada de várias maneiras.
Definida uma interface operacao que foi implementada nas classes somar e subtrair.
A classe operar são atribuidos dois operados uma operacao, sendo que esta operacao pode ser de somar ou subtrair.
Ao chamar o método de calcular da classe operar, é realizada a operação devida segundo como ela foi implementada.

É um design pattern voltado para encapsulamento de algoritmos que podem variar para prover um comportamento mais adequado para um objeto frente a uma situação.
No caso acima, temos uma interface operacao que pode tem encapsulada sua implmentação as classes somar e subtrair.
Dependeno do comportamento que foi determinado para operar ao se instaciar a sua operacao, esta pode se comportar como a classe somar ou como a classe subtrair.
